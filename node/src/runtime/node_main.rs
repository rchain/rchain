//! Node main entry points (port of `runtime/NodeMain.scala`).
//!
//! `run_cli` mirrors `NodeMain.runCLI` (the thin-client CLI); the `run` (start-node) path is wired
//! separately in `crate::main`.

use std::path::Path;

use rchain_casper::protocol::client::{DeployRuntime, GrpcDeployService, GrpcProposeService, Name};
use rchain_crypto::private_key::PrivateKey;
use rchain_crypto::signatures::secp256k1::Secp256k1;
use rchain_crypto::signatures::signatures_alg::{from_algorithm, SignaturesAlg};
use rchain_crypto::util::key_util::write_keys;

use crate::configuration::commandline::options::{Commands, Options};
use crate::effects::{ConsoleIo, GrpcReplClient, RustylineConsole, StdioConsole};
use crate::runtime::repl_runtime::ReplRuntime;

/// The internal gRPC port used by the repl/propose clients by default (port of `Options.GrpcInternalPort`).
const GRPC_INTERNAL_PORT: i32 = 40402;
/// The external gRPC port used by the deploy client by default (port of `Options.GrpcExternalPort`).
const GRPC_EXTERNAL_PORT: i32 = 40401;

const VALIDATOR_PASSWORD_ENV_VAR: &str = "RNODE_VALIDATOR_PASSWORD";

/// Execute a CLI command against a remote node (port of `NodeMain.runCLI`).
pub async fn run_cli(options: &Options) -> Result<(), Vec<String>> {
    let host = options.grpc_host.clone();
    let max_size = options.grpc_max_recv_message_size;

    match &options.subcommand {
        Commands::Run(_) => Err(vec!["`run` is handled by the node, not the CLI client".to_string()]),

        Commands::Keygen { location } => {
            let mut console = StdioConsole;
            generate_key(&mut console, location)
        }

        Commands::Repl => {
            let port = options.grpc_port.unwrap_or(GRPC_INTERNAL_PORT);
            let client = GrpcReplClient::connect(&host, port, max_size)
                .await
                .map_err(|e| vec![e])?;
            tokio::task::spawn_blocking(move || {
                let mut console = match RustylineConsole::new() {
                    Ok(c) => c,
                    Err(e) => {
                        eprintln!("Failed to initialize the REPL console: {e}");
                        return;
                    }
                };
                ReplRuntime.repl_program(&mut console, &client);
            })
            .await
            .map_err(|e| vec![e.to_string()])
        }

        Commands::Eval {
            file_names,
            print_unmatched_sends_only,
        } => {
            let port = options.grpc_port.unwrap_or(GRPC_INTERNAL_PORT);
            let client = GrpcReplClient::connect(&host, port, max_size)
                .await
                .map_err(|e| vec![e])?;
            let files = file_names.clone();
            let unmatched = *print_unmatched_sends_only;
            tokio::task::spawn_blocking(move || {
                let mut console = StdioConsole;
                ReplRuntime.eval_program(&mut console, &client, &files, unmatched);
            })
            .await
            .map_err(|e| vec![e.to_string()])
        }

        // The remaining subcommands talk to the deploy service (and the propose service for
        // `propose`).
        _ => {
            let external_port = options.grpc_port.unwrap_or(GRPC_EXTERNAL_PORT);
            let internal_port = options.grpc_port.unwrap_or(GRPC_INTERNAL_PORT);
            let deploy = GrpcDeployService::connect(&host, external_port, max_size)
                .await
                .map_err(|e| vec![e])?;

            match &options.subcommand {
                Commands::Propose {
                    print_unmatched_sends,
                } => {
                    let propose = GrpcProposeService::connect(&host, internal_port, max_size)
                        .await
                        .map_err(|e| vec![e])?;
                    DeployRuntime::propose(&propose, *print_unmatched_sends).await
                }
                Commands::Deploy {
                    phlo_limit,
                    phlo_price,
                    valid_after_block_number,
                    private_key,
                    private_key_path,
                    location,
                    shard_id,
                } => {
                    let mut console = StdioConsole;
                    let private_key = resolve_private_key(
                        private_key.as_deref(),
                        private_key_path.as_deref(),
                        &mut console,
                    )?;
                    DeployRuntime::deploy_file_program(
                        &deploy,
                        *phlo_limit,
                        *phlo_price,
                        valid_after_block_number.unwrap_or(-1),
                        &private_key,
                        location,
                        shard_id,
                    )
                    .await
                }
                Commands::DeployStatus { deploy_signature } => {
                    DeployRuntime::deploy_status(&deploy, deploy_signature).await
                }
                Commands::FindDeploy { deploy_id } => {
                    DeployRuntime::find_deploy(&deploy, deploy_id).await
                }
                Commands::ShowBlock { hash } => DeployRuntime::get_block(&deploy, hash).await,
                Commands::ShowBlocks { depth } => {
                    DeployRuntime::get_blocks(&deploy, depth.unwrap_or(1)).await
                }
                Commands::Vdag {
                    depth,
                    show_justification_lines,
                } => {
                    DeployRuntime::visualize_dag(&deploy, depth.unwrap_or(-1), *show_justification_lines)
                        .await
                }
                Commands::Mvdag => DeployRuntime::machine_verifiable_dag(&deploy).await,
                Commands::ListenDataAtName {
                    type_of_name,
                    content,
                } => {
                    let name = build_single_name(type_of_name, content)?;
                    DeployRuntime::listen_for_data_at_name(&deploy, &name).await
                }
                Commands::ListenContAtName {
                    type_of_name,
                    content,
                } => {
                    let names = build_names(type_of_name, content)?;
                    DeployRuntime::listen_for_continuation_at_name(&deploy, &names).await
                }
                Commands::LastFinalizedBlock => DeployRuntime::last_finalized_block(&deploy).await,
                Commands::IsFinalized { hash } => DeployRuntime::is_finalized(&deploy, hash).await,
                Commands::BondStatus {
                    validator_public_key,
                } => DeployRuntime::bond_status(&deploy, validator_public_key).await,
                Commands::Status => DeployRuntime::status(&deploy).await,
                _ => Err(vec!["unexpected subcommand for deploy service".to_string()]),
            }
        }
    }
}

/// Build a single listen-at-name `Name` from the `type`/`content` CLI args (port of the `SINGLE`
/// name converter in `Options.nameProviderConverter`).
fn build_single_name(type_of_name: &str, content: &[String]) -> Result<Name, Vec<String>> {
    let first = content.first().cloned().unwrap_or_default();
    match type_of_name {
        "priv" => Ok(Name::PrivName(first)),
        "pub" => Ok(Name::PubName(first)),
        _ => Err(vec!["Bad option value. Use \"pub\" or \"priv\"".to_string()]),
    }
}

/// Build a list of listen-at-name `Name`s (port of the `LIST` name converter).
fn build_names(type_of_name: &str, content: &[String]) -> Result<Vec<Name>, Vec<String>> {
    match type_of_name {
        "priv" => Ok(content.iter().map(|c| Name::PrivName(c.clone())).collect()),
        "pub" => Ok(content.iter().map(|c| Name::PubName(c.clone())).collect()),
        _ => Err(vec!["Bad option value. Use \"pub\" or \"priv\"".to_string()]),
    }
}

/// Resolve the deploy private key from `--private-key` (hex) or `--private-key-path` (PEM) (port of
/// `NodeMain.runCLI`'s `getPrivateKey`).
fn resolve_private_key(
    private_key: Option<&[u8]>,
    private_key_path: Option<&Path>,
    console: &mut dyn ConsoleIo,
) -> Result<PrivateKey, Vec<String>> {
    if let Some(bytes) = private_key {
        Ok(PrivateKey::new(bytes.to_vec()))
    } else if let Some(path) = private_key_path {
        decrypt_key_from_pem_file(console, path)
    } else {
        Err(vec!["Private key is missing".to_string()])
    }
}

/// Decrypt a PEM private key (port of `NodeMain.decryptKeyFromPemFile`).
fn decrypt_key_from_pem_file(
    console: &mut dyn ConsoleIo,
    path: &Path,
) -> Result<PrivateKey, Vec<String>> {
    let password = get_validator_password(console);
    Secp256k1::parse_pem_file(path, &password).map_err(|e| vec![e])
}

/// Read the validator password from the environment, falling back to the console (port of
/// `NodeMain.getValidatorPassword`).
fn get_validator_password(console: &mut dyn ConsoleIo) -> String {
    match std::env::var(VALIDATOR_PASSWORD_ENV_VAR) {
        Ok(password) if !password.is_empty() => password,
        _ => console.read_password(
            "Variable RNODE_VALIDATOR_PASSWORD is not set, please enter password for keyfile. \nPassword for keyfile: ",
        ),
    }
}

/// Generate a validator key pair and write it to `path` (port of `NodeMain.generateKey`).
fn generate_key(console: &mut dyn ConsoleIo, path: &Path) -> Result<(), Vec<String>> {
    let password = console.read_password("Enter password for keyfile: ");
    let password_repeat = console.read_password("Repeat password: ");
    if password != password_repeat {
        console.println("Passwords do not match. Try again:");
        return generate_key(console, path);
    }
    if password.is_empty() {
        console.println("Password is empty. Try again:");
        return generate_key(console, path);
    }

    let sig_algorithm =
        from_algorithm(Secp256k1.name()).ok_or_else(|| vec!["Invalid algorithm name".to_string()])?;
    let (sk, pk) = sig_algorithm.new_key_pair();
    let private_path = path.join("rnode.key");
    let public_path = path.join("rnode.pub.pem");
    let hex_path = path.join("rnode.pub.hex");

    write_keys(
        &sk,
        &pk,
        sig_algorithm,
        &password,
        &private_path,
        &public_path,
        &hex_path,
    )
    .map_err(|e| vec![e])?;

    console.println(&format!(
        "\nSuccess!\nPrivate key file (encrypted PEM format):  {}\nPublic  key file (PEM format):            {}\nPublic  key file (HEX format):            {}",
        private_path.display(),
        public_path.display(),
        hex_path.display()
    ));
    Ok(())
}
