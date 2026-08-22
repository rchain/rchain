//! The node entry point (port of `Main.scala` + `NodeMain.startNode`).

use std::sync::Arc;

use clap::Parser;

use rchain_node::configuration::commandline::options::{Commands, Options};
use rchain_node::configuration::configuration::Configuration;
use rchain_node::runtime::{node_environment, node_runtime, run_cli};
use rchain_shared::log::StderrLog;

fn main() {
    // The rholang parser + reducer recurse through deeply-nested contract sources (the genesis
    // blessed terms in particular); a 2 MiB worker stack overflows. Give the runtime a larger
    // per-worker stack (the JVM node runs these paths on a much larger native stack).
    let runtime = match tokio::runtime::Builder::new_multi_thread()
        .thread_stack_size(32 * 1024 * 1024)
        .enable_all()
        .build()
    {
        Ok(rt) => rt,
        Err(e) => {
            eprintln!("Failed to build the tokio runtime: {e}");
            std::process::exit(1);
        }
    };
    runtime.block_on(async_main());
}

async fn async_main() {
    let options = Options::parse();

    // Execute a thin-client CLI command (port of `Main.main`'s `runCLI` branch).
    if !matches!(options.subcommand, Commands::Run(_)) {
        if let Err(errors) = run_cli(&options).await {
            for error in &errors {
                println!("{error}");
            }
            std::process::exit(1);
        }
        return;
    }

    let (node_conf, _profile, _config_file) = match Configuration::build(&options) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Configuration error: {e}");
            std::process::exit(1);
        }
    };

    let id = match node_environment::create(&node_conf) {
        Ok(id) => id,
        Err(e) => {
            eprintln!("Initialization error: {e}");
            std::process::exit(1);
        }
    };

    let program = match node_runtime::setup_node_program(&node_conf, &id, Arc::new(StderrLog)).await {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Setup error: {e}");
            std::process::exit(1);
        }
    };

    if let Err(e) = program.serve().await {
        eprintln!("Server error: {e}");
        std::process::exit(1);
    }
}
