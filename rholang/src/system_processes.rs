//! Built-in system contracts (port of `interpreter/SystemProcesses.scala`).
//!
//! `FixedChannels`/`BodyRefs` are the byte channels and dispatch-table ids; [`SystemProcesses`]
//! builds the `ScalaBodyFn` handlers (stdout/stderr, crypto verify/hash, block data, REV address,
//! deployer-id ops, registry ops, sys-auth-token ops) that the runtime installs and dispatches to.

use std::sync::{Arc, Mutex};

use rchain_crypto::hash::{blake2b256, keccak256, sha256};
use rchain_crypto::public_key::PublicKey;
use rchain_crypto::signatures::ed25519::Ed25519;
use rchain_crypto::signatures::secp256k1::Secp256k1;
use rchain_models::ast::Par;
use rchain_models::casper::protocol::casper_message::BlockMessage;
use rchain_shared::refined::{BlockHeight, NonNegI64, SeqNum};
use rchain_models::rholang::RhoType::{
    RhoBoolean, RhoByteArray, RhoDeployerId, RhoList, RhoMap, RhoName, RhoNil, RhoNumber, RhoSet,
    RhoString, RhoSysAuthToken, RhoTupleN, RhoUri,
};
use rchain_models::runtime::ListParWithRandom;
use rchain_models::validator::Validator;

use crate::contract_call::ContractCall;
use crate::dispatch::{RholangAndScalaDispatcher, ScalaBodyFn};
use crate::errors::RholangError;
use crate::native_state::NativeSystemState;
use crate::pretty_printer::PrettyPrinter;
use crate::registry;
use crate::storage::ChargingRSpace;
use crate::util::rev_address::RevAddress;

/// A byte-name channel (port of `SystemProcesses.byteName`): `GPrivate(<single byte>)`.
pub fn byte_name(b: u8) -> Par {
    RhoName::apply_bytes(vec![b])
}

/// The fixed system channels (port of `SystemProcesses.FixedChannels`).
pub struct FixedChannels;
impl FixedChannels {
    pub fn stdout() -> Par {
        byte_name(0)
    }
    pub fn stdout_ack() -> Par {
        byte_name(1)
    }
    pub fn stderr() -> Par {
        byte_name(2)
    }
    pub fn stderr_ack() -> Par {
        byte_name(3)
    }
    pub fn ed25519_verify() -> Par {
        byte_name(4)
    }
    pub fn sha256_hash() -> Par {
        byte_name(5)
    }
    pub fn keccak256_hash() -> Par {
        byte_name(6)
    }
    pub fn blake2b256_hash() -> Par {
        byte_name(7)
    }
    pub fn secp256k1_verify() -> Par {
        byte_name(8)
    }
    pub fn get_block_data() -> Par {
        byte_name(10)
    }
    pub fn get_invalid_blocks() -> Par {
        byte_name(11)
    }
    pub fn rev_address() -> Par {
        byte_name(12)
    }
    pub fn deployer_id_ops() -> Par {
        byte_name(13)
    }
    pub fn reg_lookup() -> Par {
        byte_name(14)
    }
    pub fn reg_insert_random() -> Par {
        byte_name(15)
    }
    pub fn reg_insert_signed() -> Par {
        byte_name(16)
    }
    pub fn reg_ops() -> Par {
        byte_name(17)
    }
    pub fn sys_auth_token_ops() -> Par {
        byte_name(18)
    }
    pub fn pos() -> Par {
        byte_name(19)
    }
    pub fn rev_vault() -> Par {
        byte_name(20)
    }
    pub fn multi_sig_rev_vault() -> Par {
        byte_name(21)
    }
}

/// The dispatch-table ids (port of `SystemProcesses.BodyRefs`).
pub struct BodyRefs;
impl BodyRefs {
    pub const STDOUT: i64 = 0;
    pub const STDOUT_ACK: i64 = 1;
    pub const STDERR: i64 = 2;
    pub const STDERR_ACK: i64 = 3;
    pub const ED25519_VERIFY: i64 = 4;
    pub const SHA256_HASH: i64 = 5;
    pub const KECCAK256_HASH: i64 = 6;
    pub const BLAKE2B256_HASH: i64 = 7;
    pub const SECP256K1_VERIFY: i64 = 9;
    pub const GET_BLOCK_DATA: i64 = 11;
    pub const GET_INVALID_BLOCKS: i64 = 12;
    pub const REV_ADDRESS: i64 = 13;
    pub const DEPLOYER_ID_OPS: i64 = 14;
    pub const REG_OPS: i64 = 15;
    pub const SYS_AUTHTOKEN_OPS: i64 = 16;
    pub const REG_LOOKUP: i64 = 17;
    pub const REG_INSERT_RANDOM: i64 = 18;
    pub const REG_INSERT_SIGNED: i64 = 19;
    pub const POS: i64 = 20;
    pub const REV_VAULT: i64 = 21;
    pub const MULTI_SIG_REV_VAULT: i64 = 22;
}

/// Per-block data exposed to the `rho:block:data` contract (port of `SystemProcesses.BlockData`).
#[derive(Clone, Debug)]
pub struct BlockData {
    pub block_number: BlockHeight,
    pub sender: PublicKey,
    pub seq_num: SeqNum,
}

impl BlockData {
    pub fn empty() -> Self {
        BlockData {
            block_number: BlockHeight::zero(),
            sender: PublicKey::new(vec![0]),
            seq_num: SeqNum::zero(),
        }
    }

    /// Build the per-block data from a block message (port of `BlockData.fromBlock`).
    pub fn from_block(block: &BlockMessage) -> Self {
        BlockData {
            block_number: block.block_number,
            sender: PublicKey::new(block.sender.as_bytes().to_vec()),
            seq_num: block.seq_num,
        }
    }
}

/// A system-contract definition: urn + fixed channel + arity + dispatch id + handler.
pub struct Definition {
    pub urn: String,
    pub fixed_channel: Par,
    pub arity: i32,
    /// Whether the last argument is a remainder (variable-arity method dispatch).
    pub remainder: bool,
    pub body_ref: i64,
    pub handler: ScalaBodyFn,
}

fn illegal_arg(msg: &str) -> RholangError {
    RholangError::ReduceError(msg.to_string())
}

/// The system-process context (port of `SystemProcesses[F]`).
pub struct SystemProcesses {
    contract_call: ContractCall<ChargingRSpace, Arc<RholangAndScalaDispatcher>>,
    pretty_printer: PrettyPrinter,
    block_data: Arc<Mutex<BlockData>>,
    native_state: Arc<NativeSystemState>,
}

impl SystemProcesses {
    pub fn new(
        space: ChargingRSpace,
        dispatcher: Arc<RholangAndScalaDispatcher>,
        block_data: Arc<Mutex<BlockData>>,
        native_state: Arc<NativeSystemState>,
    ) -> Self {
        SystemProcesses {
            contract_call: ContractCall::new(space, dispatcher),
            pretty_printer: PrettyPrinter::new(),
            block_data,
            native_state,
        }
    }

    /// The ordered list of standard system contracts (port of `stdSystemProcesses` +
    /// `stdRhoCryptoProcesses`).
    pub fn definitions(&self) -> Vec<Definition> {
        vec![
            Definition {
                urn: "rho:io:stdout".to_string(),
                fixed_channel: FixedChannels::stdout(),
                arity: 1,
                remainder: false,
                body_ref: BodyRefs::STDOUT,
                handler: self.stdout(),
            },
            Definition {
                urn: "rho:io:stdoutAck".to_string(),
                fixed_channel: FixedChannels::stdout_ack(),
                arity: 2,
                remainder: false,
                body_ref: BodyRefs::STDOUT_ACK,
                handler: self.stdout_ack(),
            },
            Definition {
                urn: "rho:io:stderr".to_string(),
                fixed_channel: FixedChannels::stderr(),
                arity: 1,
                remainder: false,
                body_ref: BodyRefs::STDERR,
                handler: self.stderr(),
            },
            Definition {
                urn: "rho:io:stderrAck".to_string(),
                fixed_channel: FixedChannels::stderr_ack(),
                arity: 2,
                remainder: false,
                body_ref: BodyRefs::STDERR_ACK,
                handler: self.stderr_ack(),
            },
            Definition {
                urn: "rho:block:data".to_string(),
                fixed_channel: FixedChannels::get_block_data(),
                arity: 1,
                remainder: false,
                body_ref: BodyRefs::GET_BLOCK_DATA,
                handler: self.get_block_data(),
            },
            Definition {
                urn: "rho:rev:address".to_string(),
                fixed_channel: FixedChannels::rev_address(),
                arity: 3,
                remainder: false,
                body_ref: BodyRefs::REV_ADDRESS,
                handler: self.rev_address(),
            },
            Definition {
                urn: "rho:rchain:deployerId:ops".to_string(),
                fixed_channel: FixedChannels::deployer_id_ops(),
                arity: 3,
                remainder: false,
                body_ref: BodyRefs::DEPLOYER_ID_OPS,
                handler: self.deployer_id_ops(),
            },
            Definition {
                urn: "rho:registry:ops".to_string(),
                fixed_channel: FixedChannels::reg_ops(),
                arity: 3,
                remainder: false,
                body_ref: BodyRefs::REG_OPS,
                handler: self.registry_ops(),
            },
            Definition {
                urn: "sys:authToken:ops".to_string(),
                fixed_channel: FixedChannels::sys_auth_token_ops(),
                arity: 3,
                remainder: false,
                body_ref: BodyRefs::SYS_AUTHTOKEN_OPS,
                handler: self.sys_auth_token_ops(),
            },
            Definition {
                urn: "rho:registry:lookup".to_string(),
                fixed_channel: FixedChannels::reg_lookup(),
                arity: 2,
                remainder: false,
                body_ref: BodyRefs::REG_LOOKUP,
                handler: self.registry_lookup(),
            },
            Definition {
                urn: "rho:registry:insertArbitrary".to_string(),
                fixed_channel: FixedChannels::reg_insert_random(),
                arity: 2,
                remainder: false,
                body_ref: BodyRefs::REG_INSERT_RANDOM,
                handler: self.registry_insert_arbitrary(),
            },
            Definition {
                urn: "rho:registry:insertSigned:secp256k1".to_string(),
                fixed_channel: FixedChannels::reg_insert_signed(),
                arity: 3,
                remainder: false,
                body_ref: BodyRefs::REG_INSERT_SIGNED,
                handler: self.registry_insert_signed(),
            },
            Definition {
                urn: "rho:rchain:pos".to_string(),
                fixed_channel: FixedChannels::pos(),
                arity: 1,
                remainder: true,
                body_ref: BodyRefs::POS,
                handler: self.pos(),
            },
            Definition {
                urn: "rho:rchain:revVault".to_string(),
                fixed_channel: FixedChannels::rev_vault(),
                arity: 1,
                remainder: true,
                body_ref: BodyRefs::REV_VAULT,
                handler: self.rev_vault(),
            },
            Definition {
                urn: "rho:rchain:multiSigRevVault".to_string(),
                fixed_channel: FixedChannels::multi_sig_rev_vault(),
                arity: 1,
                remainder: true,
                body_ref: BodyRefs::MULTI_SIG_REV_VAULT,
                handler: self.rev_vault(),
            },
            Definition {
                urn: "rho:crypto:secp256k1Verify".to_string(),
                fixed_channel: FixedChannels::secp256k1_verify(),
                arity: 4,
                remainder: false,
                body_ref: BodyRefs::SECP256K1_VERIFY,
                handler: self.secp256k1_verify(),
            },
            Definition {
                urn: "rho:crypto:blake2b256Hash".to_string(),
                fixed_channel: FixedChannels::blake2b256_hash(),
                arity: 2,
                remainder: false,
                body_ref: BodyRefs::BLAKE2B256_HASH,
                handler: self.blake2b256_hash(),
            },
            Definition {
                urn: "rho:crypto:keccak256Hash".to_string(),
                fixed_channel: FixedChannels::keccak256_hash(),
                arity: 2,
                remainder: false,
                body_ref: BodyRefs::KECCAK256_HASH,
                handler: self.keccak256_hash(),
            },
            Definition {
                urn: "rho:crypto:sha256Hash".to_string(),
                fixed_channel: FixedChannels::sha256_hash(),
                arity: 2,
                remainder: false,
                body_ref: BodyRefs::SHA256_HASH,
                handler: self.sha256_hash(),
            },
            Definition {
                urn: "rho:crypto:ed25519Verify".to_string(),
                fixed_channel: FixedChannels::ed25519_verify(),
                arity: 4,
                remainder: false,
                body_ref: BodyRefs::ED25519_VERIFY,
                handler: self.ed25519_verify(),
            },
        ]
    }

    // --- io ------------------------------------------------------------

    fn stdout(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let pp = self.pretty_printer.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let pp = pp.clone();
            Box::pin(async move {
                let (pars, _) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("stdout expects one argument"))?;
                match pars.as_slice() {
                    [arg] => {
                        println!("{}", pp.build_string(arg));
                        Ok(())
                    }
                    _ => Err(illegal_arg("stdout expects one argument")),
                }
            })
        })
    }

    fn stdout_ack(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let pp = self.pretty_printer.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let pp = pp.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("stdoutAck expects two arguments"))?;
                match pars.as_slice() {
                    [arg, ack] => {
                        println!("{}", pp.build_string(arg));
                        cc.produce(&rand, &[Par::default()], ack).await
                    }
                    _ => Err(illegal_arg("stdoutAck expects two arguments")),
                }
            })
        })
    }

    fn stderr(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let pp = self.pretty_printer.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let pp = pp.clone();
            Box::pin(async move {
                let (pars, _) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("stderr expects one argument"))?;
                match pars.as_slice() {
                    [arg] => {
                        eprintln!("{}", pp.build_string(arg));
                        Ok(())
                    }
                    _ => Err(illegal_arg("stderr expects one argument")),
                }
            })
        })
    }

    fn stderr_ack(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let pp = self.pretty_printer.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let pp = pp.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("stderrAck expects two arguments"))?;
                match pars.as_slice() {
                    [arg, ack] => {
                        eprintln!("{}", pp.build_string(arg));
                        cc.produce(&rand, &[Par::default()], ack).await
                    }
                    _ => Err(illegal_arg("stderrAck expects two arguments")),
                }
            })
        })
    }

    // --- crypto --------------------------------------------------------

    fn verify_signature_contract(
        &self,
        name: &'static str,
        algorithm: fn(&[u8], &[u8], &[u8]) -> bool,
    ) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            Box::pin(async move {
                let (pars, rand) = cc.unapply(&args).ok_or_else(|| {
                    illegal_arg(&format!(
                        "{name} expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
                    ))
                })?;
                match pars.as_slice() {
                    [data, signature, pub_key, ack] => {
                        let (Some(d), Some(s), Some(p)) = (
                            RhoByteArray::unapply(data),
                            RhoByteArray::unapply(signature),
                            RhoByteArray::unapply(pub_key),
                        ) else {
                            return Err(illegal_arg(&format!(
                                "{name} expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
                            )));
                        };
                        let verified = algorithm(d, s, p);
                        cc.produce(&rand, &[RhoBoolean::apply(verified)], ack).await
                    }
                    _ => Err(illegal_arg(&format!(
                        "{name} expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
                    ))),
                }
            })
        })
    }

    fn hash_contract(&self, name: &'static str, algorithm: fn(&[u8]) -> Vec<u8>) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg(&format!("{name} expects a byte array and return channel")))?;
                match pars.as_slice() {
                    [input, ack] => match RhoByteArray::unapply(input) {
                        Some(bytes) => {
                            let hash = algorithm(bytes);
                            cc.produce(&rand, &[RhoByteArray::apply(hash)], ack).await
                        }
                        None => Err(illegal_arg(&format!("{name} expects a byte array and return channel"))),
                    },
                    _ => Err(illegal_arg(&format!("{name} expects a byte array and return channel"))),
                }
            })
        })
    }

    fn secp256k1_verify(&self) -> ScalaBodyFn {
        self.verify_signature_contract("secp256k1Verify", Secp256k1::verify_bytes)
    }

    fn ed25519_verify(&self) -> ScalaBodyFn {
        self.verify_signature_contract("ed25519Verify", Ed25519::verify_bytes)
    }

    fn sha256_hash(&self) -> ScalaBodyFn {
        self.hash_contract("sha256Hash", sha256::hash)
    }

    fn keccak256_hash(&self) -> ScalaBodyFn {
        self.hash_contract("keccak256Hash", keccak256::hash)
    }

    fn blake2b256_hash(&self) -> ScalaBodyFn {
        self.hash_contract("blake2b256Hash", blake2b256::hash)
    }

    // --- block / rev / ops ---------------------------------------------

    fn get_block_data(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let bd = self.block_data.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let bd = bd.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("blockData expects only a return channel"))?;
                match pars.as_slice() {
                    [ack] => {
                        let (block_number, sender_bytes) = {
                            let data = bd.lock().unwrap_or_else(|p| p.into_inner());
                            (i64::from(data.block_number), data.sender.bytes().to_vec())
                        };
                        let reply = vec![
                            RhoNumber::apply(block_number),
                            RhoByteArray::apply(sender_bytes),
                        ];
                        cc.produce(&rand, &reply, ack).await
                    }
                    _ => Err(illegal_arg("blockData expects only a return channel")),
                }
            })
        })
    }

    fn rev_address(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("revAddress expects an operation, an argument and an acknowledgement channel"))?;
                let [op, arg, ack] = pars.as_slice() else {
                    return Err(illegal_arg(
                        "revAddress expects an operation, an argument and an acknowledgement channel",
                    ));
                };
                let Some(op) = RhoString::unapply(op) else {
                    return Err(illegal_arg("revAddress expects an operation string"));
                };
                let response = match op {
                    "validate" => match RhoString::unapply(arg) {
                        Some(address) => RevAddress::parse(address)
                            .err()
                            .map(RhoString::apply)
                            .unwrap_or_default(),
                        None => Par::default(),
                    },
                    "fromPublicKey" => match RhoByteArray::unapply(arg) {
                        Some(pk) => RevAddress::from_public_key(&PublicKey::new(pk.to_vec()))
                            .map(|ra| RhoString::apply(ra.to_base58()))
                            .unwrap_or_default(),
                        None => Par::default(),
                    },
                    "fromDeployerId" => match RhoDeployerId::unapply(arg) {
                        Some(id) => RevAddress::from_deployer_id(id)
                            .map(|ra| RhoString::apply(ra.to_base58()))
                            .unwrap_or_default(),
                        None => Par::default(),
                    },
                    "fromUnforgeable" => match RhoName::unapply(arg) {
                        Some(g) => RhoString::apply(RevAddress::from_unforgeable(g).to_base58()),
                        None => Par::default(),
                    },
                    _ => return Err(illegal_arg("revAddress: unknown operation")),
                };
                cc.produce(&rand, &[response], ack).await
            })
        })
    }

    fn deployer_id_ops(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("deployerIdOps expects an operation, an argument and an acknowledgement channel"))?;
                let [op, arg, ack] = pars.as_slice() else {
                    return Err(illegal_arg(
                        "deployerIdOps expects an operation, an argument and an acknowledgement channel",
                    ));
                };
                let response = match RhoString::unapply(op) {
                    Some("pubKeyBytes") => match RhoDeployerId::unapply(arg) {
                        Some(pk) => RhoByteArray::apply(pk.to_vec()),
                        None => Par::default(),
                    },
                    _ => return Err(illegal_arg("deployerIdOps: unknown operation")),
                };
                cc.produce(&rand, &[response], ack).await
            })
        })
    }

    fn registry_ops(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("registryOps expects an operation, an argument and an acknowledgement channel"))?;
                let [op, arg, ack] = pars.as_slice() else {
                    return Err(illegal_arg(
                        "registryOps expects an operation, an argument and an acknowledgement channel",
                    ));
                };
                let response = match RhoString::unapply(op) {
                    Some("buildUri") => match RhoByteArray::unapply(arg) {
                        Some(ba) => RhoUri::apply(registry::build_uri(&blake2b256::hash(ba))),
                        None => Par::default(),
                    },
                    _ => return Err(illegal_arg("registryOps: unknown operation")),
                };
                cc.produce(&rand, &[response], ack).await
            })
        })
    }

    fn sys_auth_token_ops(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("sysAuthTokenOps expects an operation, an argument and an acknowledgement channel"))?;
                let [op, arg, ack] = pars.as_slice() else {
                    return Err(illegal_arg(
                        "sysAuthTokenOps expects an operation, an argument and an acknowledgement channel",
                    ));
                };
                let response = match RhoString::unapply(op) {
                    Some("check") => RhoBoolean::apply(RhoSysAuthToken::unapply(arg)),
                    _ => return Err(illegal_arg("sysAuthTokenOps: unknown operation")),
                };
                cc.produce(&rand, &[response], ack).await
            })
        })
    }

    // --- native registry -------------------------------------------------

    /// `rho:registry:lookup(uri, ret)` — return `(uri, value)` or `Nil` from the native registry.
    fn registry_lookup(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let native = self.native_state.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let native = native.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("registry lookup expects a uri and return channel"))?;
                let [uri, ret] = pars.as_slice() else {
                    return Err(illegal_arg("registry lookup expects a uri and return channel"));
                };
                let uri_str = RhoUri::unapply(uri)
                    .or_else(|| RhoString::unapply(uri))
                    .ok_or_else(|| illegal_arg("registry lookup expects a uri string"))?
                    .to_string();
                match native.registry_lookup(&uri_str).await.map_err(|e| illegal_arg(&e))? {
                    Some(value) => {
                        cc.produce(&rand, &[RhoTupleN::apply(vec![uri.clone(), value])], ret)
                            .await
                    }
                    None => cc.produce(&rand, &[RhoNil::apply()], ret).await,
                }
            })
        })
    }

    /// `rho:registry:insertArbitrary(data, ret)` — store `data` under a fresh URI and return it.
    fn registry_insert_arbitrary(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let native = self.native_state.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let native = native.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("insertArbitrary expects data and a return channel"))?;
                let [data, ret] = pars.as_slice() else {
                    return Err(illegal_arg("insertArbitrary expects data and a return channel"));
                };
                let uri = registry::build_uri(&blake2b256::hash(&rand.to_bytes()));
                native.registry_insert(&uri, data);
                cc.produce(&rand, &[RhoUri::apply(uri)], ret).await
            })
        })
    }

    /// `rho:registry:insertSigned:secp256k1((nonce, data), deployerID, ret)` — store `(nonce, data)`
    /// under the deployer-derived URI, or `Nil` when the nonce is stale.
    fn registry_insert_signed(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let native = self.native_state.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let native = native.clone();
            Box::pin(async move {
                let (pars, rand) = cc.unapply(&args).ok_or_else(|| {
                    illegal_arg("insertSigned expects (nonce, data), deployerID and a return channel")
                })?;
                let [signed, deployer_id, ret] = pars.as_slice() else {
                    return Err(illegal_arg(
                        "insertSigned expects (nonce, data), deployerID and a return channel",
                    ));
                };
                let tuple = RhoTupleN::unapply(signed).ok_or_else(|| {
                    illegal_arg("insertSigned expects a (nonce, data) tuple")
                })?;
                let [nonce_par, data] = tuple else {
                    return Err(illegal_arg("insertSigned expects a (nonce, data) tuple"));
                };
                let nonce = RhoNumber::unapply(nonce_par)
                    .ok_or_else(|| illegal_arg("insertSigned nonce must be a number"))?;
                let pub_key = RhoDeployerId::unapply(deployer_id)
                    .ok_or_else(|| illegal_arg("insertSigned expects a deployerID"))?;
                let uri = registry::build_uri(&blake2b256::hash(pub_key));

                if let Some(stored) = native.registry_lookup(&uri).await.map_err(|e| illegal_arg(&e))? {
                    let old_nonce = RhoTupleN::unapply(&stored)
                        .and_then(|ps| ps.first())
                        .and_then(|n| RhoNumber::unapply(n))
                        .unwrap_or(0);
                    if nonce <= old_nonce {
                        return cc.produce(&rand, &[RhoNil::apply()], ret).await;
                    }
                }
                native.registry_insert(
                    &uri,
                    &RhoTupleN::apply(vec![RhoNumber::apply(nonce), data.clone()]),
                );
                cc.produce(&rand, &[RhoUri::apply(uri)], ret).await
            })
        })
    }

    // --- native PoS ------------------------------------------------------

    /// `rho:rchain:pos` — native method dispatch over the PoS state (`getBonds`, `getActiveValidators`).
    fn pos(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let native = self.native_state.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let native = native.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("pos expects a method and arguments"))?;
                let [op, rest_par] = pars.as_slice() else {
                    return Err(illegal_arg("pos expects a method and arguments"));
                };
                let op = RhoString::unapply(op)
                    .ok_or_else(|| illegal_arg("pos method must be a string"))?;
                let rest = RhoList::unapply(rest_par)
                    .ok_or_else(|| illegal_arg("pos arguments must be a list"))?;
                match op {
                    "getBonds" => {
                        let [ret] = rest else {
                            return Err(illegal_arg("getBonds expects a return channel"));
                        };
                        let bonds = native.bonds().await.map_err(|e| illegal_arg(&e))?;
                        let kvs: Vec<(Par, Par)> = bonds
                            .iter()
                            .map(|(v, stake)| {
                                (
                                    RhoByteArray::apply(v.as_bytes().to_vec()),
                                    RhoNumber::apply(i64::from(*stake)),
                                )
                            })
                            .collect();
                        cc.produce(&rand, &[RhoMap::apply(kvs)], ret).await
                    }
                    "getActiveValidators" => {
                        let [ret] = rest else {
                            return Err(illegal_arg("getActiveValidators expects a return channel"));
                        };
                        let validators =
                            native.active_validators().await.map_err(|e| illegal_arg(&e))?;
                        let ps: Vec<Par> = validators
                            .iter()
                            .map(|v| RhoByteArray::apply(v.as_bytes().to_vec()))
                            .collect();
                        cc.produce(&rand, &[RhoSet::apply(ps)], ret).await
                    }
                    "bond" => {
                        let [deployer_id, amount, ret] = rest else {
                            return Err(illegal_arg(
                                "bond expects deployerId, amount and return channel",
                            ));
                        };
                        let deployer_id = RhoByteArray::unapply(deployer_id)
                            .ok_or_else(|| illegal_arg("bond expects a deployerId byte array"))?;
                        let amount = RhoNumber::unapply(amount)
                            .ok_or_else(|| illegal_arg("bond expects a number amount"))?;
                        let amount =
                            NonNegI64::try_from(amount).map_err(|e| illegal_arg(&e.to_string()))?;
                        let validator = Validator::try_from(deployer_id)
                            .map_err(|e| illegal_arg(&e.to_string()))?;
                        let out = match native.bond(&validator, amount).await.map_err(|e| illegal_arg(&e))? {
                            Ok(()) => RhoTupleN::apply(vec![RhoBoolean::apply(true), RhoNil::apply()]),
                            Err(msg) => RhoTupleN::apply(vec![
                                RhoBoolean::apply(false),
                                RhoString::apply(msg),
                            ]),
                        };
                        cc.produce(&rand, &[out], ret).await
                    }
                    "withdraw" => {
                        let [deployer_id, ret] = rest else {
                            return Err(illegal_arg("withdraw expects deployerId and return channel"));
                        };
                        let deployer_id = RhoByteArray::unapply(deployer_id)
                            .ok_or_else(|| illegal_arg("withdraw expects a deployerId byte array"))?;
                        let validator = Validator::try_from(deployer_id)
                            .map_err(|e| illegal_arg(&e.to_string()))?;
                        let out = match native.withdraw(&validator).await.map_err(|e| illegal_arg(&e))? {
                            Ok(()) => RhoTupleN::apply(vec![RhoBoolean::apply(true), RhoNil::apply()]),
                            Err(msg) => RhoTupleN::apply(vec![
                                RhoBoolean::apply(false),
                                RhoString::apply(msg),
                            ]),
                        };
                        cc.produce(&rand, &[out], ret).await
                    }
                    _ => Err(illegal_arg(&format!("pos: unknown method {op}"))),
                }
            })
        })
    }

    // --- native vault ----------------------------------------------------

    /// `rho:rchain:revVault` — native method dispatch over the vault balance map.
    fn rev_vault(&self) -> ScalaBodyFn {
        let cc = self.contract_call.clone();
        let native = self.native_state.clone();
        Box::new(move |args: Vec<ListParWithRandom>| {
            let cc = cc.clone();
            let native = native.clone();
            Box::pin(async move {
                let (pars, rand) = cc
                    .unapply(&args)
                    .ok_or_else(|| illegal_arg("revVault expects a method and arguments"))?;
                let [op, rest_par] = pars.as_slice() else {
                    return Err(illegal_arg("revVault expects a method and arguments"));
                };
                let op = RhoString::unapply(op)
                    .ok_or_else(|| illegal_arg("revVault method must be a string"))?;
                let rest = RhoList::unapply(rest_par)
                    .ok_or_else(|| illegal_arg("revVault arguments must be a list"))?;
                match op {
                    "getBalance" => {
                        let [addr, ret] = rest else {
                            return Err(illegal_arg("getBalance expects an address and return channel"));
                        };
                        let addr = RhoString::unapply(addr)
                            .ok_or_else(|| illegal_arg("getBalance expects a string address"))?;
                        let balance = match native.vault_balance(addr).await.map_err(|e| illegal_arg(&e))? {
                            Some(b) => b,
                            None => NonNegI64::zero(),
                        };
                        cc.produce(&rand, &[RhoNumber::apply(i64::from(balance))], ret)
                            .await
                    }
                    "deposit" => {
                        let [addr, amount, ret] = rest else {
                            return Err(illegal_arg("deposit expects an address, amount and return channel"));
                        };
                        let addr = RhoString::unapply(addr)
                            .ok_or_else(|| illegal_arg("deposit expects a string address"))?;
                        let amount = RhoNumber::unapply(amount)
                            .ok_or_else(|| illegal_arg("deposit expects a number amount"))?;
                        let amount =
                            NonNegI64::try_from(amount).map_err(|e| illegal_arg(&e.to_string()))?;
                        let current = match native.vault_balance(addr).await.map_err(|e| illegal_arg(&e))? {
                            Some(b) => b,
                            None => NonNegI64::zero(),
                        };
                        let new_balance = NonNegI64::try_from(i64::from(current) + i64::from(amount))
                            .map_err(|e| illegal_arg(&e.to_string()))?;
                        native.set_vault_balance(addr, new_balance);
                        cc.produce(&rand, &[RhoNil::apply()], ret).await
                    }
                    "transfer" => {
                        let [from, to, amount, ret] = rest else {
                            return Err(illegal_arg("transfer expects from, to, amount and return channel"));
                        };
                        let from = RhoString::unapply(from)
                            .ok_or_else(|| illegal_arg("transfer expects a string from-address"))?;
                        let to = RhoString::unapply(to)
                            .ok_or_else(|| illegal_arg("transfer expects a string to-address"))?;
                        let amount = RhoNumber::unapply(amount)
                            .ok_or_else(|| illegal_arg("transfer expects a number amount"))?;
                        let amount =
                            NonNegI64::try_from(amount).map_err(|e| illegal_arg(&e.to_string()))?;
                        let from_balance =
                            match native.vault_balance(from).await.map_err(|e| illegal_arg(&e))? {
                                Some(b) => b,
                                None => NonNegI64::zero(),
                            };
                        if i64::from(from_balance) < i64::from(amount) {
                            return Err(illegal_arg("transfer: insufficient balance"));
                        }
                        let to_balance =
                            match native.vault_balance(to).await.map_err(|e| illegal_arg(&e))? {
                                Some(b) => b,
                                None => NonNegI64::zero(),
                            };
                        let new_from = NonNegI64::try_from(i64::from(from_balance) - i64::from(amount))
                            .map_err(|e| illegal_arg(&e.to_string()))?;
                        let new_to = NonNegI64::try_from(i64::from(to_balance) + i64::from(amount))
                            .map_err(|e| illegal_arg(&e.to_string()))?;
                        native.set_vault_balance(from, new_from);
                        native.set_vault_balance(to, new_to);
                        cc.produce(&rand, &[RhoNil::apply()], ret).await
                    }
                    "findOrCreate" => {
                        let [addr, ret] = rest else {
                            return Err(illegal_arg("findOrCreate expects an address and return channel"));
                        };
                        let addr = RhoString::unapply(addr)
                            .ok_or_else(|| illegal_arg("findOrCreate expects a string address"))?;
                        native
                            .find_or_create_vault(addr)
                            .await
                            .map_err(|e| illegal_arg(&e))?;
                        // In the simplified address-keyed model the vault identifier is the address.
                        let out = RhoTupleN::apply(vec![
                            RhoBoolean::apply(true),
                            RhoString::apply(addr.to_string()),
                        ]);
                        cc.produce(&rand, &[out], ret).await
                    }
                    _ => Err(illegal_arg(&format!("revVault: unknown method {op}"))),
                }
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
    use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
    use rchain_models::sorted::SortedProc;
    use rchain_rspace::errors::RSpaceError;
    use rchain_rspace::tuple_space::{ContResult, Result as RSpaceResult, Tuplespace as RSpaceTuplespace};
    use std::collections::BTreeSet;
    use std::sync::{Arc, Mutex};

    struct MockSpace {
        produced: Mutex<Vec<(SortedProc, ListParWithRandom, bool)>>,
    }

    #[async_trait]
    impl RSpaceTuplespace<SortedProc, BindPattern, ListParWithRandom, TaggedContinuation> for MockSpace {
        async fn consume(
            &self,
            _channels: &[SortedProc],
            _patterns: &[BindPattern],
            _continuation: TaggedContinuation,
            _persist: bool,
            _peeks: BTreeSet<usize>,
        ) -> Result<
            Option<(
                ContResult<SortedProc, BindPattern, TaggedContinuation>,
                Vec<RSpaceResult<SortedProc, ListParWithRandom>>,
            )>,
            RSpaceError,
        > {
            Ok(None)
        }

        async fn produce(
            &self,
            channel: SortedProc,
            data: ListParWithRandom,
            persist: bool,
        ) -> Result<
            Option<(
                ContResult<SortedProc, BindPattern, TaggedContinuation>,
                Vec<RSpaceResult<SortedProc, ListParWithRandom>>,
            )>,
            RSpaceError,
        > {
            self.produced.lock().unwrap_or_else(|p| p.into_inner()).push((channel, data, persist));
            Ok(None)
        }

        async fn install(
            &self,
            _channels: &[SortedProc],
            _patterns: &[BindPattern],
            _continuation: TaggedContinuation,
        ) -> Result<Option<(TaggedContinuation, Vec<ListParWithRandom>)>, RSpaceError> {
            Ok(None)
        }
    }

    fn mock_system_processes(
        mock: &Arc<MockSpace>,
    ) -> (SystemProcesses, Vec<Definition>) {
        let charging = ChargingRSpace::new(
            mock.clone(),
            Arc::new(crate::accounting::CostAccounting::from_initial(
                crate::accounting::Costs::unsafe_max(),
            )),
        );
        let dispatcher = Arc::new(RholangAndScalaDispatcher::new(std::collections::BTreeMap::new()));
        let block_data = Arc::new(Mutex::new(BlockData::empty()));
        let native_state = Arc::new(NativeSystemState::new(Arc::new(
            rchain_rspace::native_store::InMemNativeStore::empty(),
        )));
        let sp = SystemProcesses::new(charging, dispatcher, block_data, native_state);
        let defs = sp.definitions();
        (sp, defs)
    }

    fn lpw(pars: Vec<Par>) -> ListParWithRandom {
        ListParWithRandom {
            pars: pars.into_iter().map(SortedProc::new).collect(),
            random_state: Blake2b512Random::new_random(128),
        }
    }

    #[tokio::test]
    async fn blake2b256_hash_contract_replies_with_hash() {
        let mock = Arc::new(MockSpace {
            produced: Mutex::new(Vec::new()),
        });
        let (_sp, defs) = mock_system_processes(&mock);

        let handler = defs
            .iter()
            .find(|d| d.body_ref == BodyRefs::BLAKE2B256_HASH)
            .expect("blake2b256Hash definition");
        let input = vec![1u8, 2, 3, 4];
        let ack = FixedChannels::stdout();
        let args = vec![lpw(vec![RhoByteArray::apply(input.clone()), ack.clone()])];
        (handler.handler)(args).await.unwrap();

        let produced = mock.produced.lock().unwrap_or_else(|p| p.into_inner());
        assert_eq!(produced.len(), 1);
        assert_eq!(produced[0].0.as_par(), &ack);
        assert_eq!(produced[0].1.pars, vec![SortedProc::new(RhoByteArray::apply(blake2b256::hash(&input)))]);
    }

    #[tokio::test]
    async fn registry_insert_arbitrary_then_lookup_round_trips() {
        let mock = Arc::new(MockSpace {
            produced: Mutex::new(Vec::new()),
        });
        let (_sp, defs) = mock_system_processes(&mock);

        let insert = defs
            .iter()
            .find(|d| d.body_ref == BodyRefs::REG_INSERT_RANDOM)
            .expect("insertArbitrary definition");
        let data = RhoNumber::apply(42);
        let ret = FixedChannels::stdout();
        (insert.handler)(vec![lpw(vec![data.clone(), ret.clone()])])
            .await
            .unwrap();

        let uri = {
            let produced = mock.produced.lock().unwrap_or_else(|p| p.into_inner());
            assert_eq!(produced.len(), 1);
            assert_eq!(produced[0].0.as_par(), &ret);
            produced[0].1.pars[0].clone()
        };
        assert!(RhoUri::unapply(uri.as_par()).is_some(), "insertArbitrary returns a URI");

        let lookup = defs
            .iter()
            .find(|d| d.body_ref == BodyRefs::REG_LOOKUP)
            .expect("lookup definition");
        let ret2 = FixedChannels::stdout_ack();
        (lookup.handler)(vec![lpw(vec![uri.as_par().clone(), ret2.clone()])])
            .await
            .unwrap();

        let produced = mock.produced.lock().unwrap_or_else(|p| p.into_inner());
        assert_eq!(produced.len(), 2);
        assert_eq!(produced[1].0.as_par(), &ret2);
        let tuple = &produced[1].1.pars[0];
        let parts = RhoTupleN::unapply(tuple.as_par()).expect("lookup returns a (uri, value) tuple");
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], *uri.as_par());
        assert_eq!(parts[1], data);
    }

    #[tokio::test]
    async fn pos_get_bonds_returns_bonds_map() {
        let mock = Arc::new(MockSpace {
            produced: Mutex::new(Vec::new()),
        });
        let native = NativeSystemState::new(Arc::new(
            rchain_rspace::native_store::InMemNativeStore::empty(),
        ));
        let mut bonds = std::collections::BTreeMap::new();
        bonds.insert(
            rchain_models::validator::Validator::new([1u8; 65]),
            NonNegI64::try_from(10).unwrap(),
        );
        bonds.insert(
            rchain_models::validator::Validator::new([2u8; 65]),
            NonNegI64::try_from(20).unwrap(),
        );
        native.set_bonds(&bonds);

        let charging = ChargingRSpace::new(
            mock.clone(),
            Arc::new(crate::accounting::CostAccounting::from_initial(
                crate::accounting::Costs::unsafe_max(),
            )),
        );
        let dispatcher = Arc::new(RholangAndScalaDispatcher::new(std::collections::BTreeMap::new()));
        let block_data = Arc::new(Mutex::new(BlockData::empty()));
        let sp = SystemProcesses::new(charging, dispatcher, block_data, Arc::new(native));
        let defs = sp.definitions();

        let pos = defs.iter().find(|d| d.body_ref == BodyRefs::POS).expect("pos definition");
        let ret = FixedChannels::stdout();
        let args = vec![lpw(vec![
            RhoString::apply("getBonds".to_string()),
            RhoList::apply(vec![ret.clone()]),
        ])];
        (pos.handler)(args).await.unwrap();

        let produced = mock.produced.lock().unwrap_or_else(|p| p.into_inner());
        assert_eq!(produced.len(), 1);
        assert_eq!(produced[0].0.as_par(), &ret);
        let map = RhoMap::unapply(produced[0].1.pars[0].as_par()).expect("getBonds returns a map");
        assert_eq!(map.len(), 2);
    }

    #[tokio::test]
    async fn rev_vault_deposit_transfer_and_get_balance() {
        let mock = Arc::new(MockSpace {
            produced: Mutex::new(Vec::new()),
        });
        let (_sp, defs) = mock_system_processes(&mock);
        let vault = defs
            .iter()
            .find(|d| d.body_ref == BodyRefs::REV_VAULT)
            .expect("revVault definition");

        let alice = "alice".to_string();
        let bob = "bob".to_string();

        // deposit(alice, 100, _) and deposit(bob, 50, _)
        for (addr, amt) in [(&alice, 100), (&bob, 50)] {
            let ret = FixedChannels::stdout();
            (vault.handler)(vec![lpw(vec![
                RhoString::apply("deposit".to_string()),
                RhoList::apply(vec![RhoString::apply(addr.clone()), RhoNumber::apply(amt), ret]),
            ])])
            .await
            .unwrap();
        }

        // transfer(alice, bob, 30, _)
        let ret = FixedChannels::stdout();
        (vault.handler)(vec![lpw(vec![
            RhoString::apply("transfer".to_string()),
            RhoList::apply(vec![
                RhoString::apply(alice.clone()),
                RhoString::apply(bob.clone()),
                RhoNumber::apply(30),
                ret,
            ]),
        ])])
        .await
        .unwrap();

        // getBalance(alice, ret) and getBalance(bob, ret)
        let alice_ret = FixedChannels::stdout_ack();
        (vault.handler)(vec![lpw(vec![
            RhoString::apply("getBalance".to_string()),
            RhoList::apply(vec![RhoString::apply(alice.clone()), alice_ret.clone()]),
        ])])
        .await
        .unwrap();
        let bob_ret = FixedChannels::stdout();
        (vault.handler)(vec![lpw(vec![
            RhoString::apply("getBalance".to_string()),
            RhoList::apply(vec![RhoString::apply(bob.clone()), bob_ret.clone()]),
        ])])
        .await
        .unwrap();

        let produced = mock.produced.lock().unwrap_or_else(|p| p.into_inner());
        // 2 deposits + 1 transfer + 2 getBalance = 5 produces.
        assert_eq!(produced.len(), 5);
        // The last two are the getBalance replies.
        assert_eq!(produced[3].0.as_par(), &alice_ret);
        assert_eq!(
            RhoNumber::unapply(produced[3].1.pars[0].as_par()).expect("alice balance"),
            70
        );
        assert_eq!(produced[4].0.as_par(), &bob_ret);
        assert_eq!(
            RhoNumber::unapply(produced[4].1.pars[0].as_par()).expect("bob balance"),
            80
        );
    }
}
