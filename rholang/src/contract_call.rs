//! System-contract message unapplying (port of `ContractCall.scala`).

use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::Par;
use rchain_models::runtime::ListParWithRandom;
use rchain_models::sorted::SortedProc;

use crate::errors::RholangError;
use crate::reduce::{Dispatch, Tuplespace};

/// Unapplies a message sent to a system contract, returning the producer and the message content
/// (port of `ContractCall`). The producer is separated into [`ContractCall::produce`] plus the
/// recovered random state, so callers can send a reply without capturing the store.
#[derive(Clone)]
pub struct ContractCall<T: Tuplespace, D: Dispatch> {
    space: T,
    dispatcher: D,
}

impl<T: Tuplespace, D: Dispatch> ContractCall<T, D> {
    pub fn new(space: T, dispatcher: D) -> Self {
        ContractCall { space, dispatcher }
    }

    /// Send `values` through `ch`, dispatching any matched continuation (port of `produce`).
    pub async fn produce(
        &self,
        rand: &Blake2b512Random,
        values: &[Par],
        ch: &Par,
    ) -> Result<(), RholangError> {
        let result = self
            .space
            .produce(
                &SortedProc::new(ch.clone()),
                ListParWithRandom {
                    pars: values.iter().map(|p| SortedProc::new(p.clone())).collect(),
                    random_state: rand.clone(),
                },
                false,
            )
            .await?;
        if let Some((continuation, data_list, _)) = result {
            let data: Vec<ListParWithRandom> =
                data_list.iter().map(|(_, matched, _, _)| matched.clone()).collect();
            self.dispatcher.dispatch(continuation, data).await?;
        }
        Ok(())
    }

    /// Extract the message content and its random state if there is exactly one argument (port of
    /// `unapply`).
    pub fn unapply(
        &self,
        contract_args: &[ListParWithRandom],
    ) -> Option<(Vec<Par>, Blake2b512Random)> {
        if let [single] = contract_args {
            Some((single.pars.iter().map(|p| p.as_par().clone()).collect(), single.random_state.clone()))
        } else {
            None
        }
    }
}
