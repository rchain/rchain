//! Gas (phlogiston) cost accounting.
//!
//! Mirrors `rholang/.../interpreter/accounting/` (`Cost`, `Costs`, `Chargeable`, `CostAccounting`).
//! The proto-size-dependent costs (`equalityCheckCost`, `storageCost*`, `toByteArrayCost`, and
//! `Chargeable.fromProtobuf`) are deferred until wire serialization is ported. The cats-mtl
//! `_cost[F]` monad stack is modeled as the synchronous [`CostAccounting`] state cell.

use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Mutex;

use num_bigint::BigInt;

use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_shared::serialize::Serialize;

use crate::errors::RholangError;

/// A gas cost (port of `Cost`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cost {
    pub value: i64,
    pub operation: String,
}

impl Cost {
    pub fn new(value: i64, operation: impl Into<String>) -> Cost {
        Cost {
            value,
            operation: operation.into(),
        }
    }

    /// Scale a cost (port of `Cost.*`).
    pub fn mul(&self, base: i64) -> Cost {
        Cost {
            value: self.value * base,
            operation: format!("({} * {})", self.operation, base),
        }
    }

    /// Add two costs (port of `Cost.+`).
    pub fn add(&self, other: &Cost) -> Cost {
        Cost {
            value: self.value + other.value,
            operation: String::new(),
        }
    }

    /// Subtract two costs (port of `Cost.-`).
    pub fn sub(&self, other: &Cost) -> Cost {
        Cost {
            value: self.value - other.value,
            operation: String::new(),
        }
    }
}

/// The cost table (port of the `Costs` trait).
pub struct Costs;

impl Costs {
    pub fn sum_cost() -> Cost {
        Cost::new(3, "sum")
    }
    pub fn subtraction_cost() -> Cost {
        Cost::new(3, "subtraction")
    }
    pub fn boolean_and_cost() -> Cost {
        Cost::new(2, "boolean and")
    }
    pub fn boolean_or_cost() -> Cost {
        Cost::new(2, "boolean or")
    }
    pub fn comparison_cost() -> Cost {
        Cost::new(3, "comparison")
    }
    pub fn multiplication_cost() -> Cost {
        Cost::new(9, "multiplication")
    }
    pub fn division_cost() -> Cost {
        Cost::new(9, "division")
    }
    pub fn modulo_cost() -> Cost {
        Cost::new(9, "modulo")
    }
    pub fn lookup_cost() -> Cost {
        Cost::new(3, "lookup")
    }
    pub fn remove_cost() -> Cost {
        Cost::new(3, "removal")
    }
    pub fn add_cost() -> Cost {
        Cost::new(3, "addition")
    }
    pub fn hex_to_bytes_cost(s: &str) -> Cost {
        Cost::new(s.len() as i64, "hex to bytes")
    }
    pub fn bytes_to_hex_cost(bytes: &[u8]) -> Cost {
        Cost::new(bytes.len() as i64, "bytes to hex")
    }
    pub fn diff_cost(num_elements: i64) -> Cost {
        Cost::new(
            Self::remove_cost().mul(num_elements).value,
            format!("{num_elements} elements diff cost"),
        )
    }
    pub fn union_cost(num_elements: i64) -> Cost {
        Cost::new(
            Self::add_cost().mul(num_elements).value,
            format!("{num_elements} union cost"),
        )
    }
    pub fn list_append_cost(size: i64) -> Cost {
        Cost::new(size, "list append")
    }
    pub fn string_append_cost(n: i64, m: i64) -> Cost {
        Cost::new(n + m, "string append")
    }
    pub fn interpolate_cost(str_length: i64, map_size: i64) -> Cost {
        Cost::new(str_length * map_size, "interpolate")
    }
    pub fn to_int_cost_bigint(bi: &BigInt) -> Cost {
        Cost::new(Self::big_int_size(bi), "bigint to int")
    }
    pub fn to_int_cost_string(s: &str) -> Cost {
        Cost::new(s.len() as i64, "string to int")
    }
    pub fn int_to_bigint_cost() -> Cost {
        Cost::new(8, "int to bigint")
    }
    pub fn to_bigint_cost(s: &str) -> Cost {
        Cost::new(s.len() as i64, "string to bigint")
    }
    pub fn big_int_negation(bi: &BigInt) -> Cost {
        Cost::new(Self::big_int_size(bi), "bigint negation")
    }
    pub fn big_int_comparison(left: &BigInt, right: &BigInt) -> Cost {
        Cost::new(
            Self::big_int_size(left).min(Self::big_int_size(right)),
            "bigint comparison",
        )
    }
    pub fn big_int_sum(left: &BigInt, right: &BigInt) -> Cost {
        Cost::new(
            Self::big_int_size(left).max(Self::big_int_size(right)) + 1,
            "bigint sum",
        )
    }
    pub fn big_int_subtraction(left: &BigInt, right: &BigInt) -> Cost {
        Cost::new(
            Self::big_int_size(left).max(Self::big_int_size(right)) + 1,
            "bigint subtraction",
        )
    }
    pub fn big_int_multiplication(left: &BigInt, right: &BigInt) -> Cost {
        Cost::new(
            Self::big_int_size(left) * Self::big_int_size(right),
            "bigint multiplication",
        )
    }
    pub fn big_int_division(left: &BigInt, right: &BigInt) -> Cost {
        Cost::new(
            Self::big_int_size(left) * Self::big_int_size(right),
            "bigint division",
        )
    }
    pub fn big_int_modulo(left: &BigInt, right: &BigInt) -> Cost {
        Cost::new(
            Self::big_int_size(left) * Self::big_int_size(right),
            "bigint modulo",
        )
    }
    /// The number of bytes needed to store a bigint (port of `bigIntSize`).
    pub fn big_int_size(bi: &BigInt) -> i64 {
        (bi.magnitude().bits() / 8 + 1) as i64
    }
    pub fn size_method_cost(size: i64) -> Cost {
        Cost::new(size, "size")
    }
    pub fn slice_cost(to: i64) -> Cost {
        Cost::new(to, "slice")
    }
    pub fn take_cost(to: i64) -> Cost {
        Cost::new(to, "take")
    }
    pub fn to_list_cost(size: i64) -> Cost {
        Cost::new(size, "toList")
    }
    pub fn parsing_cost(term: &str) -> Cost {
        Cost::new(term.len() as i64, "parsing")
    }
    pub fn nth_method_call_cost() -> Cost {
        Cost::new(10, "nth method call")
    }
    pub fn keys_method_cost() -> Cost {
        Cost::new(10, "keys method")
    }
    pub fn length_method_cost() -> Cost {
        Cost::new(10, "length method")
    }
    pub fn method_call_cost() -> Cost {
        Cost::new(10, "method call")
    }
    pub fn op_call_cost() -> Cost {
        Cost::new(10, "op call")
    }
    pub fn var_eval_cost() -> Cost {
        Cost::new(10, "var eval")
    }
    pub fn send_eval_cost() -> Cost {
        Cost::new(11, "send eval")
    }
    pub fn receive_eval_cost() -> Cost {
        Cost::new(11, "receive eval")
    }
    pub fn channel_eval_cost() -> Cost {
        Cost::new(11, "channel eval")
    }
    pub fn new_binding_cost() -> Cost {
        Cost::new(2, "new binding")
    }
    pub fn new_eval_cost() -> Cost {
        Cost::new(10, "new eval")
    }
    pub fn new_bindings_cost(n: i64) -> Cost {
        Cost::new(
            Self::new_binding_cost().mul(n).add(&Self::new_eval_cost()).value,
            format!("{n} new bindings"),
        )
    }
    pub fn match_eval_cost() -> Cost {
        Cost::new(12, "match eval")
    }

    /// Size-proportional equality check (port of `equalityCheckCost`): the smaller serialized size.
    pub fn equality_check_cost<T, P>(x: &T, y: &P) -> Cost
    where
        T: Serialize<T>,
        P: Serialize<P>,
    {
        Cost::new(
            (<T as Serialize<T>>::encode(x).len() as i64)
                .min(<P as Serialize<P>>::encode(y).len() as i64),
            "equality check",
        )
    }

    /// Serializing a term into a byte array allocates + copies `serializedSize` bytes (port of
    /// `toByteArrayCost`).
    pub fn to_byte_array_cost<T: Serialize<T>>(a: &T) -> Cost {
        Cost::new(<T as Serialize<T>>::encode(a).len() as i64, "to byte array")
    }

    /// Storage cost: the sum of the serialized sizes (port of `storageCost`).
    pub fn storage_cost<T: Serialize<T>>(terms: &[T]) -> Cost {
        Cost::new(
            terms.iter().map(|a| <T as Serialize<T>>::encode(a).len() as i64).sum(),
            "storage cost",
        )
    }

    /// Consume storage cost: channels + patterns + the `ParBody` continuation body (port of
    /// `storageCostConsume`).
    pub fn storage_cost_consume(
        channels: &[SortedProc],
        patterns: &[BindPattern],
        continuation: &TaggedContinuation,
    ) -> Cost {
        let channels_cost: i64 = channels
            .iter()
            .map(|c| <SortedProc as Serialize<SortedProc>>::encode(c).len() as i64)
            .sum();
        let patterns_cost: i64 = patterns
            .iter()
            .map(|p| <BindPattern as Serialize<BindPattern>>::encode(p).len() as i64)
            .sum();
        let body_cost: i64 = match continuation {
            TaggedContinuation::ParBody(pwr) => {
                <SortedProc as Serialize<SortedProc>>::encode(&pwr.body).len() as i64
            }
            _ => 0,
        };
        Cost::new(channels_cost + patterns_cost + body_cost, "consume storage")
    }

    /// Produce storage cost: channel + the produced data (port of `storageCostProduce`).
    pub fn storage_cost_produce(channel: &SortedProc, data: &ListParWithRandom) -> Cost {
        let channel_cost = <SortedProc as Serialize<SortedProc>>::encode(channel).len() as i64;
        let data_cost: i64 = data
            .pars
            .iter()
            .map(|p| <SortedProc as Serialize<SortedProc>>::encode(p).len() as i64)
            .sum();
        Cost::new(channel_cost + data_cost, "produces storage")
    }

    const HASH_LEN: i64 = 32;

    pub fn event_storage_cost(channels_involved: i64) -> Cost {
        Cost::new(
            Self::HASH_LEN + channels_involved * Self::HASH_LEN,
            "event storage cost",
        )
    }
    pub fn comm_event_storage_cost(channels_involved: i64) -> Cost {
        let consume = Self::event_storage_cost(channels_involved);
        let produces = Self::event_storage_cost(1).mul(channels_involved);
        let mut c = consume.add(&produces);
        c.operation = "comm event storage cost".to_string();
        c
    }

    pub fn max_value() -> i64 {
        i32::MAX as i64
    }
    pub fn unsafe_max() -> Cost {
        Cost::new(i32::MAX as i64, "")
    }
}

/// Typeclass for charging a term (port of `Chargeable`).
pub trait Chargeable<A> {
    fn cost(a: &A) -> i64;
}

/// The protobuf `fromProtobuf` instance: charge a term by its serialized (wire) size.
impl<T> Chargeable<T> for T
where
    T: Serialize<T>,
{
    fn cost(a: &T) -> i64 {
        <T as Serialize<T>>::encode(a).len() as i64
    }
}

/// The synchronous cost-accounting state (models the `_cost[F]` monad stack).
#[derive(Default)]
pub struct CostAccounting {
    value: AtomicI64,
    log: Mutex<Vec<Cost>>,
}

impl CostAccounting {
    pub fn new() -> Self {
        CostAccounting::default()
    }

    pub fn from_initial(init: Cost) -> Self {
        CostAccounting {
            value: AtomicI64::new(init.value),
            log: Mutex::new(Vec::new()),
        }
    }

    pub fn get(&self) -> Cost {
        Cost::new(self.value.load(Ordering::SeqCst), "get")
    }

    /// Total phlo charged so far (the sum of every `charge`d amount, i.e. consumed phlo).
    pub fn total_charged(&self) -> i64 {
        self.log.lock().unwrap_or_else(|p| p.into_inner()).iter().map(|c| c.value).sum()
    }

    /// Set the current cost balance (port of `_cost.set`).
    pub fn set(&self, cost: Cost) {
        self.value.store(cost.value, Ordering::SeqCst);
    }

    /// Charge `amount` phlogistons, raising `OutOfPhlogistonsError` if the balance goes negative
    /// (port of `charge`).
    pub fn charge(&self, amount: Cost) -> Result<(), RholangError> {
        let current = self.value.load(Ordering::SeqCst);
        if current < 0 {
            return Err(RholangError::OutOfPhlogistonsError);
        }
        let amount_value = amount.value;
        self.log.lock().unwrap_or_else(|p| p.into_inner()).push(amount);
        self.value.store(current - amount_value, Ordering::SeqCst);
        if self.value.load(Ordering::SeqCst) < 0 {
            return Err(RholangError::OutOfPhlogistonsError);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cost_arithmetic() {
        let a = Cost::new(3, "sum");
        let b = Cost::new(5, "sub");
        assert_eq!(a.mul(2).value, 6);
        assert_eq!(a.mul(2).operation, "(sum * 2)");
        assert_eq!(a.add(&b).value, 8);
        assert_eq!(a.add(&b).operation, "");
        assert_eq!(a.sub(&b).value, -2);
    }

    #[test]
    fn charge_decrements_and_errors_when_exhausted() {
        let acc = CostAccounting::from_initial(Cost::new(10, "init"));
        assert!(acc.charge(Cost::new(4, "x")).is_ok());
        assert_eq!(acc.get().value, 6);
        assert!(acc.charge(Cost::new(7, "y")).is_err());
    }

    #[test]
    fn size_proportional_costs_match_serialized_size() {
        let par = rchain_models::par_ops::from_expr(rchain_models::ast::Expr::GInt(42));
        let encoded =
            <rchain_models::ast::Par as Serialize<rchain_models::ast::Par>>::encode(&par);
        let len = encoded.len() as i64;

        assert_eq!(Costs::to_byte_array_cost(&par).value, len);
        assert_eq!(Costs::to_byte_array_cost(&par).operation, "to byte array");
        assert_eq!(Costs::storage_cost(&[par.clone()]).value, len);
        assert_eq!(Costs::equality_check_cost(&par, &par).value, len);
    }
}
