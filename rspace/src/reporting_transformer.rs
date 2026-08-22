//! Transforming reporting events into more readable forms (port of `ReportingTransformer.scala`).

use crate::reporting_rspace::{
    ReportingComm, ReportingConsume, ReportingEvent, ReportingProduce,
};

/// Transforms [`ReportingEvent`]s into some target type `E` (port of `ReportingTransformer`).
pub trait ReportingTransformer<C, P, A, K, E> {
    fn serialize_consume(&self, rc: &ReportingConsume<C, P, K>) -> E;
    fn serialize_produce(&self, rp: &ReportingProduce<C, A>) -> E;
    fn serialize_comm(&self, rc: &ReportingComm<C, P, A, K>) -> E;

    fn transform_event(&self, re: &ReportingEvent<C, P, A, K>) -> E {
        match re {
            ReportingEvent::Comm(comm) => self.serialize_comm(comm),
            ReportingEvent::Consume(cons) => self.serialize_consume(cons),
            ReportingEvent::Produce(prod) => self.serialize_produce(prod),
        }
    }
}

/// Stringified reporting events (port of `ReportingRhoStringTransformer.RhoEvent`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RhoEvent {
    Comm(RhoComm),
    Produce(RhoProduce),
    Consume(RhoConsume),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RhoComm {
    pub consume: RhoConsume,
    pub produces: Vec<RhoProduce>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RhoProduce {
    pub channel: String,
    pub data: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RhoConsume {
    pub channels: String,
    pub patterns: String,
    pub continuation: String,
}

/// A [`ReportingTransformer`] that renders events as strings (port of
/// `ReportingEventStringTransformer`).
pub struct ReportingEventStringTransformer<C, P, A, K> {
    pub serialize_c: fn(&C) -> String,
    pub serialize_p: fn(&P) -> String,
    pub serialize_a: fn(&A) -> String,
    pub serialize_k: fn(&K) -> String,
}

impl<C, P, A, K> ReportingTransformer<C, P, A, K, RhoEvent> for ReportingEventStringTransformer<C, P, A, K> {
    fn serialize_consume(&self, rc: &ReportingConsume<C, P, K>) -> RhoEvent {
        RhoEvent::Consume(self.consume_to_rho(rc))
    }

    fn serialize_produce(&self, rp: &ReportingProduce<C, A>) -> RhoEvent {
        RhoEvent::Produce(self.produce_to_rho(rp))
    }

    fn serialize_comm(&self, rc: &ReportingComm<C, P, A, K>) -> RhoEvent {
        let consume = self.consume_to_rho(&rc.consume);
        let produces = rc.produces.iter().map(|rp| self.produce_to_rho(rp)).collect();
        RhoEvent::Comm(RhoComm { consume, produces })
    }
}

impl<C, P, A, K> ReportingEventStringTransformer<C, P, A, K> {
    fn consume_to_rho(&self, rc: &ReportingConsume<C, P, K>) -> RhoConsume {
        let k = (self.serialize_k)(&rc.continuation);
        let chs = rc
            .channels
            .iter()
            .map(self.serialize_c)
            .collect::<Vec<_>>()
            .join(";");
        let ps = rc
            .patterns
            .iter()
            .map(self.serialize_p)
            .collect::<Vec<_>>()
            .join(";");
        RhoConsume {
            channels: format!("[{}]", chs),
            patterns: format!("[{}]", ps),
            continuation: k,
        }
    }

    fn produce_to_rho(&self, rp: &ReportingProduce<C, A>) -> RhoProduce {
        RhoProduce {
            channel: (self.serialize_c)(&rp.channel),
            data: (self.serialize_a)(&rp.data),
        }
    }
}
