//! Convert between rspace trace events and the casper wire `Event` (port of `EventConverter.scala`).

use std::collections::{BTreeMap, BTreeSet};

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::casper::protocol::casper_message::{
    CommEvent, ConsumeEvent, Event, Peek, ProduceEvent,
};
use rchain_rspace::trace::event::{Comm, Consume, Event as REvent, Produce};

fn bytes_to_hash(bytes: &[u8]) -> Blake2b256Hash {
    Blake2b256Hash::from_byte_array(bytes)
}

fn hash_to_bytes(hash: &Blake2b256Hash) -> Vec<u8> {
    hash.as_bytes().to_vec()
}

/// Convert a casper wire `Event` into an rspace trace event (port of `toRspaceEvent`).
pub fn to_rspace_event(event: &Event) -> REvent {
    match event {
        Event::Produce(pe) => REvent::Produce(Produce::from_hash(
            bytes_to_hash(&pe.channels_hash),
            bytes_to_hash(&pe.hash),
            pe.persistent,
        )),
        Event::Consume(ce) => REvent::Consume(Consume::from_hash(
            ce.channels_hashes.iter().map(|b| bytes_to_hash(b)).collect(),
            bytes_to_hash(&ce.hash),
            ce.persistent,
        )),
        Event::Comm(comme) => {
            let consume = Consume::from_hash(
                comme
                    .consume
                    .channels_hashes
                    .iter()
                    .map(|b| bytes_to_hash(b))
                    .collect(),
                bytes_to_hash(&comme.consume.hash),
                comme.consume.persistent,
            );
            let mut times_repeated: BTreeMap<Produce, usize> = BTreeMap::new();
            for pe in &comme.produces {
                let p = Produce::from_hash(
                    bytes_to_hash(&pe.channels_hash),
                    bytes_to_hash(&pe.hash),
                    pe.persistent,
                );
                times_repeated.insert(p, pe.times_repeated as usize);
            }
            let mut produces: Vec<Produce> = times_repeated.keys().cloned().collect();
            produces.sort_by_key(|p| (p.channels_hash, p.hash, p.persistent));
            let peeks: BTreeSet<usize> =
                comme.peeks.iter().map(|p| p.channel_index as usize).collect();
            REvent::Comm(Comm {
                consume,
                produces,
                peeks,
                times_repeated,
            })
        }
    }
}

/// Convert an rspace trace event into a casper wire `Event` (port of `toCasperEvent`).
pub fn to_casper_event(event: &REvent) -> Event {
    match event {
        REvent::Produce(p) => Event::Produce(ProduceEvent {
            channels_hash: hash_to_bytes(&p.channels_hash),
            hash: hash_to_bytes(&p.hash),
            persistent: p.persistent,
            times_repeated: 0,
        }),
        REvent::Consume(c) => Event::Consume(ConsumeEvent {
            channels_hashes: c.channels_hashes.iter().map(hash_to_bytes).collect(),
            hash: hash_to_bytes(&c.hash),
            persistent: c.persistent,
        }),
        REvent::Comm(comm) => Event::Comm(CommEvent {
            consume: ConsumeEvent {
                channels_hashes: comm.consume.channels_hashes.iter().map(hash_to_bytes).collect(),
                hash: hash_to_bytes(&comm.consume.hash),
                persistent: comm.consume.persistent,
            },
            produces: comm
                .produces
                .iter()
                .map(|p| ProduceEvent {
                    channels_hash: hash_to_bytes(&p.channels_hash),
                    hash: hash_to_bytes(&p.hash),
                    persistent: p.persistent,
                    times_repeated: comm.times_repeated.get(p).copied().unwrap_or(0) as i32,
                })
                .collect(),
            peeks: comm
                .peeks
                .iter()
                .map(|&i| Peek {
                    channel_index: i as i32,
                })
                .collect(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn h(byte: u8) -> Blake2b256Hash {
        Blake2b256Hash::from_bytes([byte; 32])
    }

    #[test]
    fn produce_round_trips() {
        let e = Event::Produce(ProduceEvent {
            channels_hash: h(1).as_bytes().to_vec(),
            hash: h(2).as_bytes().to_vec(),
            persistent: false,
            times_repeated: 0,
        });
        let r = to_rspace_event(&e);
        assert_eq!(to_casper_event(&r), e);
    }

    #[test]
    fn consume_round_trips() {
        let e = Event::Consume(ConsumeEvent {
            channels_hashes: vec![h(1).as_bytes().to_vec(), h(2).as_bytes().to_vec()],
            hash: h(3).as_bytes().to_vec(),
            persistent: true,
        });
        let r = to_rspace_event(&e);
        assert_eq!(to_casper_event(&r), e);
    }

    #[test]
    fn comm_round_trips() {
        let e = Event::Comm(CommEvent {
            consume: ConsumeEvent {
                channels_hashes: vec![h(1).as_bytes().to_vec()],
                hash: h(2).as_bytes().to_vec(),
                persistent: false,
            },
            produces: vec![
                ProduceEvent {
                    channels_hash: h(1).as_bytes().to_vec(),
                    hash: h(3).as_bytes().to_vec(),
                    persistent: false,
                    times_repeated: 2,
                },
                ProduceEvent {
                    channels_hash: h(1).as_bytes().to_vec(),
                    hash: h(4).as_bytes().to_vec(),
                    persistent: true,
                    times_repeated: 1,
                },
            ],
            peeks: vec![Peek { channel_index: 0 }],
        });
        let r = to_rspace_event(&e);
        assert_eq!(to_casper_event(&r), e);
    }
}
