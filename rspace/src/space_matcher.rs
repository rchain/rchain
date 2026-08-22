//! Pattern-matching search over data/continuations.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/SpaceMatcher.scala`.

use std::collections::BTreeMap;

use crate::internal::{ConsumeCandidate, Datum, ProduceCandidate, WaitingContinuation};
use crate::match_::Match;

/// Search data for a match with a pattern (port of `findMatchingDataCandidate`).
pub fn find_matching_data_candidate<C, P, A>(
    channel: &C,
    data: &[(Datum<A>, i64)],
    pattern: &P,
    m: &dyn Match<P, A>,
) -> Option<(ConsumeCandidate<C, A>, Vec<(Datum<A>, i64)>)>
where
    C: Clone,
    A: Clone,
{
    let mut prefix: Vec<(Datum<A>, i64)> = Vec::new();
    let mut remaining = data;
    loop {
        match remaining.first() {
            None => return None,
            Some((datum, data_index)) => match m.get(pattern, &datum.a) {
                None => {
                    prefix.insert(0, remaining[0].clone());
                    remaining = &remaining[1..];
                }
                Some(mat) => {
                    let indexed_datums = if datum.persist {
                        data.to_vec()
                    } else {
                        let mut out = prefix.clone();
                        out.extend_from_slice(&remaining[1..]);
                        out
                    };
                    let candidate = ConsumeCandidate {
                        channel: channel.clone(),
                        datum: Datum {
                            a: mat,
                            persist: datum.persist,
                            source: datum.source.clone(),
                        },
                        removed_datum: datum.a.clone(),
                        datum_index: *data_index,
                    };
                    return Some((candidate, indexed_datums));
                }
            },
        }
    }
}

/// Iterate (channel, pattern) pairs looking for matching data (port of `extractDataCandidates`).
pub fn extract_data_candidates<C, P, A>(
    channel_pattern_pairs: &[(C, P)],
    channel_to_indexed_data: &BTreeMap<C, Vec<(Datum<A>, i64)>>,
    m: &dyn Match<P, A>,
) -> Vec<Option<ConsumeCandidate<C, A>>>
where
    C: Ord + Clone,
    A: Clone,
{
    let mut acc: Vec<Option<ConsumeCandidate<C, A>>> = Vec::new();
    let mut map = channel_to_indexed_data.clone();
    for (channel, pattern) in channel_pattern_pairs {
        let maybe = match map.get(channel) {
            Some(indexed_data) => {
                find_matching_data_candidate(channel, indexed_data, pattern, m)
            }
            None => None,
        };
        match maybe {
            Some((candidate, rem)) => {
                map.insert(channel.clone(), rem);
                acc.push(Some(candidate));
            }
            None => acc.push(None),
        }
    }
    acc
}

/// Find the first waiting continuation whose patterns match all channels (port of
/// `extractFirstMatch`).
pub fn extract_first_match<C, P, A, K>(
    channels: &[C],
    match_candidates: &[(WaitingContinuation<P, K>, usize)],
    channel_to_indexed_data: &BTreeMap<C, Vec<(Datum<A>, i64)>>,
    m: &dyn Match<P, A>,
) -> Option<ProduceCandidate<C, P, A, K>>
where
    C: Ord + Clone,
    A: Clone,
    P: Clone,
    K: Clone,
{
    for (wc, index) in match_candidates {
        let data_candidates = extract_data_candidates(
            &channels.iter().cloned().zip(wc.patterns.iter().cloned()).collect::<Vec<_>>(),
            channel_to_indexed_data,
            m,
        );
        if data_candidates.iter().all(|c| c.is_some()) {
            return Some(ProduceCandidate {
                channels: channels.to_vec(),
                continuation: wc.clone(),
                continuation_index: *index,
                data_candidates: data_candidates.into_iter().flatten().collect(),
            });
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
    use crate::trace::event::Produce;

    struct EqMatch;
    impl Match<i32, i32> for EqMatch {
        fn get(&self, p: &i32, a: &i32) -> Option<i32> {
            if p == a { Some(*a) } else { None }
        }
    }

    fn datum(a: i32) -> Datum<i32> {
        Datum {
            a,
            persist: false,
            source: Produce::from_hash(
                Blake2b256Hash::from_bytes([0; 32]),
                Blake2b256Hash::from_bytes([0; 32]),
                false,
            ),
        }
    }

    #[test]
    fn find_matching_data_candidate_finds_first_match() {
        let data = vec![(datum(1), 0), (datum(2), 1)];
        let result = find_matching_data_candidate(&0i32, &data, &2, &EqMatch).unwrap();
        assert_eq!(result.0.datum.a, 2);
        assert_eq!(result.0.datum_index, 1);
    }

    #[test]
    fn find_matching_data_candidate_none_when_no_match() {
        let data = vec![(datum(1), 0)];
        assert!(find_matching_data_candidate(&0i32, &data, &99, &EqMatch).is_none());
    }
}
