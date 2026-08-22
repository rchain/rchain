//! A change to a channel (added/removed values), a monoid.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/merger/ChannelChange.scala`.

/// Change to a channel (port of `ChannelChange`).
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ChannelChange<A> {
    pub added: Vec<A>,
    pub removed: Vec<A>,
}

impl<A: Clone> ChannelChange<A> {
    pub fn empty() -> Self {
        ChannelChange {
            added: Vec::new(),
            removed: Vec::new(),
        }
    }

    /// Concatenate the added/removed vectors (port of `ChannelChange.combine`).
    pub fn combine(x: &ChannelChange<A>, y: &ChannelChange<A>) -> ChannelChange<A> {
        let mut added = x.added.clone();
        added.extend(y.added.clone());
        let mut removed = x.removed.clone();
        removed.extend(y.removed.clone());
        ChannelChange { added, removed }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn combine_is_associative() {
        let a = ChannelChange { added: vec![1], removed: vec![2] };
        let b = ChannelChange { added: vec![3], removed: vec![] };
        let c = ChannelChange { added: vec![], removed: vec![4] };
        let ab = ChannelChange::combine(&a, &b);
        let bc = ChannelChange::combine(&b, &c);
        assert_eq!(
            ChannelChange::combine(&ab, &c),
            ChannelChange::combine(&a, &bc)
        );
    }

    #[test]
    fn empty_is_identity() {
        let a = ChannelChange { added: vec![1], removed: vec![2] };
        assert_eq!(ChannelChange::combine(&a, &ChannelChange::empty()), a);
    }
}
