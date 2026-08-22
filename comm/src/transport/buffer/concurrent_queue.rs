//! Bounded concurrent queue.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/buffer/ConcurrentQueue.scala`. The Monix
//! `MpscArrayQueue` becomes `crossbeam::queue::ArrayQueue` (a bounded MPSC queue).

use crossbeam::queue::ArrayQueue;

/// Recommended initial capacity (the Scala `ConcurrentQueue.recommendedSize`).
pub const RECOMMENDED_SIZE: usize = 1024;

/// A bounded concurrent queue (port of `ConcurrentQueue[A]`).
pub struct ConcurrentQueue<A> {
    queue: ArrayQueue<A>,
}

impl<A> ConcurrentQueue<A> {
    /// Create a bounded queue whose capacity is the next power of two, at least 4 (port of
    /// `ConcurrentQueue.limited`).
    pub fn limited(capacity: usize) -> Self {
        let max_capacity = capacity.next_power_of_two().max(4);
        ConcurrentQueue {
            queue: ArrayQueue::new(max_capacity),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    /// Try to enqueue an element; returns `false` if the queue is full (port of `offer`).
    pub fn offer(&self, elem: A) -> bool {
        self.queue.push(elem).is_ok()
    }

    /// Dequeue an element, or `None` if empty (port of `poll`).
    pub fn poll(&self) -> Option<A> {
        self.queue.pop()
    }

    /// Dequeue up to `limit` elements into `buffer` (port of `drain`).
    pub fn drain(&self, buffer: &mut Vec<A>, limit: usize) {
        let mut fetched = 0;
        while fetched < limit {
            match self.queue.pop() {
                Some(next) => {
                    buffer.push(next);
                    fetched += 1;
                }
                None => return,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offer_then_poll() {
        let q = ConcurrentQueue::limited(4);
        assert!(q.is_empty());
        assert!(q.offer(1));
        assert!(q.offer(2));
        assert_eq!(q.poll(), Some(1));
        assert_eq!(q.poll(), Some(2));
        assert_eq!(q.poll(), None);
        assert!(q.is_empty());
    }

    #[test]
    fn offer_fails_when_full() {
        let q = ConcurrentQueue::limited(4); // capacity 4
        for i in 0..4 {
            assert!(q.offer(i));
        }
        assert!(!q.offer(99));
    }

    #[test]
    fn drain_respects_limit() {
        let q = ConcurrentQueue::limited(8);
        for i in 0..5 {
            assert!(q.offer(i));
        }
        let mut buf = Vec::new();
        q.drain(&mut buf, 3);
        assert_eq!(buf, vec![0, 1, 2]);
        assert_eq!(q.poll(), Some(3));
    }
}
