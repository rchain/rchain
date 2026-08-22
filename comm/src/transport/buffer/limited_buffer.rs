//! Drop-new bounded message buffer.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/buffer/LimitedBuffer.scala`. The Monix
//! `DropNewBufferedSubscriber` run-loop (backpressure / `Future[Ack]`) collapses to a plain bounded
//! queue with a drop-new overflow policy, matching the ported `ConcurrentQueue`.

use std::sync::atomic::{AtomicBool, Ordering};

use super::concurrent_queue::ConcurrentQueue;

/// A bounded buffer with a drop-new overflow policy (port of `LimitedBuffer[A]`).
pub trait LimitedBuffer<A> {
    /// Try to enqueue `elem`; returns `false` when the buffer is full or complete.
    fn push_next(&self, elem: A) -> bool;
    /// Signal that no more elements will be pushed.
    fn complete(&self);
}

/// The concrete drop-new buffer (port of `DropNewBuffer`).
pub struct DropNewBuffer<A> {
    queue: ConcurrentQueue<A>,
    upstream_complete: AtomicBool,
    downstream_complete: AtomicBool,
}

impl<A> DropNewBuffer<A> {
    pub fn new(buffer_size: usize) -> Self {
        assert!(buffer_size > 0, "bufferSize must be a strictly positive number");
        DropNewBuffer {
            queue: ConcurrentQueue::limited(buffer_size),
            upstream_complete: AtomicBool::new(false),
            downstream_complete: AtomicBool::new(false),
        }
    }
}

impl<A> LimitedBuffer<A> for DropNewBuffer<A> {
    fn push_next(&self, elem: A) -> bool {
        if self.upstream_complete.load(Ordering::SeqCst)
            || self.downstream_complete.load(Ordering::SeqCst)
        {
            false
        } else {
            self.queue.offer(elem)
        }
    }

    fn complete(&self) {
        if !self.upstream_complete.load(Ordering::SeqCst)
            && !self.downstream_complete.load(Ordering::SeqCst)
        {
            self.upstream_complete.store(true, Ordering::SeqCst);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_next_returns_false_when_full() {
        let buffer = DropNewBuffer::new(4);
        for i in 0..4 {
            assert!(buffer.push_next(i));
        }
        assert!(!buffer.push_next(99));
    }

    #[test]
    fn push_next_rejected_after_complete() {
        let buffer = DropNewBuffer::new(4);
        buffer.complete();
        assert!(!buffer.push_next(1));
    }
}
