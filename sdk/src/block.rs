//! Block acquisition interface (port of `sdk/block/BlockRequester.scala`).

/// High-level module for acquiring blocks (port of `BlockRequester[F, B, BId]`; the `F[_]` effect
/// and `fs2.Stream` are simplified to synchronous calls and a `Vec`).
pub trait BlockRequester<B, BId> {
    fn request_block(&self, id: &BId);

    fn response(&self) -> Vec<B>;
}
