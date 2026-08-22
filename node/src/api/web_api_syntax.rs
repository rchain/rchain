//! Web API syntax helpers (port of `api/WebApiSyntax.scala`).
//!
//! Extension methods that lift `Option`/`Either` into a result carrying the API's exception types.
//! Used by `WebApiImpl`/`AdminWebApiImpl`.

use super::dto::{BlockApiException, SignatureException};

/// Extension methods on `Option` (port of `OptionExt`).
pub trait OptionExt<T> {
    /// Lift into a result, raising `SignatureException(error)` on `None` (port of `liftToSigErr`).
    fn lift_to_sig_err(self, error: &str) -> Result<T, SignatureException>;
}

impl<T> OptionExt<T> for Option<T> {
    fn lift_to_sig_err(self, error: &str) -> Result<T, SignatureException> {
        self.ok_or_else(|| SignatureException(error.to_string()))
    }
}

/// Extension methods on `Result<T, String>` (port of `EitherStringExt` over `Either[String, A]`).
pub trait EitherStringExt<T> {
    /// Lift into a result, converting the `String` error to `BlockApiException`
    /// (port of `liftToBlockApiErr`).
    fn lift_to_block_api_err(self) -> Result<T, BlockApiException>;
}

impl<T> EitherStringExt<T> for Result<T, String> {
    fn lift_to_block_api_err(self) -> Result<T, BlockApiException> {
        self.map_err(BlockApiException)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lift_to_sig_err_raises_on_none() {
        let ok: Option<i32> = Some(42);
        assert_eq!(ok.lift_to_sig_err("boom").unwrap(), 42);

        let none: Option<i32> = None;
        assert_eq!(
            none.lift_to_sig_err("boom").unwrap_err(),
            SignatureException("boom".to_string())
        );
    }

    #[test]
    fn lift_to_block_api_err_maps_the_error() {
        let ok: Result<i32, String> = Ok(7);
        assert_eq!(ok.lift_to_block_api_err().unwrap(), 7);

        let err: Result<i32, String> = Err("bad".to_string());
        assert_eq!(
            err.lift_to_block_api_err().unwrap_err(),
            BlockApiException("bad".to_string())
        );
    }
}
