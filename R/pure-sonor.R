#' Get pure sonorousness
#'
#' Computes the pure sonorousness of a sound, after
#' \insertCite{Parncutt1994;textual}{parn94}.
#' @param x Object to analyse.
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105).
#' @param ... Further parameters to pass to \code{\link{parn94}()}.
#' @return Pure sonorousness, a numeric scalar.
#' @rdname pure_sonor
#' @references
#' \insertAllCited{}
#' @export
pure_sonor <- function(x, k_p = parn94_params()$k_p, ...) {
  UseMethod("pure_sonor")
}

#' @rdname pure_sonor
#' @export
pure_sonor.parn94 <- function(x, k_p = parn94_params()$k_p, ...) {
  k_p * sqrt(sum(x$pure_spectrum$pure_tone_audibility ^ 2))
}

#' @rdname pure_sonor
#' @export
pure_sonor.default <- function(x, k_p = parn94_params()$k_p, ...) {
  x <- parn94(x, ...)
  pure_sonor(x, k_p = k_p)
}
