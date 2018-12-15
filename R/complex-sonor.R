#' Get complex sonorousness
#'
#' Computes the complex sonorousness of a sound, after
#' \insertCite{Parncutt1994;textual}{parn94}.
#' @param x Object to analyse.
#' @param k_c Parncutt & Strasburger (1994) set this to 0.2 (p. 105)
#' @param ... Further parameters to pass to \code{\link{parn94}()}.
#' @return Complex sonorousness, a numeric scalar.
#' @rdname complex_sonor
#' @references
#' \insertAllCited{}
#' @export
complex_sonor <- function(x, k_c = parn94_params()$k_c, ...) {
  UseMethod("complex_sonor")
}

#' @rdname complex_sonor
#' @export
complex_sonor.parn94 <- function(x, k_c = parn94_params()$k_c, ...) {
  if (length(x$complex_tone_audibility) == 0)
    0 else
      k_c * max(x$complex_tone_audibility)
}

#' @rdname complex_sonor
#' @export
complex_sonor.default <- function(x, k_c = parn94_params()$k_c, ...) {
  x <- parn94(x, ...)
  complex_sonor(x, k_c = k_c)
}
