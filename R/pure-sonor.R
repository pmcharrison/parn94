#' Get pure sonorousness
#'
#' Computes the pure sonorousness of a sound, after Parncutt & Strasburger (1994).
#' @param x Object to analyse.
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105).
#' @return Pure sonorousness, a numeric scalar.
#' @rdname pure_sonor
#' @references
#' @export
pure_sonor <- function(x, k_p = parn94_params()$k_p, ...) {
  UseMethod("pure_sonor")
}

#' @rdname pure_sonor
pure_sonor.parn94 <- function(x, k_p = parn94_params()$k_p, ...) {
  k_p * sqrt(sum(x$pure_tone_audibility ^ 2))
}
