#' Get multiplicity
#'
#' Computes the multiplicity of a sound, after
#' \insertCite{Parncutt1994;textual}{parn94}.
#' @param x Object to analyse.
#' @param k_s Numeric scalar, parameter from Parncutt & Strasburger (1994).
#' @param ... Further parameters to pass to \code{\link{parn94}()}.
#' @return Multiplicity, a numeric scalar.
#' @rdname multiplicity
#' @references
#' \insertAllCited{}
#' @export
multiplicity <- function(x, k_s = parn94_params()$k_s, ...) {
  UseMethod("multiplicity")
}

#' @rdname multiplicity
#' @export
multiplicity.parn94 <- function(x, k_s = parn94_params()$k_s, ...) {
  audibility <- x$combined_spectrum$combined_audibility
  if (length(audibility) == 0) {
    0
  } else {
    a_max <- max(audibility)
    m_prime <- sum(audibility) / a_max
    m <- m_prime ^ k_s
    m
  }
}

#' @rdname multiplicity
#' @export
multiplicity.default <- function(x, k_s = parn94_params()$k_s, ...) {
  x <- parn94(x, ...)
  multiplicity(x, k_s = k_s)
}
