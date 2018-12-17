#' Parncutt & Strasburger (1994)
#'
#' This function analyses a sonority using Richard Parncutt's
#' psychoacoustic model of harmony, as described in
#' \insertCite{Parncutt1994;textual}{parn94}.
#' @param x Object to analyse.
#' @param par Parameter list as created by \code{\link{parn94_params}()}.
#' @return An list of class \code{parn94}, comprising the following components:
#' \item{pure_spectrum}{A tibble describing the sonority's pure spectrum.
#' The pure spectrum is a spectral representation of the input sound
#' after auditory masking, but before pattern matching.}
#' \item{pure_spectrum}{A tibble describing the sonority's complex spectrum.
#' The complex spectrum is created from the pure spectrum through
#' harmonic template matching.}
#' \item{combined_spectrum}{A tibble describing the sonority's combined spectrum.
#' The combined spectrum corresponds to the combination of the
#' pure and complex spectra.}
#' \item{par}{A list comprising the parameters used to perform the analysis,
#' as created by \code{\link{parn94_params}()}.}
#' @references
#' \insertAllCited{}
#' @rdname parn94
#' @export
parn94 <- function(x, par = parn94_params()) {
  UseMethod("parn94")
}

#' @rdname parn94
#' @export
parn94.numeric <- function(x, par = parn94_params()) {
  x <- hrep::pi_chord(x)
  parn94(x, par = par)
}

#' @rdname parn94
#' @export
parn94.pi_chord <- function(x, par = parn94_params()) {
  if (!hrep::is.equal_tempered(x)) stop("input must be equal-tempered")
  x <- hrep::pi_sparse_spectrum(x, round = TRUE)
  parn94(x, par = par)
}

#' @rdname parn94
#' @export
parn94.pi_sparse_spectrum <- function(x, par = parn94_params()) {
  x <- preprocess_spectrum(x, par)
  .parn94() %>%
    add_pure_spectrum(x, par) %>%
    add_complex_spectrum(par) %>%
    add_combined_spectrum(par) %>%
    add_par(par)
}

preprocess_spectrum <- function(x, par) {
  if (!hrep::is.equal_tempered(x)) stop("input must be equal-tempered")
  x[hrep::pitch(x) >= par$min_midi &
      hrep::pitch(x) <= par$max_midi, ]
}

add_par <- function(x, par) {
  x$par <- par
  x
}

.parn94 <- function() {
  x <- list()
  class(x) <- "parn94"
  x
}