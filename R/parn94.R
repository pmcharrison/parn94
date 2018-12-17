#' @export
parn94 <- function(x, par = parn94_params()) {
  UseMethod("parn94")
}

#' @export
parn94.numeric <- function(x, par = parn94_params()) {
  x <- hrep::pi_chord(x)
  parn94(x, par = par)
}

#' @export
parn94.pi_chord <- function(x, par = parn94_params()) {
  if (!hrep::is.equal_tempered(x)) stop("input must be equal-tempered")
  x <- hrep::pi_sparse_spectrum(x, round = TRUE)
  parn94(x, par = par)
}

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
