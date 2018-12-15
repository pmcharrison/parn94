#' Get pitch salience
#'
#' Analyses the pitch salience of a sonority, after
#' \insertCite{Parncutt1994;textual}{parn94}.
#' @param x Object to analyse.
#' @param ... Further arguments to pass to \code{\link{parn94}()}.
#' @return Returns a vector where each element describes
#' the salience of a different chromatic pitch.
#' The first element of this vector corresponds to the
#' \code{min_midi} argument from \code{\link{parn94_params}},
#' and the last element corresponds to the \code{max_midi} argument.
#' @references
#' \insertAllCited{}
#' @rdname pitch_salience
#' @export
pitch_salience <- function(x, ...) {
  UseMethod("pitch_salience")
}

#' @rdname pitch_salience
#' @export
pitch_salience.default <- function(x, ...) {
  x <- parn94(x, ...)
  pitch_salience(x)
}

#' @rdname pitch_salience
#' @export
pitch_salience.parn94 <- function(x, ...) {
  vec <- numeric(x$par$max_midi - x$par$min_midi + 1)
  ind <- x$combined_spectrum$pitch - x$par$min_midi + 1
  val <- x$combined_spectrum$salience
  vec[ind] <- val
  .pitch_salience(vec, x$par$min_midi, x$par$max_midi)
}

.pitch_salience <- function(x, min_midi, max_midi) {
  class(x) <- "pitch_salience"
  attr(x, "min_midi") <- min_midi
  attr(x, "max_midi") <- max_midi
  x
}
