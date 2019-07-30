#' Get pitch commonality
#'
#' Gets the pitch commonality between two sonorities, after
#' \insertCite{Parncutt1994;textual}{parn94}.
#'
#' @param x The first sonority to compare, passed to \code{\link{pitch_salience}()}.
#' Typically will be a numeric vector of MIDI pitches.
#'
#' @param y The second sonority to compare, passed to \code{\link{pitch_salience}()}.
#' Typically will be a numeric vector of MIDI pitches.
#'
#' @param ... Further arguments to pass to \code{\link{pitch_salience}()}.
#'
#' @return Pitch commonality, as a numeric scalar.
#'
#' @references
#' \insertAllCited{}
#'
#' @export
pitch_commonality <- function(x, y, ...) {
  s1 <- pitch_salience(x, ...)
  s2 <- pitch_salience(y, ...)
  if (length(s1) == 0L || length(s2) == 0L) return(as.numeric(NA))

  if (attr(s1, "min_midi") != attr(s2, "min_midi") ||
      attr(s1, "max_midi") != attr(s2, "max_midi"))
    stop("x and y must be created with identical 'min_midi' and max_midi' ",
         "parameters")

  if (all(s1 == 0 ) || all(s2 == 0))
    as.numeric(NA) else
      cor(s1, s2)
}
