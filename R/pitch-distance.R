#' Get pitch distance
#'
#' Gets the pitch distance between two sonorities, after
#' \insertCite{Parncutt1994;textual}{parn94}.
#' @param x The first sonority to compare.
#' @param y The second sonority to compare.
#' @param ... Further arguments to pass to \code{\link{pitch_salience}()}.
#' @return Pitch distance, as a numeric scalar.
#' @references
#' \insertAllCited{}
#' @export
pitch_distance <- function(x, y, ...) {
  s1 <- pitch_salience(x, ...)
  s2 <- pitch_salience(y, ...)
  if (length(s1) == 0L || length(s2) == 0L) return(as.numeric(NA))

  min_midi <- attr(s1, "min_midi")
  max_midi <- attr(s1, "max_midi")
  if (min_midi != attr(s2, "min_midi") ||
      max_midi != attr(s2, "max_midi"))
    stop("x and y must be created with identical 'min_midi' and max_midi' ",
         "parameters")

  # We define some matrices that will allow us to vectorise our calculation -
  # see Equation 17 of Parncutt & Strasburger (1994).
  # Element [i, j] of each matrix corresponds to one combination of P / P'
  # in Equation 17.

  dim <- length(s1)
  m1 <- matrix(data = rep(seq(from = min_midi, to = max_midi), each = dim),
               nrow = dim, byrow = TRUE)
  m2 <- matrix(data = rep(seq(from = min_midi, to = max_midi), each = dim),
               nrow = dim, byrow = FALSE)
  dist <- abs(m1 - m2)
  s1_mat <- matrix(data = rep(s1, each = dim), nrow = dim, byrow = TRUE)
  s2_mat <- matrix(data = rep(s2, each = dim), nrow = dim, byrow = FALSE)

  sum(s1_mat * s2_mat * dist) -
    sqrt(sum(s1_mat * t(s1_mat) * dist) *
           sum(t(s2_mat) * s2_mat * dist))
}
