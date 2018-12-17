#' Get free field threshold
#'
#' Returns the free-field threshold (dB SPL) of hearing in quiet for pure tones
#' of given frequencies.
#' This is the minimum sound level at which a pure tone at that frequency
#' will be heard.
#' Corresponds to Equation 2 in \insertCite{Parncutt1994;textual}{parn94}.
#' @param kHz Numeric vector of frequencies in kHz.
#' @return Numeric vector of corresponding free-field thresholds in dB SPL.
#' @references
#' \insertAllCited{}
#' @export
get_free_field_threshold <- function(kHz) {
  3.64 * (kHz ^ -0.8) -
    6.5 * exp(- 0.6 * (kHz - 3.3) ^ 2) +
    (10 ^ (-3)) * (kHz ^ 4)
}

#' Get pure-tone height
#'
#' Returns the pure-tone heights (a.k.a. critical-band rates)
#' of pure tones of given frequencies.
#' Equation 3 in \insertCite{Parncutt1994;textual}{parn94}.
#' @param kHz Numeric vector of frequencies in kHz.
#' @return Numeric vector of corresponding pure-tone heights,
#' with units of equivalent rectangular bandwidths (ERBs).
#' @references
#' \insertAllCited{}
#' @export
get_pure_tone_height <- function(kHz) {
  H1 <- 11.17
  H0 <- 43.0
  f1 <- 0.312
  f2 <- 14.675
  H1 * log((kHz + f1) / (kHz + f2)) + H0
}

#' Get partial masking level
#'
#' Returns the effective reduction in dB of the audible level
#' of a masked pure tone (maskee) on account of a masking pure tone (masker).
#' Equation 4 in \insertCite{Parncutt1994;textual}{parn94}.
#' @param masker_auditory_level Numeric vector of masker auditory levels.
#' @param masker_pure_tone_height Numeric vector of masker pure tone heights.
#' @param maskee_auditory_level Numeric vector of maskee auditory levels.
#' @param maskee_pure_tone_height Numeric vector of maskee pure tone heights.
#' @param k_m Parameter \code{k_m} in \insertCite{Parncutt1994;textual}{parn94}.
#' represents the masking pattern gradient for a pure tone,
#' with units of dB per critical band.
#' Parncutt & Strasburger use a value of 12 in their examples,
#' but imply that 12-18 is a typical range of values for this parameter.
#' @return Matrix where element [i, j] gives the level of masking
#' for masker j on maskee i.
#' @references
#' \insertAllCited{}
#' @export
get_partial_masking_level <- function(masker_auditory_level,
                                      masker_pure_tone_height,
                                      maskee_auditory_level,
                                      maskee_pure_tone_height,
                                      k_m) {
  assertthat::assert_that(
    length(masker_auditory_level) == length(masker_pure_tone_height),
    length(maskee_auditory_level) == length(maskee_pure_tone_height)
  )
  ncol <- length(masker_auditory_level)
  nrow <- length(maskee_auditory_level)
  # Masker matrices
  masker_auditory_level_matrix <- matrix(
    data = rep(masker_auditory_level, each = nrow),
    nrow = nrow, ncol = ncol, byrow = FALSE
  )
  masker_pure_tone_height_matrix <- matrix(
    data = rep(masker_pure_tone_height, each = nrow),
    nrow = nrow, ncol = ncol, byrow = FALSE
  )
  # Maskee matrix
  maskee_pure_tone_height_matrix <- matrix(
    data = rep(maskee_pure_tone_height, each = ncol),
    nrow = nrow, ncol = ncol, byrow = TRUE
  )
  # Result
  masker_auditory_level_matrix -
    k_m * abs(
      masker_pure_tone_height_matrix - maskee_pure_tone_height_matrix
    )
}

#' Get overall masking level
#'
#' Returns overall masking levels for a set of pure tones
#' that are assumed to be playing simultaneously.
#' Corresponds to Parncutt & Strasburger (1994) Equation 5.
#' @param auditory_level Numeric vector of auditory levels
#' @param pure_tone_height Numeric vector of pure tone heights
#' @param k_m See \code{\link{parn94_params}()}.
#' @return Numeric vector of overall masking levels (dB).
#' @references
#' \insertAllCited{}
#' @export
get_overall_masking_level <- function(auditory_level,
                                      pure_tone_height,
                                      k_m) {
  partial_mask_matrix <- get_partial_masking_level(
    masker_auditory_level = auditory_level,
    masker_pure_tone_height = pure_tone_height,
    maskee_auditory_level = auditory_level,
    maskee_pure_tone_height = pure_tone_height,
    k_m = k_m
  )
  # Tones don't mask themselves
  diag(partial_mask_matrix) <- 0
  # Sum over maskers to find the masking for each maskee
  apply(
    partial_mask_matrix, 1,
    function(x) {
      max(c(
        20 * log(sum(10 ^ (x / 20)), base = 10),
        0
      ))
    }
  )
}

#' Get audible level
#'
#' Returns the audible level for set of pure tones subject
#' to a given masking pattern.
#' Corresponds to Equation 6 of \insertCite{Parncutt1994;textual}{parn94}.
#' @param auditory_level Numeric vector of auditory levels for
#' a set of pure tones (dB).
#' @param overall_masking_level Numeric vector of overall masking levels
#' for a set of pure tones (dB).
#' @return Numeric vector of audible levels (dB).
#' @references
#' \insertAllCited{}
#' @export
get_pure_tone_audible_level <- function(auditory_level, overall_masking_level) {
  assertthat::assert_that(length(auditory_level) == length(overall_masking_level))
  pmax(0, auditory_level - overall_masking_level)
}

#' Get audibility
#'
#' Returns the audibility of a set of pure tone components as a
#' function of their audible levels.
#' Corresponds to Equation 7 of \insertCite{Parncutt1994;textual}{parn94}.
#' @param pure_tone_audible_level Numeric vector of audible levels (dB).
#' @param al_0 constant (see Equation 7 of Parncutt & Strasburger (1994)).
#' @return Numeric vector of pure tone audibilities.
#' @references
#' \insertAllCited{}
#' @export
get_pure_tone_audibility <- function(pure_tone_audible_level, al_0) {
  1 - exp(- pure_tone_audible_level / al_0)
}

#' Get tone salience
#'
#' Gets the salience of different tones within a sonority with
#' reference to the combined (complex and pure) spectrum.
#' Salience can be interpreted as the probability of consciously
#' perceiving a given pitch.
#' @param combined_audibility Numeric vector corresponding to the
#' audibilities of each tone in the combined (pure and complex) spectrum.
#' @param k_s Numeric scalar; \insertCite{Parncutt1994;textual}{parn94}
#' set this to 0.5.
#' @return Numeric vector of saliences of the same length as
#' \code{combined_audibility}, giving the salience of each respective tone.
#' @references
#' \insertAllCited{}
#' @export
get_tone_salience <- function(combined_audibility, k_s) {
  if (length(combined_audibility) == 0) {
    numeric()
  } else {
    a_max <- max(combined_audibility)
    m_prime <- sum(combined_audibility) / a_max
    m <- m_prime ^ k_s
    (combined_audibility / a_max) *
      (m / m_prime)
  }
}
