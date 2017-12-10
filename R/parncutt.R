# Implementing Parncutt's psychoacoustic model as reported in
# Parncutt and Strasburger (1994)

# Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in composition: “Harmonic” progressions of “nonharmonic” sonorities. Perspectives of New Music, 32(2), 88–129.

#' @importFrom magrittr "%>%"
NULL

#' Convert MIDI note numbers to frequencies
#'
#' Converts MIDI note numbers to frequencies (kHz), optionally using stretched octaves. Corresponds to Equation 1 of Parncutt & Strasburger (1994).
#' @param midi Numeric vector of MIDI note numbers
#' @param stretched_octave Logical scalar; whether or not to use a stretched octave
#' @return Numeric vector of frequencies in kHz
#' @export
midi_to_freq <- function(midi, stretched_octave = TRUE) {
  0.44 * (2 ^ ((midi - 57) / if (stretched_octave) 11.9 else 12))
}

#' Get free field threshold
#'
#' Returns the free-field threshold (dB SPL) of hearing in quiet for pure tones of given frequencies. This is the minimum sound level at which a pure tone at that frequency will be heard. Corresponds to Equation 2 in Parncutt & Strasburger (1994).
#' @param kHz Numeric vector of frequencies in kHz
#' @return Numeric vector of corresponding free-field thresholds in dB SPL
#' @export
free_field_threshold <- function(kHz) {
  3.64 * (kHz ^ -0.8) -
    6.5 * exp(- 0.6 * (kHz - 3.3) ^ 2) +
    (10 ^ (-3)) * (kHz ^ 4)
}

#' Get pure-tone height
#'
#' Returns the pure-tone heights (a.k.a. critical-band rates) of pure tones of given frequencies.
#' Equation 3 in Parncutt & Strasburger (1994).
#' #' @param kHz Numeric vector of frequencies in kHz
#' @return Numeric vector of corresponding pure-tone heights, with units of equivalent rectangular bandwidths (ERBs)
#' @export
pure_tone_height <- function(kHz) {
  H1 <- 11.17
  H0 <- 43.0
  f1 <- 0.312
  f2 <- 14.675
  H1 * log((kHz + f1) / (kHz + f2)) + H0
}

#' Get partial masking level
#'
#' Returns the effective reduction in dB of the audible level of a masked pure tone (maskee) on account of a masking pure tone (masker).
#' Equation 4 in Parncutt & Strasburger (1994).
#' @param masker_auditory_level Vector?
#' @param masker_pure_tone_height
#' @param maskee_pure_tone_height Vector?
#' @param k_m Parameter \code{k_m} in Parncutt & Strasburger (1994); represents the masking pattern gradient for a pure tone, with units of dB per critical band. Parncutt & Strasburger use a value of 12 in their examples, but imply that 12-18 is a typical range of values for this parameter.
#' @return  Matrix where element [i,j] gives the level of masking for masker j on maskee i.
#' @export
partial_masking_level <- function(masker_auditory_level,
                                  masker_pure_tone_height,
                                  maskee_auditory_level,
                                  maskee_pure_tone_height,
                                  k_m = 12) {
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
  # Maskee matrices
  maskee_auditory_level_matrix <- matrix(
    data = rep(maskee_auditory_level, each = ncol),
    nrow = nrow, ncol = ncol, byrow = TRUE
  )
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
#' Returns overall masking levels for a set of pure tones that are assumed to be playing simultaneously. Corresponds to Parncutt & Strasburger (1994) Equation 5.
#' @param auditory_level Numeric vector of auditory levels
#' @param pure_tone_height Numeric vector of pure tone heights
#' @return Numeric vector of overall masking levels (dB)
#' @export
overall_masking_level <- function(auditory_level, pure_tone_height, k_m = 12) {
  partial_mask_matrix <- partial_masking_level(
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

#### Deprecated ####

#' Ensure that an object's level is an auditory level
#' @examples
#' ensure_level_is_auditory_level(
#'   new("pure_tone", frequency = as.kHz(0.44), level = as.dB_SPL(80))
#' )
# setGeneric("ensure_level_is_auditory_level", valueClass = "pure_tone",
#            function(object) standardGeneric("ensure_level_is_auditory_level"))
# setMethod(
#   "ensure_level_is_auditory_level", signature("pure_tone"),
#   function(object) {
#     if (!is(object@level, "auditory_level")) {
#       threshold <- parncutt_free_field_threshold(freq = object@frequency)
#       object@level <- new(
#         "auditory_level",
#         value = pmax(get.dB_SPL(object@level) - get.dB_SPL(threshold),
#                      0))
#     }
#     object
#   })
#
# ensure_level_is_auditory_level(
#   new("pure_tone", frequency = as.kHz(0.44), level = as.dB_SPL(80))
# )
#

