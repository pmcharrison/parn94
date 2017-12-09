# Implementing Parncutt's psychoacoustic model as reported in
# Parncutt and Strasburger (1994)

# Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in composition: “Harmonic” progressions of “nonharmonic” sonorities. Perspectives of New Music, 32(2), 88–129.

#' @importFrom magrittr "%>%"

parncutt_params <- list(
  k_m = 12 # gradient of the masking pattern of a single pure tone
)

#' Converts MIDI note numbers to frequencies
midi_to_freq <- function(midi, stretched_octave = TRUE) {
  as.kHz(0.44 * (2 ^ ((midi - 57) / if (stretched_octave) 11.9 else 12)))
}

#' Get free field threshold
#' Returns the free-field threshold of hearing in quiet for a pure tone of a given frequency. This is the minimum sound level at which a pure tone at that frequency will be heard. Represents Equation 2 in Parncutt & Strasburger (1994).
#' @export
parncutt_free_field_threshold <- function(freq) {
  kHz <- get.kHz(freq)
  res <- 3.64 * (kHz ^ -0.8) -
    6.5 * exp(- 0.6 * (kHz - 3.3) ^ 2) +
    (10 ^ (-3)) * (kHz ^ 4)
  as.dB_SPL(res)
}

#' Equation 3 in Parncutt & Strasburger (1994).
parncutt_pure_tone_height <- function(freq) {
  kHz <- get.kHz(freq)
  H1 <- 11.17
  H0 <- 43.0
  f1 <- 0.312
  f2 <- 14.675
  as.erb(H1 * log((kHz + f1) / (kHz + f2)) + H0)
}

#' Ensure that an object's level is an auditory level
#' @export
#' @examples
#' ensure_level_is_auditory_level(
#'   new("pure_tone", frequency = as.kHz(0.44), level = as.dB_SPL(80))
#' )
setGeneric("ensure_level_is_auditory_level", valueClass = "pure_tone",
           function(object) standardGeneric("ensure_level_is_auditory_level"))
setMethod(
  "ensure_level_is_auditory_level", signature("pure_tone"),
  function(object) {
    if (!is(object@level, "auditory_level")) {
      threshold <- parncutt_free_field_threshold(freq = object@frequency)
      object@level <- new(
        "auditory_level",
        value = pmax(get.dB_SPL(object@level) - get.dB_SPL(threshold),
                     0))
    }
    object
  })

ensure_level_is_auditory_level(
  new("pure_tone", frequency = as.kHz(0.44), level = as.dB_SPL(80))
)

#' Equation 4 in Parncutt & Strasburger (1994).
#' @export
parncutt_partial_masking_level <- function(masker, maskee) {
  assertthat::assert_that(is(masker, "pure_tone"), is(maskee, "pure_tone"))
  as.dB_SPL(
    get.auditory_level(masker) -
      parncutt_params$k_m * abs(
        get.erb(parncutt_pure_tone_height(masker@frequency)) -
          get.erb(parncutt_pure_tone_height(maskee@frequency))
      )
  )
}
