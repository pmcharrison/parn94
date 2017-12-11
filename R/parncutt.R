# Implementing Parncutt's psychoacoustic model as reported in
# Parncutt and Strasburger (1994)

# Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in composition: “Harmonic” progressions of “nonharmonic” sonorities. Perspectives of New Music, 32(2), 88–129.

#' @importFrom magrittr "%>%"
NULL

setClass("pitch_set",
         slots = list(midi_pitches = "numeric"))
#' @export
setMethod(
  f = "initialize",
  signature = "pitch_set",
  definition = function(.Object, midi_pitches) {
    .Object@midi_pitches <- sort(unique(midi_pitches))
    return(.Object)
  }
)

setClass("midi_spectrum",
         slots = list(
           pure_spectrum = "data.frame",
           complex_spectrum = "data.frame",
           combined_spectrum = "data.frame",
           pure_sonorousness = "numeric",
           complex_sonorousness = "numeric",
           multiplicity = "numeric"))
#' @param k_t
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105)
#' @param k_c Parncutt & Strasburger (1994) set this to 0.2 (p. 105)
#' @param k_s Parncutt & Strasburger (1994) set this to 0.5 (p. 106)
setMethod(
  f = "initialize",
  signature = "midi_spectrum",
  definition = function(.Object,
                        midi_pitch,
                        level,
                        keep_inaudible = FALSE,
                        template_num_harmonics = 11, # including fundamental
                        template_roll_off = function(x) 1 / (1 + x),
                        k_t = 3,
                        k_p = 0.5,
                        k_c = 0.2,
                        k_s = 0.5) {
    assertthat::assert_that(
      length(midi_pitch) == length(level),
      !anyDuplicated(midi_pitch)
    )
    .Object@pure_spectrum <- get_pure_spectrum(
      midi_pitch = midi_pitch,
      level = level,
      keep_inaudible = keep_inaudible,
      template_num_harmonics = template_num_harmonics,
      template_roll_off = template_roll_off
    )
    .Object@complex_spectrum <- get_complex_spectrum(
      midi_pitch = .Object@pure_spectrum$midi_pitch,
      pure_tone_audibility = .Object@pure_spectrum$pure_tone_audibility,
      template_num_harmonics = template_num_harmonics,
      template_roll_off = template_roll_off,
      k_t = k_t
    )
    .Object@combined_spectrum <- get_combined_spectrum(
      pure_midi_pitch = .Object@pure_spectrum$midi_pitch,
      pure_tone_audibility = .Object@pure_spectrum$pure_tone_audibility,
      complex_midi_pitch = .Object@complex_spectrum$midi_pitch,
      complex_tone_audibility = .Object@complex_spectrum$complex_tone_audibility,
      k_s = k_s
    )
    .Object@pure_sonorousness <- get_pure_sonorousness(
      pure_tone_audibility = .Object@pure_spectrum$pure_tone_audibility,
      k_p = k_p
    )
    .Object@complex_sonorousness <- get_complex_sonorousness(
      complex_tone_audibility = .Object@complex_spectrum$complex_tone_audibility,
      k_c = k_c
    )
    .Object@multiplicity <- get_multiplicity(
      combined_audibility = .Object@combined_spectrum$combined_audibility,
      k_s = k_s
    )
    return(.Object)
  }
)

get_pure_spectrum <- function(midi_pitch,
                              level,
                              keep_inaudible,
                              template_num_harmonics,
                              template_roll_off) {
  order <- order(midi_pitch, decreasing = FALSE)
  df <- data.frame(midi_pitch = midi_pitch[order],
                   level = level[order])
  df$kHz <- midi_to_freq(df$midi_pitch,
                         stretched_octave = TRUE)
  df$free_field_threshold <- free_field_threshold(kHz = df$kHz)
  df$auditory_level <- pmax(df$level -
                              df$free_field_threshold, 0)
  df$pure_tone_height <- pure_tone_height(kHz = df$kHz)
  df$overall_masking_level <- overall_masking_level(
    auditory_level = df$auditory_level,
    pure_tone_height = df$pure_tone_height
  )
  df$pure_tone_audible_level <- get_pure_tone_audible_level(
    auditory_level = df$auditory_level,
    overall_masking_level = df$overall_masking_level
  )
  df$pure_tone_audibility <- get_pure_tone_audibility(
    pure_tone_audible_level = df$pure_tone_audible_level
  )
  if (!keep_inaudible) {
    df <- df[df$pure_tone_audibility > 0, ]
  }
  df
}

#' @export
setMethod(
  f = "as.data.frame",
  signature = "midi_spectrum",
  definition = function(x, row.names = NULL, optional = FALSE) {
    df <- data.frame(midi_pitch = x@midi_pitch,
                     level = x@level,
                     kHz = x@kHz,
                     free_field_threshold = x@free_field_threshold,
                     auditory_level = x@auditory_level,
                     pure_tone_height = x@pure_tone_height,
                     overall_masking_level = x@overall_masking_level,
                     pure_tone_audible_level = x@pure_tone_audible_level,
                     pure_tone_audibility = x@pure_tone_audibility)
    rownames(df) <- row.names
    df
  }
)

#' Get MIDI spectrum
#'
#' MIDI spectra coerce harmonics to the nearest semitone.
#' @export
setGeneric("get_midi_spectrum",
           valueClass = "midi_spectrum",
           function(object,
                    keep_inaudible = FALSE,
                    level = 60,
                    num_harmonics = 10,
                    roll_off = function(x) 1 / (1 + x)) {
             standardGeneric("get_midi_spectrum")
           })
#' @export
setMethod("get_midi_spectrum", signature("pitch_set"),
          function(object,
                   keep_inaudible = FALSE,
                   level = 60,
                   num_harmonics = 11, # including the fundamental
                   roll_off = function(x) 1 / (1 + x)) {
            template <- get_harmonic_template(
              num_harmonics = num_harmonics,
              level = level, roll_off = roll_off
            )
            spectrum <- new.env()
            for (pitch in object@midi_pitches) {
              # Iterate over every fundamental frequency and add the spectral template
              mapply(function(pitch, level) {
                key <- as.character(pitch)
                spectrum[[key]] <<- if (is.null(spectrum[[key]])) level else {
                  sum_sound_levels(spectrum[[key]], level, coherent = FALSE)
                }
              }, template$pitch + pitch, template$level)
            }
            spectrum <- as.list(spectrum) %>%
              (function(x) new("midi_spectrum",
                               midi_pitch = as.numeric(names(x)),
                               level = as.numeric(unlist(x)),
                               keep_inaudible = keep_inaudible))
            spectrum
          })

#' @param num_harmonics Number of harmonics (including fundamental)
#' @param level Level of the fundamental frequency
#' @param roll_off Function determining the level of the nth harmonic. Default value corresponds to Equation 9 of Parncutt and Strasburger (1994)
#' @export
get_harmonic_template <- function(num_harmonics,
                                  level,
                                  roll_off = function(x) 1 / (1 + x)) {
  harmonic_numbers <- seq(from = 0, length.out = num_harmonics)
  template <- data.frame(pitch = round(12 * log(harmonic_numbers + 1, base = 2)),
                         level = level * do.call(roll_off, list(harmonic_numbers)))
  template
}

#' Sums pairs of sound levels assuming either coherent or incoherent (default) wave superposition
#' @param x The first sound level to be summed in dB (can be vectorised)
#' @param y The second sound level to be summed in dB (can be vectorised)
#' @export
sum_sound_levels <- function(x, y, coherent = FALSE) {
  assertthat::assert_that(
    length(x) == length(y)
  )
  if (coherent) {
    20 * log10(10 ^ (x / 20) + 10 ^ (y / 20))
  } else {
    10 * log10(10 ^ (x / 10) + 10 ^ (y / 10))
  }
}

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

#' Get audible level
#'
#' Returns the audible level for set of pure tones subject to a given masking pattern. Corresponds to Equation 6 of Parncutt & Strasburger (1994).
#' @param auditory_level Numeric vector of auditory levels for a set of pure tones (dB)
#' @param overall_masking_level Numeric vector of overall masking levels for a set of pure tones (dB)
#' @return Numeric vector of audible levels (dB)
#' @export
get_pure_tone_audible_level <- function(auditory_level, overall_masking_level) {
  assertthat::assert_that(length(auditory_level) == length(overall_masking_level))
  pmax(0, auditory_level - overall_masking_level)
}

#' Get audibility
#'
#' Returns the audibility of a set of pure tone components as a function of their audible levels. Corresponds to Equation 7 of Parncutt & Strasburger (1994).
#' @param audible_level Numeric vector of audible levels (dB)
#' @param al_0 constant (see Equation 7 of Parncutt & Strasburger (1994).)
#' @export
get_pure_tone_audibility <- function(pure_tone_audible_level, al_0 = 15) {
  1 - exp(- pure_tone_audible_level / al_0)
}

#' @param k_t corresponds to parameter \eqn{k_t} in Equation 10 of Parncutt & Strasburger (1994)
get_complex_spectrum <- function(midi_pitch, pure_tone_audibility,
                                 template_num_harmonics,
                                 template_roll_off,
                                 k_t) {
  spectrum <- data.frame(midi_pitch = midi_pitch,
                         pure_tone_audibility = pure_tone_audibility)
  template <- get_harmonic_template(num_harmonics = template_num_harmonics,
                                    level = 1,
                                    roll_off = template_roll_off)
  df <- data.frame(midi_pitch = seq(from = min(midi_pitch) - max(template$pitch),
                                    to = max(midi_pitch)),
                   complex_tone_audibility = NA)
  df$complex_tone_audibility <- vapply(
    df$midi_pitch,
    function(pitch) {
      transposed_template <- data.frame(midi_pitch = template$pitch + pitch,
                                        weight = template$level)
      df <- merge(transposed_template, spectrum,
                  all.x = FALSE, all.y = FALSE)
      ((sum(sqrt(df$weight * df$pure_tone_audibility))) ^ 2) / k_t
    }, numeric(1)
  )
  df <- df[df$complex_tone_audibility > 0, ]
  df
}

get_audibility <- function() {

}

#' @param pure_tone_audibility Numeric vector of pure tone audibilities
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105)
get_pure_sonorousness <- function(pure_tone_audibility, k_p) {
  k_p * sqrt(sum(pure_tone_audibility ^ 2))
}

#' @param complex_tone_audibility Numeric vector of complex tone audibilities
#' @param k_c Parncutt & Strasburger (1994) set this to 0.2 (p. 105)
get_complex_sonorousness <- function(complex_tone_audibility, k_c) {
  k_c * max(complex_tone_audibility)
}

#' Represents Equations 14 and 15 from Parncutt & Strasburger (1994)
#' @param combined_audibility Numeric vector of combined audibilities as derived in Equation 11 of Parncutt & Strasburger (1994)
#' @return Numeric scalar corresponding to the multiplicity of the sonority
get_multiplicity <- function(combined_audibility,
                             k_s) {
  a_max <- max(combined_audibility)
  m_prime <- sum(combined_audibility) / a_max
  m <- m_prime ^ k_s
  m
}

#' Represents Equations 11 and 16 in Parncutt & Strasburger (1994)
#' @param pure_midi_pitch Numeric vector of MIDI pitches for the pure spectrum
#' @param pure_tone_audibility Numeric vector of audibilities for the pure spectrum
#' @param pure_midi_pitch Numeric vector of MIDI pitches for the complex spectrum
#' @param pure_tone_audibility Numeric vector of audibilities for the complex spectrum
#' @return \code{data.frame} with columns \code{midi_pitch} and \code{combined_audibility}
get_combined_spectrum <- function(pure_midi_pitch,
                                  pure_tone_audibility,
                                  complex_midi_pitch,
                                  complex_tone_audibility,
                                  k_s) {
  df <- merge(
    data.frame(midi_pitch = pure_midi_pitch,
               pure_tone_audibility = pure_tone_audibility),
    data.frame(midi_pitch = complex_midi_pitch,
               complex_tone_audibility = complex_tone_audibility),
    by = "midi_pitch",
    all = TRUE
  )
  df$combined_audibility <- pmax(df$pure_tone_audibility,
                                 df$complex_tone_audibility,
                                 0, na.rm = TRUE)
  df$salience <- get_tone_salience(
    combined_audibility = df$combined_audibility,
    k_s = k_s
  )
  df$pure_tone_audibility <- NULL
  df$complex_tone_audibility <- NULL
  df
}

get_tone_salience <- function(combined_audibility, k_s) {
  a_max <- max(combined_audibility)
  m_prime <- sum(combined_audibility) / a_max
  m <- m_prime ^ k_s
  (combined_audibility / a_max) *
    (m / m_prime)
}
