# Implementing Parncutt's psychoacoustic model as reported in
# Parncutt and Strasburger (1994)

# Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in composition: “Harmonic” progressions of “nonharmonic” sonorities. Perspectives of New Music, 32(2), 88–129.

#' @importFrom magrittr "%>%"
NULL

setClass("sonority_analysis",
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
  signature = "sonority_analysis",
  definition = function(.Object,
                        pitch_midi,
                        level,
                        keep_inaudible = FALSE,
                        template_num_harmonics = 11, # including fundamental
                        template_roll_off = function(x) 1 / (1 + x),
                        k_t = 3,
                        k_p = 0.5,
                        k_c = 0.2,
                        k_s = 0.5,
                        min_midi = 0, max_midi = 120) {
    assertthat::assert_that(
      length(pitch_midi) == length(level),
      !anyDuplicated(pitch_midi)
    )
    .Object@pure_spectrum <- get_pure_spectrum(
      pitch_midi = pitch_midi,
      level = level,
      keep_inaudible = keep_inaudible,
      template_num_harmonics = template_num_harmonics,
      template_roll_off = template_roll_off
    )
    .Object@complex_spectrum <- get_complex_spectrum(
      pitch_midi = .Object@pure_spectrum$pitch_midi,
      pure_tone_audibility = .Object@pure_spectrum$pure_tone_audibility,
      template_num_harmonics = template_num_harmonics,
      template_roll_off = template_roll_off,
      k_t = k_t, min_midi = min_midi, max_midi = max_midi
    )
    .Object@combined_spectrum <- get_combined_spectrum(
      pure_midi_pitch = .Object@pure_spectrum$pitch_midi,
      pure_tone_audibility = .Object@pure_spectrum$pure_tone_audibility,
      complex_midi_pitch = .Object@complex_spectrum$pitch_midi,
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

analyse_sonority <- function(pitch_midi,
                             level_dB = 60,
                             expand_harmonics = TRUE,
                             num_harmonics = 11,
                             harmonic_roll_off = function(x) 1 / (1 + x),
                             k_t = 3,
                             k_p = 0.5,
                             k_c = 0.2,
                             k_s = 0.5,
                             min_midi = 0, max_midi = 120) {
  assertthat::assert_that(
    is.numeric(pitch_midi),
    is.numeric(level_dB),
    is.numeric(num_harmonics), assertthat::is.scalar(num_harmonics),
    is.function(harmonic_roll_off),
    is.numeric(k_t), assertthat::is.scalar(k_t),
    is.numeric(k_p), assertthat::is.scalar(k_p),
    is.numeric(k_c), assertthat::is.scalar(k_c),
    is.numeric(k_s), assertthat::is.scalar(k_s),
    length(level_dB) == 1 || length(level_dB) == length(pitch_midi)
  )
  # Expand level_dB if only one value was provided
  level_dB <- if (length(level_dB) == 1) {
    rep(level_dB, times = length(pitch_midi))
  } else level_dB
  # Expand harmonics if requested
  if (expand_harmonics) {
    tmp <- expand_harmonics(pitch_midi = pitch_midi,
                            level = level_dB,
                            num_harmonics = num_harmonics,
                            roll_off = harmonic_roll_off,
                            min_midi = 0, max_midi = 120)
    pitch_midi <- tmp$pitch_midi
    level_dB <- tmp$level
  }
  # Compute the analysis
  new("sonority_analysis",
      pitch_midi = pitch_midi,
      level = level_dB,
      template_num_harmonics = num_harmonics,
      template_roll_off = harmonic_roll_off,
      k_t = k_t,
      k_p = k_p,
      k_c = k_c,
      k_s = k_s,
      min_midi = min_midi, max_midi = max_midi)
}

#' @export
setGeneric("get_pitch_commonality",
           valueClass = "numeric",
           function(chord_1, chord_2, min_midi = 0, max_midi = 120) {
             standardGeneric("get_pitch_commonality")
           })
setMethod(
  f = "get_pitch_commonality",
  signature = c("sonority_analysis", "sonority_analysis"),
  definition = function(chord_1, chord_2, min_midi = 0, max_midi = 120) {
    cor(get_expanded_salience_vector(chord_1,
                                     min_midi = min_midi, max_midi = max_midi),
        get_expanded_salience_vector(chord_2,
                                     min_midi = min_midi, max_midi = max_midi))
  })
setMethod(
  f = "get_pitch_commonality",
  signature = c("numeric", "numeric"),
  definition = function(chord_1, chord_2, min_midi = 0, max_midi = 120) {
    get_pitch_commonality(
      chord_1 = analyse_sonority(chord_1, min_midi = min_midi, max_midi = max_midi),
      chord_2 = analyse_sonority(chord_2, min_midi = min_midi, max_midi = max_midi),
      min_midi = min_midi, max_midi = max_midi
    )
  }
)

#' @export
get_pitch_distance <- function(sonority_analysis_1,
                               sonority_analysis_2,
                               min_midi = 0, max_midi = 120) {
  s1 <- get_expanded_salience_vector(
    sonority_analysis_1, min_midi = min_midi, max_midi = max_midi
  )
  s2 <- get_expanded_salience_vector(
    sonority_analysis_2, min_midi = min_midi, max_midi = max_midi
  )
  # We define some matrices that will allow us to vectorise our calculation -
  # see Equation 17 of Parncutt & Strasburger to see how this works.
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

get_pure_spectrum <- function(pitch_midi,
                              level,
                              keep_inaudible,
                              template_num_harmonics,
                              template_roll_off) {
  order <- order(pitch_midi, decreasing = FALSE)
  df <- data.frame(pitch_midi = pitch_midi[order],
                   level = level[order])
  df$kHz <- convert_midi_to_freq(df$pitch_midi,
                                 stretched_octave = TRUE)
  df$free_field_threshold <- get_free_field_threshold(kHz = df$kHz)
  df$auditory_level <- pmax(df$level -
                              df$free_field_threshold, 0)
  df$pure_tone_height <- get_pure_tone_height(kHz = df$kHz)
  df$overall_masking_level <- get_overall_masking_level(
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

expand_harmonics <- function(pitch_midi, level,
                             num_harmonics = 11, # including the fundamental
                             roll_off = function(x) 1 / (1 + x),
                             min_midi = 0, max_midi = 120) {
  assertthat::assert_that(
    all.equal(round(pitch_midi), pitch_midi),
    length(level) == length(pitch_midi),
    is.numeric(num_harmonics), assertthat::is.scalar(num_harmonics),
    is.function(roll_off),
    is.numeric(min_midi), assertthat::is.scalar(min_midi),
    round(min_midi) == min_midi,
    is.numeric(max_midi), assertthat::is.scalar(max_midi),
    round(max_midi) == max_midi
  )
  template <- get_harmonic_template(
    num_harmonics = num_harmonics,
    level = 1, roll_off = roll_off
  )
  spectrum <- new.env()
  for (i in seq_along(pitch_midi)) {
    fundamental_pitch <- pitch_midi[i]
    fundamental_level <- level[i]
    # Iterate over every fundamental frequency and add the spectral template
    mapply(function(pitch, level) {
      if (pitch >= min_midi && pitch <= max_midi) {
        key <- as.character(pitch)
        spectrum[[key]] <<- if (is.null(spectrum[[key]])) level else {
          sum_sound_levels(spectrum[[key]], level, coherent = FALSE)
        }
      }
    }, template$pitch + fundamental_pitch, template$level * fundamental_level)
  }
  spectrum <- as.list(spectrum) %>%
    (function(x) data.frame(
      pitch_midi = as.numeric(names(x)),
      level = as.numeric(unlist(x))))
  spectrum
}

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
convert_midi_to_freq <- function(midi, stretched_octave = TRUE) {
  0.44 * (2 ^ ((midi - 57) / if (stretched_octave) 11.9 else 12))
}

#' Get free field threshold
#'
#' Returns the free-field threshold (dB SPL) of hearing in quiet for pure tones of given frequencies. This is the minimum sound level at which a pure tone at that frequency will be heard. Corresponds to Equation 2 in Parncutt & Strasburger (1994).
#' @param kHz Numeric vector of frequencies in kHz
#' @return Numeric vector of corresponding free-field thresholds in dB SPL
#' @export
get_free_field_threshold <- function(kHz) {
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
get_pure_tone_height <- function(kHz) {
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
get_partial_masking_level <- function(masker_auditory_level,
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
get_overall_masking_level <- function(auditory_level,
                                      pure_tone_height,
                                      k_m = 12) {
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
get_complex_spectrum <- function(pitch_midi, pure_tone_audibility,
                                 template_num_harmonics,
                                 template_roll_off,
                                 k_t,
                                 min_midi = 0, max_midi = 120) {
  spectrum <- data.frame(pitch_midi = pitch_midi,
                         pure_tone_audibility = pure_tone_audibility)
  template <- get_harmonic_template(num_harmonics = template_num_harmonics,
                                    level = 1,
                                    roll_off = template_roll_off)
  df <- data.frame(pitch_midi = seq(from = min_midi,
                                    to = max_midi),
                   complex_tone_audibility = NA)
  df$complex_tone_audibility <- vapply(
    df$pitch_midi,
    function(pitch) {
      transposed_template <- data.frame(pitch_midi = template$pitch + pitch,
                                        weight = template$level)
      df <- merge(transposed_template, spectrum,
                  all.x = FALSE, all.y = FALSE)
      ((sum(sqrt(df$weight * df$pure_tone_audibility))) ^ 2) / k_t
    }, numeric(1)
  )
  df <- df[df$complex_tone_audibility > 0, ]
  df
}

#' @param pure_tone_audibility Numeric vector of pure tone audibilities
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105)
#' @export
get_pure_sonorousness <- function(pure_tone_audibility, k_p) {
  k_p * sqrt(sum(pure_tone_audibility ^ 2))
}

#' @param complex_tone_audibility Numeric vector of complex tone audibilities
#' @param k_c Parncutt & Strasburger (1994) set this to 0.2 (p. 105)
#' @export
get_complex_sonorousness <- function(complex_tone_audibility, k_c) {
  k_c * max(complex_tone_audibility)
}

#' Represents Equations 14 and 15 from Parncutt & Strasburger (1994)
#' @param combined_audibility Numeric vector of combined audibilities as derived in Equation 11 of Parncutt & Strasburger (1994)
#' @return Numeric scalar corresponding to the multiplicity of the sonority
#' @export
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
#' @return \code{data.frame} with columns \code{pitch_midi} and \code{combined_audibility}
#' @export
get_combined_spectrum <- function(pure_midi_pitch,
                                  pure_tone_audibility,
                                  complex_midi_pitch,
                                  complex_tone_audibility,
                                  k_s) {
  df <- merge(
    data.frame(pitch_midi = pure_midi_pitch,
               pure_tone_audibility = pure_tone_audibility),
    data.frame(pitch_midi = complex_midi_pitch,
               complex_tone_audibility = complex_tone_audibility),
    by = "pitch_midi",
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

#' @export
get_tone_salience <- function(combined_audibility, k_s) {
  a_max <- max(combined_audibility)
  m_prime <- sum(combined_audibility) / a_max
  m <- m_prime ^ k_s
  (combined_audibility / a_max) *
    (m / m_prime)
}

#' Returns a numeric salience vector where each element describes the salience of a different chromatic pitch. The first element of this vector corresponds to the \code{min_midi} argument, and the last element corresponds to \code{max_midi}.
setGeneric("get_expanded_salience_vector",
           valueClass = "numeric",
           function(x, min_midi, max_midi) {
             standardGeneric("get_expanded_salience_vector")
           })
setMethod(
  f = "get_expanded_salience_vector",
  signature = "sonority_analysis",
  definition = function(x, min_midi, max_midi) {
    get_expanded_salience_vector(x@combined_spectrum,
                                 min_midi, max_midi)
  }
)
setMethod(
  f = "get_expanded_salience_vector",
  signature = "data.frame",
  definition = function(x, min_midi, max_midi) {
    n <- max_midi - min_midi + 1
    vec <- numeric(n)
    vec[x$pitch_midi - min_midi + 1] <- x$salience
    names(vec) <- seq(from = min_midi, to = max_midi)
    vec
  }
)
