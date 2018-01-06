# Implementing Parncutt's psychoacoustic model as reported in
# Parncutt and Strasburger (1994)

# Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in composition: “Harmonic” progressions of “nonharmonic” sonorities. Perspectives of New Music, 32(2), 88–129.

#' @importFrom magrittr "%>%"
NULL

#' @export
setClass("sonority_analysis",
         slots = list(
           pure_spectrum = "data.frame",
           complex_spectrum = "data.frame",
           combined_spectrum = "data.frame",
           pure_sonorousness = "numeric",
           complex_sonorousness = "numeric",
           multiplicity = "numeric"))

#' Make new sonority analysis object
#'
#' Make new sonority analysis object
#' @param k_t Parncutt & Strasburger (1994) set this to 3 (p. 104)
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105)
#' @param k_c Parncutt & Strasburger (1994) set this to 0.2 (p. 105)
#' @param k_s Parncutt & Strasburger (1994) set this to 0.5 (p. 106)
#' @return A \code{sonority_analysis} object containing slots describing various aspects of the analysed sonority, including \code{pure_spectrum}, \code{complex_spectrum}, \code{combined_spectrum}, \code{pure_sonorousness}, \code{complex_sonorousness}, and \code{multiplicity}. These features can be accessed using `@`, e.g. `analysis@multiplicity`.
setMethod(
  f = "initialize",
  signature = "sonority_analysis",
  definition = function(.Object,
                        pitch_midi,
                        level,
                        keep_inaudible = FALSE,
                        template_num_harmonics = 11, # including fundamental
                        template_roll_off = function(x) 1 / (1 + x),
                        stretched_octave = TRUE,
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
      template_roll_off = template_roll_off,
      stretched_octave = stretched_octave
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


#' Analyse sonority
#'
#' Analyse a sonority using Parncutt's psychoacoustic models.
#' @param pitch_midi Numeric vector of MIDI pitches in the sonority
#' @param level_dB Numeric vector of sound levels (dB) for these pitches; must either be length 1 or of the same length as \code{pitch_midi}
#' @param expand_harmonics Boolean scalar; whether or not to expand these pitches to include their implied harmonics
#' @param num_harmonics Numeric scalar; if \code{expand_harmonics} is \code{TRUE}, this determines how many harmonics are provided for each pitch, including the fundamental
#' @param harmonic_roll_off Function describing the roll off in sound levels as a function of harmonic number
#' @param stretched_octave Logical scalar; whether or not to use stretched octaves, default from Parncutt & Strasburger (1994) is \code{TRUE}
#' @param k_t Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param k_p Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param k_c Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param k_s Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param min_midi Numeric scalar; the lowest MIDI pitch considered in the psychoacoustic model
#' @param max_midi Numeric scalar; the highest MIDI pitch considered in the psychoacoustic model
#' @export
analyse_sonority <- function(
  pitch_midi,
  level_dB = NULL,
  expand_harmonics = TRUE,
  params = get_parncutt_params()
) {
  # Sort out level_dB
  level_dB <- if (is.null(level_dB)) params$level_dB else level_dB
  level_dB <- if (length(level_dB) == 1) {
    rep(level_dB, times = length(pitch_midi))
  } else level_dB
  # Check other inputs
  assertthat::assert_that(
    is.numeric(pitch_midi),
    is.numeric(level_dB),
    length(level_dB) == length(pitch_midi)
  )
  message(
    "The analysis currently uses a 1/n roll-off in the dB domain, which seems unrealistic. ",
    "We should change this to a 1/n roll-off in the amplitude domain. ",
    "In this context, Parncutt's 1/n roll-off for the audibility template seems wrong, ",
    "because amplitude and audibility have a non-linear relationship. ",
    "I think it would be best to empirically estimate a new template ",
    "by repeatedly sampling from the model at different pitches. "
  )
  message(
    "Expanding harmonics should be shifted to the utils package"
  )
  # Expand harmonics if requested
  if (expand_harmonics) {
    tmp <- expand_harmonics(
      pitch_midi = pitch_midi,
      level = level_dB,
      num_harmonics = params$num_harmonics,
      roll_off = params$harmonic_roll_off,
      min_midi = params$min_midi,
      max_midi = params$max_midi
    )
    pitch_midi <- tmp$pitch_midi
    level_dB <- tmp$level
  }
  # Compute the analysis
  new(
    "sonority_analysis",
    pitch_midi = pitch_midi,
    level = level_dB,
    template_num_harmonics = params$num_harmonics,
    template_roll_off = params$harmonic_roll_off,
    stretched_octave = params$stretched_octave,
    k_t = params$k_t,
    k_p = params$k_p,
    k_c = params$k_c,
    k_s = params$k_s,
    min_midi = params$min_midi,
    max_midi = params$max_midi
  )
}


#' Get pitch commonality
#'
#' Gets the pitch commonality between two chords.
#' @param chord_1 The first chord to compare
#' @param chord_2 The second chord to compare
#' @param min_midi Numeric scalar; the lowest MIDI pitch considered in the psychoacoustic model
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

#' Get pitch distance
#'
#' Gets the pitch distance between two chords.
#' @param chord_1 The first chord to compare
#' @param chord_2 The second chord to compare
#' @param min_midi Numeric scalar; the lowest MIDI pitch considered in the psychoacoustic model
#' @export
setGeneric("get_pitch_distance",
           valueClass = "numeric",
           function(chord_1, chord_2, min_midi = 0, max_midi = 120) {
             standardGeneric("get_pitch_distance")
           })
setMethod(
  f = "get_pitch_distance",
  signature = c("numeric", "numeric"),
  definition = function(chord_1, chord_2, min_midi = 0, max_midi = 120) {
    get_pitch_distance(
      chord_1 = analyse_sonority(chord_1, min_midi = min_midi, max_midi = max_midi),
      chord_2 = analyse_sonority(chord_2, min_midi = min_midi, max_midi = max_midi),
      min_midi = min_midi, max_midi = max_midi
    )
  }
)
setMethod(
  f = "get_pitch_distance",
  signature = c("sonority_analysis", "sonority_analysis"),
  definition = function(chord_1, chord_2, min_midi = 0, max_midi = 120) {
    s1 <- get_expanded_salience_vector(
      chord_1, min_midi = min_midi, max_midi = max_midi
    )
    s2 <- get_expanded_salience_vector(
      chord_2, min_midi = min_midi, max_midi = max_midi
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
)

get_pure_spectrum <- function(pitch_midi,
                              level,
                              keep_inaudible,
                              template_num_harmonics,
                              template_roll_off,
                              stretched_octave) {
  order <- order(pitch_midi, decreasing = FALSE)
  df <- data.frame(pitch_midi = pitch_midi[order],
                   level = level[order])
  df$kHz <- convert_midi_to_freq(df$pitch_midi,
                                 stretched_octave = stretched_octave)
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

#' Get harmonic template
#'
#' Gets a harmonic template.
#' @param num_harmonics Number of harmonics (including fundamental)
#' @param level Level of the fundamental frequency
#' @param roll_off Function determining the level of the nth harmonic. Default value corresponds to Equation 9 of Parncutt and Strasburger (1994)
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
#' @param kHz Numeric vector of frequencies in kHz
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
#' @param masker_auditory_level Numeric vector of masker auditory levels
#' @param masker_pure_tone_height Numeric vector of masker pure tone heights
#' @param maskee_pure_tone_height Numeric vector of maskee pure tone heights
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
#' @return Numeric vector of pure tone audibilities
#' @export
get_pure_tone_audibility <- function(pure_tone_audible_level, al_0 = 15) {
  1 - exp(- pure_tone_audible_level / al_0)
}


#' Get complex spectrum
#'
#' Gets a complex spectrum from a pure spectrum using a pattern-matching algorithm.
#' @param pitch_midi Numeric vector of pure tone MIDI pitches
#' @param pure_tone_audibility Corresponding numeric vector of pure tone audibilities
#' @param template_num_harmonics The number of harmonics to use in the template
#' @param template_roll_off Function to use to compute the level of the nth harmonic in the harmonic template
#' @param k_t corresponds to parameter \eqn{k_t} in Equation 10 of Parncutt & Strasburger (1994)
#' @param min_midi Minimum MIDI pitch considered in the auditory model
#' @param max_midi Maximum MIDI pitch considered in the auditory model
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

#' Get pure sonorousness
#'
#' Computes the pure sonorousness of a sound from its pure tone audibilities.
#' @param pure_tone_audibility Numeric vector of pure tone audibilities
#' @param k_p Parncutt & Strasburger (1994) set this to 0.5 (p. 105)
#' @return Pure sonorousness, a numeric scalar
#' @export
get_pure_sonorousness <- function(pure_tone_audibility, k_p) {
  k_p * sqrt(sum(pure_tone_audibility ^ 2))
}

#' Get complex sonorousness
#'
#' Computes the complex sonorousness of a sound from its complex tone audibilities.
#' @param complex_tone_audibility Numeric vector of complex tone audibilities
#' @param k_c Parncutt & Strasburger (1994) set this to 0.2 (p. 105)
#' @return Complex sonorousness, a numeric scalar
#' @export
get_complex_sonorousness <- function(complex_tone_audibility, k_c) {
  k_c * max(complex_tone_audibility)
}

#' Get multiplicity
#'
#' Gets the multiplicity of a sound from its combined (pure and complex) audibilities.
#' Represents Equations 14 and 15 from Parncutt & Strasburger (1994)
#' @param combined_audibility Numeric vector of combined audibilities as derived in Equation 11 of Parncutt & Strasburger (1994)
#' @param k_s Numeric scalar, parameter from Parncutt & Strasburger (1994)
#' @return Numeric scalar corresponding to the multiplicity of the sonority
#' @export
get_multiplicity <- function(combined_audibility,
                             k_s) {
  a_max <- max(combined_audibility)
  m_prime <- sum(combined_audibility) / a_max
  m <- m_prime ^ k_s
  m
}

#' Get combined spectrum
#'
#' Gets the combined spectrum for a sound from its pure and complex audibilities. Represents Equations 11 and 16 in Parncutt & Strasburger (1994)
#' @param pure_midi_pitch Numeric vector of MIDI pitches for the pure spectrum
#' @param pure_tone_audibility Numeric vector of audibilities for the pure spectrum
#' @param pure_midi_pitch Numeric vector of MIDI pitches for the complex spectrum
#' @param pure_tone_audibility Numeric vector of audibilities for the complex spectrum
#' @param k_s Numeric scalar; parameter from Parncutt & Strasburger (1994)
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

#' Get tone salience
#'
#' Gets the salience of different tones within a sonority with reference to the combined (complex and pure) spectrum. Salience can be interpreted as the probability of consciously perceiving a given pitch.
#' @param combined_audibility Numeric vector corresponding to the audibilities of each tone in the combined (pure and complex) spectrum
#' @param k_s Numeric scalar; Parncutt & Strasburger (1994) set this to 0.5
#' @return Numeric vector of saliences of the same length as \code{combined_audibility}, giving the salience of each respective tone.
#' @export
get_tone_salience <- function(combined_audibility, k_s) {
  a_max <- max(combined_audibility)
  m_prime <- sum(combined_audibility) / a_max
  m <- m_prime ^ k_s
  (combined_audibility / a_max) *
    (m / m_prime)
}


#' Get expanded salience vector
#'
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
