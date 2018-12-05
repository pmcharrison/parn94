# Implementing Parncutt's psychoacoustic model as reported in
# Parncutt and Strasburger (1994)

# Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in composition: “Harmonic” progressions of “nonharmonic” sonorities. Perspectives of New Music, 32(2), 88–129.

#' @importFrom magrittr "%>%"
NULL

#' @importFrom stats cor
NULL

#' @importFrom methods .valueClassTest
NULL

#' @importFrom methods new
NULL

#' @importFrom cacheR clear_cache
#' @name clear_cache
#' @export
NULL

setClassUnion("null_or_df", members = c("NULL", "data.frame"))
setClassUnion("null_or_numeric", members = c("NULL", "numeric"))

#' @export
setClass("sonority_analysis",
         slots = list(
           pure_spectrum = "null_or_df",
           complex_spectrum = "null_or_df",
           combined_spectrum = "null_or_df",
           pure_sonorousness = "numeric",
           complex_sonorousness = "null_or_numeric",
           multiplicity = "null_or_numeric"))

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
  definition = function(
    .Object,
    pitch_midi,
    level,
    midi_params = get_midi_params(),
    parncutt_params = get_parncutt_params()
  ) {
    assertthat::assert_that(
      length(pitch_midi) == length(level),
      !anyDuplicated(pitch_midi)
    )
    .Object@pure_spectrum <- if (parncutt_params$auditory_model) get_pure_spectrum(
      pitch_midi = pitch_midi,
      level = level,
      midi_params = midi_params,
      parncutt_params = parncutt_params
    )
    .Object@complex_spectrum <- get_complex_spectrum(
      pitch_midi = if (parncutt_params$auditory_model)
        .Object@pure_spectrum$pitch_midi else pitch_midi,
      pure_tone_audibility = if (parncutt_params$auditory_model)
        .Object@pure_spectrum$pure_tone_audibility else level,
      midi_params = midi_params,
      parncutt_params = parncutt_params
    )
    .Object@combined_spectrum <- if (parncutt_params$auditory_model) get_combined_spectrum(
      pure_midi_pitch = .Object@pure_spectrum$pitch_midi,
      pure_tone_audibility = .Object@pure_spectrum$pure_tone_audibility,
      complex_midi_pitch = .Object@complex_spectrum$pitch_midi,
      complex_tone_audibility = .Object@complex_spectrum$complex_tone_audibility,
      k_s = parncutt_params$k_s
    )
    .Object@pure_sonorousness <- if (parncutt_params$auditory_model) get_pure_sonorousness(
      pure_tone_audibility = .Object@pure_spectrum$pure_tone_audibility,
      k_p = parncutt_params$k_p
    ) else as.numeric(NA)
    .Object@complex_sonorousness <- get_complex_sonorousness(
      complex_tone_audibility = .Object@complex_spectrum$complex_tone_audibility,
      k_c = parncutt_params$k_c,
      peak_method = parncutt_params$peak_method
    )
    .Object@multiplicity <- get_multiplicity(
      combined_audibility = if (parncutt_params$auditory_model)
        .Object@combined_spectrum$combined_audibility else
          .Object@complex_spectrum$complex_tone_audibility,
      k_s = parncutt_params$k_s
    )
    return(.Object)
  }
)

#' Analyse sonority
#'
#' Analyse a sonority using Parncutt's psychoacoustic models. All frequency components are rounded to the nearest MIDI note number.
#' @param pitch_midi Numeric vector of MIDI pitches in the sonority
#' @param level_dB Numeric vector of sound levels (dB) for these pitches; must either be length 1 or of the same length as \code{pitch_midi}
#' @param expand_harmonics Boolean scalar; whether or not to expand these pitches to include their implied harmonics
#' @param simple Whether or not to provide a simplified model output
#' @export
get_parncutt_sonority_analysis <- function(
  frequency,
  frequency_scale = "midi",
  amplitude = 60,
  dB = TRUE,
  expand_harmonics = TRUE,
  simple = TRUE,
  midi_params = get_midi_params(),
  parncutt_params = get_parncutt_params(),
  cache = TRUE,
  cache_parncutt_sonority_analysis = NULL
) {
  assertthat::assert_that(
    is.numeric(frequency),
    assertthat::is.scalar(frequency_scale),
    frequency_scale %in% c("midi", "Hz")
  )
  # Sort out frequency
  pitch_midi <- (if (frequency_scale == "midi") {
    frequency
  } else {
    HarmonyUtils::convert_freq_to_midi(
      frequency,
      stretched_octave = midi_params$stretched_octave,
      tuning_ref_Hz = midi_params$tuning_ref_Hz
    )
  }) %>% round
  # Sort out amplitude
  level_dB <- (if (dB) {
    amplitude
  } else {
    HarmonyUtils::convert_amplitude_to_dB(
      amplitude,
      unit_amplitude_in_dB = midi_params$unit_amplitude_in_dB
    )
  }) %>% HarmonyUtils::rep_to_match(pitch_midi)
  # Check other inputs
  assertthat::assert_that(
    is.numeric(pitch_midi),
    is.numeric(level_dB),
    length(level_dB) == length(pitch_midi),
    assertthat::is.scalar(simple), is.logical(simple)
  )
  # Expand harmonics if requested
  if (expand_harmonics) {
    tmp <- HarmonyUtils::expand_harmonics(
      frequency = pitch_midi,
      amplitude = level_dB,
      dB = TRUE,
      frequency_scale = "midi",
      num_harmonics = midi_params$num_harmonics,
      roll_off = midi_params$roll_off
    )
    tmp <- tmp[tmp$frequency >= parncutt_params$min_midi &
                 tmp$frequency <= parncutt_params$max_midi, ]
    pitch_midi <- tmp$frequency
    level_dB <- tmp$amplitude
  }
  # Compute the analysis
  res <- new(
    "sonority_analysis",
    pitch_midi = pitch_midi,
    level = level_dB,
    midi_params = midi_params,
    parncutt_params = parncutt_params
  )
  if (simple) {
    list(
      pure_sonorousness = res@pure_sonorousness,
      complex_sonorousness = res@complex_sonorousness,
      multiplicity = res@multiplicity
    )
  } else res
}

#' Get pitch commonality
#'
#' Gets the pitch commonality between two chords.
#' @param chord_1 The first chord to compare
#' @param chord_2 The second chord to compare
#' @param min_midi Numeric scalar; the lowest MIDI pitch considered in the psychoacoustic model
#' @export
setGeneric("get_parncutt_pitch_commonality",
           valueClass = "numeric",
           function(chord_1, chord_2,
                    midi_params = get_midi_params(),
                    parncutt_params = get_parncutt_params(),
                    cache_inner = TRUE,
                    cache_outer = FALSE,
                    cache_parncutt_sonority_analysis = NULL) {
             standardGeneric("get_parncutt_pitch_commonality")
           })
setMethod(
  f = "get_parncutt_pitch_commonality",
  signature = c("sonority_analysis", "sonority_analysis"),
  definition = function(chord_1, chord_2,
                        midi_params = get_midi_params(),
                        parncutt_params = get_parncutt_params(),
                        cache_inner = TRUE,
                        cache_outer = FALSE,
                        cache_parncutt_sonority_analysis = NULL) {
    if (
      chord_1@complex_sonorousness == 0 ||
      chord_2@complex_sonorousness == 0
    ) {
      as.numeric(NA)
    } else {
      cor(
        get_expanded_salience_vector(chord_1,
                                     min_midi = parncutt_params$min_midi,
                                     max_midi = parncutt_params$max_midi),
        get_expanded_salience_vector(chord_2,
                                     min_midi = parncutt_params$min_midi,
                                     max_midi = parncutt_params$max_midi)
      )
    }
  })
setMethod(
  f = "get_parncutt_pitch_commonality",
  signature = c("numeric", "numeric"),
  definition = function(chord_1, chord_2,
                        midi_params = get_midi_params(),
                        parncutt_params = get_parncutt_params(),
                        cache_inner = TRUE,
                        cache_outer = FALSE,
                        cache_parncutt_sonority_analysis = NULL) {
    cacheR::cache(
      fun_name = "get_parncutt_pitch_commonality",
      cache = cache_outer,
      cache_root = "cache",
      cache_dir = "HarmonyParncutt/get_parncutt_pitch_commonality",
      expr = expression({
        list(chord_1, chord_2) %>%
          lapply(function(chord) {
            get_parncutt_sonority_analysis(
              chord,
              expand_harmonics = TRUE,
              midi_params = midi_params,
              parncutt_params = parncutt_params,
              simple = FALSE,
              cache = cache_inner,
              cache_parncutt_sonority_analysis =
                cache_parncutt_sonority_analysis
            )
          }) %>%
          (function(x) {
            get_parncutt_pitch_commonality(
              x[[1]], x[[2]],
              midi_params = midi_params,
              parncutt_params = parncutt_params
            )
          })
      })
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
setGeneric("get_parncutt_pitch_distance",
           valueClass = "numeric",
           function(chord_1, chord_2,
                    midi_params = get_midi_params(),
                    parncutt_params = get_parncutt_params(),
                    cache_inner = TRUE,
                    cache_outer = FALSE,
                    cache_parncutt_sonority_analysis = NULL) {
             standardGeneric("get_parncutt_pitch_distance")
           })
setMethod(
  f = "get_parncutt_pitch_distance",
  signature = c("numeric", "numeric"),
  definition = function(
    chord_1, chord_2,
    midi_params = get_midi_params(),
    parncutt_params = get_parncutt_params(),
    cache_inner = TRUE,
    cache_outer = FALSE,
    cache_parncutt_sonority_analysis = NULL
  ) {
    cacheR::cache(
      fun_name = "get_parncutt_pitch_distance",
      cache = cache_outer,
      cache_root = "cache",
      cache_dir = "HarmonyParncutt/get_parncutt_pitch_distance",
      expr = expression({
        list(chord_1, chord_2) %>%
          lapply(function(chord) {
            get_parncutt_sonority_analysis(
              chord,
              expand_harmonics = TRUE,
              midi_params = midi_params,
              parncutt_params = parncutt_params,
              simple = FALSE,
              cache = cache_inner,
              cache_parncutt_sonority_analysis =
                cache_parncutt_sonority_analysis
            )
          }) %>%
          (function(x) {
            get_parncutt_pitch_distance(
              x[[1]], x[[2]],
              parncutt_params = parncutt_params
            )
          })
      })
    )
  }
)
setMethod(
  f = "get_parncutt_pitch_distance",
  signature = c("sonority_analysis", "sonority_analysis"),
  definition = function(chord_1,
                        chord_2,
                        midi_params = get_midi_params(),
                        parncutt_params = get_parncutt_params(),
                        cache_inner = TRUE,
                        cache_outer = FALSE) {
    if (chord_1@complex_sonorousness == 0 ||
        chord_2@complex_sonorousness == 0) {
      as.numeric(NA)
    } else {
      s1 <- get_expanded_salience_vector(
        chord_1, min_midi = parncutt_params$min_midi, max_midi = parncutt_params$max_midi
      )
      s2 <- get_expanded_salience_vector(
        chord_2, min_midi = parncutt_params$min_midi, max_midi = parncutt_params$max_midi
      )
      # We define some matrices that will allow us to vectorise our calculation -
      # see Equation 17 of Parncutt & Strasburger to see how this works.
      # Element [i, j] of each matrix corresponds to one combination of P / P'
      # in Equation 17.
      dim <- length(s1)
      m1 <- matrix(data = rep(seq(from = parncutt_params$min_midi,
                                  to = parncutt_params$max_midi),
                              each = dim),
                   nrow = dim, byrow = TRUE)
      m2 <- matrix(data = rep(seq(from = parncutt_params$min_midi,
                                  to = parncutt_params$max_midi),
                              each = dim),
                   nrow = dim, byrow = FALSE)
      dist <- abs(m1 - m2)
      s1_mat <- matrix(data = rep(s1, each = dim), nrow = dim, byrow = TRUE)
      s2_mat <- matrix(data = rep(s2, each = dim), nrow = dim, byrow = FALSE)

      sum(s1_mat * s2_mat * dist) -
        sqrt(sum(s1_mat * t(s1_mat) * dist) *
               sum(t(s2_mat) * s2_mat * dist))
    }
  }
)

get_pure_spectrum <- function(
  pitch_midi,
  level,
  midi_params,
  parncutt_params
) {
  order <- order(pitch_midi, decreasing = FALSE)
  df <- data.frame(pitch_midi = pitch_midi[order],
                   level = level[order])
  df$kHz <- HarmonyUtils::convert_midi_to_freq(
    df$pitch_midi,
    stretched_octave = midi_params$stretched_octave
  ) / 1000
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
  df <- df[df$pure_tone_audibility > 0, ]
  df
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
get_complex_spectrum <- function(
  pitch_midi,
  pure_tone_audibility,
  midi_params = get_midi_params(),
  parncutt_params = get_parncutt_params()
) {
  spectrum <- data.frame(
    pitch_midi = pitch_midi,
    pure_tone_audibility = pure_tone_audibility
  )
  template <- HarmonyUtils::get_harmonic_template(
    num_harmonics = parncutt_params$template_num_harmonics,
    amplitude = 1,
    roll_off = parncutt_params$template_roll_off,
    interval_scale = "midi",
    round_midi_intervals = TRUE
  ) %>% HarmonyUtils::rename_columns(c(amplitude = "weight"))
  df <- data.frame(pitch_midi = seq(from = parncutt_params$min_midi,
                                    to = parncutt_params$max_midi),
                   complex_tone_audibility = NA)
  df$complex_tone_audibility <- vapply(
    df$pitch_midi,
    function(pitch) {
      transposed_template <- data.frame(pitch_midi = template$interval + pitch,
                                        weight = template$weight)
      df <- merge(transposed_template, spectrum,
                  all.x = FALSE, all.y = FALSE)
      ((sum(sqrt(df$weight * df$pure_tone_audibility))) ^ 2) / parncutt_params$k_t
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
get_complex_sonorousness <- function(complex_tone_audibility, k_c, peak_method) {
  if (peak_method == "peak") {
    get_complex_sonorousness.peak(complex_tone_audibility, k_c)
  } else if (peak_method == "kl") {
    get_complex_sonorousness.kl(complex_tone_audibility)
  } else if (peak_method == "sum") {
    get_complex_sonorousness.sum(complex_tone_audibility)
  } else stop("unknown peak_method")
}

get_complex_sonorousness.peak <- function(complex_tone_audibility, k_c) {
  if (length(complex_tone_audibility) == 0) {
    0
  } else {
    k_c * max(complex_tone_audibility)
  }
}

#' Get complex sonorousness (KL version)
#'
#' Gets complex sonorousness using Kullback-Leibler divergence
#' instead of peak audibility.
get_complex_sonorousness.kl <- function(complex_tone_audibility) {
  if (length(complex_tone_audibility) == 0) {
    0
  } else {
    # Construct a probability vector, where each bin corresponds to
    # the probability of a discrete event
    probs <- complex_tone_audibility / sum(complex_tone_audibility)
    n <- length(probs)
    uniform_probs <- 1 / n
    non_zero_probs <- probs[probs > 0]
    sum(
      non_zero_probs * log(non_zero_probs / uniform_probs, base = 2)
    )
  }
}

get_complex_sonorousness.sum <- function(complex_tone_audibility) {
  sum(complex_tone_audibility)
}

#' Get multiplicity
#'
#' Gets the multiplicity of a sound from its combined (pure and complex) audibilities.
#' Represents Equations 14 and 15 from Parncutt & Strasburger (1994). Returns NA if there are no non-zero combined audibilities.
#' @param combined_audibility Numeric vector of combined audibilities as derived in Equation 11 of Parncutt & Strasburger (1994)
#' @param k_s Numeric scalar, parameter from Parncutt & Strasburger (1994)
#' @return Numeric scalar corresponding to the multiplicity of the sonority
#' @export
get_multiplicity <- function(combined_audibility,
                             k_s) {
  if (length(combined_audibility) == 0) {
    as.numeric(NA)
  } else {
    a_max <- max(combined_audibility)
    m_prime <- sum(combined_audibility) / a_max
    m <- m_prime ^ k_s
    m
  }
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
