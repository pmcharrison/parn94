# @param res Where results are stored
# @param x Input spectrum
add_pure_spectrum <- function(res, x, par) {
  y <- tibble::tibble(
    pitch = hrep::pitch(x),
    amplitude = hrep::amp(x),
    kHz = hrep::midi_to_freq(.data$pitch) / 1000,
    level = hrep::amplitude_to_dB(.data$amplitude, par$unit_amplitude_in_dB),
    free_field_threshold = get_free_field_threshold(.data$kHz),
    auditory_level = pmax(.data$level - .data$free_field_threshold, 0),
    pure_tone_height = get_pure_tone_height(.data$kHz),
    overall_masking_level = get_overall_masking_level(.data$auditory_level,
                                                      .data$pure_tone_height,
                                                      k_m = par$k_m),
    pure_tone_audible_level = get_pure_tone_audible_level(.data$auditory_level,
                                                          .data$overall_masking_level),
    pure_tone_audibility = get_pure_tone_audibility(.data$pure_tone_audible_level,
                                                    al_0 = par$al_0)
  )
  res$pure_spectrum <- y[y$pure_tone_audibility > 0, ]
  res
}
