add_pure_spectrum <- function(res, x, par) {
  y <- tibble::tibble(
    pitch = hrep::pitch(x),
    amplitude = hrep::amp(x),
    kHz = hrep::midi_to_freq(pitch) / 1000,
    level = hrep::amplitude_to_dB(amplitude, par$unit_amplitude_in_dB),
    free_field_threshold = get_free_field_threshold(kHz),
    auditory_level = pmax(level - free_field_threshold, 0),
    pure_tone_height = get_pure_tone_height(kHz),
    overall_masking_level = get_overall_masking_level(auditory_level,
                                                      pure_tone_height,
                                                      k_m = par$k_m),
    pure_tone_audible_level = get_pure_tone_audible_level(auditory_level,
                                                          overall_masking_level),
    pure_tone_audibility <- get_pure_tone_audibility(pure_tone_audible_level,
                                                     al_0 = par$al_0)
  )
  res$y <- y[y$pure_tone_audibility > 0, ]
  res
}
