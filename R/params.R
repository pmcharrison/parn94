#' @export
get_parncutt_params <- function(
  level_dB = 60,
  num_harmonics = 11,
  harmonic_roll_off = function(x) 1 / (1 + x),
  k_t = 3,
  k_p = 0.5,
  k_c = 0.2,
  k_s = 0.5,
  stretched_octave = TRUE,
  min_midi = 0,
  max_midi = 120
) {
  list(
    level_dB = level_dB,
    num_harmonics = num_harmonics,
    harmonic_roll_off = harmonic_roll_off,
    k_t = k_t,
    k_p = k_p,
    k_c = k_c,
    k_s = k_s,
    stretched_octave = stretched_octave,
    min_midi = min_midi,
    max_midi = max_midi
  )
}
