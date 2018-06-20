#' Get parameters for Parncutt's model
#'
#' Gets parameters for Parncutt's psychoacoustic model
#' @param k_t Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param k_p Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param k_c Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param k_s Numeric scalar; parameter from Parncutt & Strasburger (1994)
#' @param min_midi Numeric scalar; the lowest MIDI pitch considered in the psychoacoustic model
#' @param max_midi Numeric scalar; the highest MIDI pitch considered in the psychoacoustic model
#' @export
get_parncutt_params <- function(
  template_num_harmonics = 11,
  template_roll_off = 1,
  template_stretched_octave = TRUE,
  k_t = 3,
  k_p = 0.5,
  k_c = 0.2,
  k_s = 0.5,
  min_midi = 0,
  max_midi = 120,
  auditory_model = TRUE,
  peak_method = "peak"
) {
  list(
    template_num_harmonics = template_num_harmonics,
    template_roll_off = template_roll_off,
    template_stretched_octave = template_stretched_octave,
    k_t = k_t,
    k_p = k_p,
    k_c = k_c,
    k_s = k_s,
    min_midi = min_midi,
    max_midi = max_midi,
    auditory_model = auditory_model,
    peak_method = peak_method
  )
}

#' Get MIDI params
#'
#' Gets MIDI parameters, filling in defaults
#' @importFrom HarmonyUtils get_midi_params
#' @name get_midi_params
#' @export
#' @examples
#' get_midi_params()
NULL
