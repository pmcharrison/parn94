#' Get parameters for Parncutt's model
#'
#' Gets parameters for Parncutt's psychoacoustic model.
#' @param k_t Numeric scalar; parameter from \insertCite{Parncutt1994;textual}{parn94}.
#' @param k_p Numeric scalar; parameter from \insertCite{Parncutt1994;textual}{parn94}.
#' @param k_c Numeric scalar; parameter from \insertCite{Parncutt1994;textual}{parn94}.
#' @param k_s Numeric scalar; parameter from \insertCite{Parncutt1994;textual}{parn94}.
#' @param al_0 Numeric scalar; parameter from \insertCite{Parncutt1994;textual}{parn94}.
#' @param min_midi Numeric scalar; the lowest MIDI pitch considered in the psychoacoustic model.
#' @param max_midi Numeric scalar; the highest MIDI pitch considered in the psychoacoustic model.
#' @references
#' \insertAllCited{}
#' @export
parn94_params <- function(
  unit_amplitude_in_dB = 60,
  template_num_harmonics = 11,
  template_roll_off = 1,
  template_stretched_octave = FALSE,
  k_t = 3,
  k_p = 0.5,
  k_c = 0.2,
  k_s = 0.5,
  k_m = 12,
  al_0 = 15,
  min_midi = 0,
  max_midi = 120,
  auditory_model = TRUE,
  peak_method = "peak"
) {
  as.list(environment())
}
