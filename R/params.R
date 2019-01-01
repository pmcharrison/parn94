#' Model parameters
#'
#' This function compiles parameters for Parncutt's psychoacoustic model.
#' The parameters are defined with reference to
#' \insertCite{Parncutt1994;textual}{parn94}.
#' @param unit_amplitude_in_dB (Numeric scalar) Describes the number of decibels
#' to assign to a spectral component of amplitude 1.
#' By default, amplitude 1 corresponds to the amplitude of each chord pitch's
#' fundamental frequency (though this can be changed by expressing input chords
#' as pitch-class sparse spectra, see \code{\link[hrep]{sparse_pi_spectrum}()}).
#' @param template_num_harmonics (Integerish scalar) Number of harmonics to
#' include in the spectral template, including the fundamental frequency.
#' @param template_roll_off (Numeric scalar) Roll-off rate for the
#' harmonic template. This parameter is passed to
#' \code{\link[hrep]{sparse_pi_spectrum}()}.
#' @param k_t (Numeric scalar) Relative perceptual weighting of complex versus pure tones.
#' @param k_p (Numeric scalar) Scaling coefficient for pure sonorousness.
#' @param k_c (Numeric scalar) Scaling coefficient for complex sonorousness.
#' @param k_s (Numeric scalar) Scaling coefficient for multiplicity.
#' @param k_m (Numeric scalar) Gradient of a pure tone's masking pattern.
#' @param al_0 (Numeric scalar) Audibility saturation rate.
#' @param min_midi (Numeric scalar) Lowest MIDI pitch considered by the model.
#' @param max_midi (Numeric scalar) Highest MIDI pitch considered by the model.
#' @return A list of parameters which can be passed to analysis functions
#' such as \code{\link{parn94}}.
#' @references
#' \insertAllCited{}
#' @export
parn94_params <- function(
  unit_amplitude_in_dB = 60,
  template_num_harmonics = 11,
  template_roll_off = 1,
  k_t = 3,
  k_p = 0.5,
  k_c = 0.2,
  k_s = 0.5,
  k_m = 12,
  al_0 = 15,
  min_midi = 0,
  max_midi = 120
) {
  as.list(environment())
}
