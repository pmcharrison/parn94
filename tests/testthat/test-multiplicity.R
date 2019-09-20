context("test-multiplicity")

test_that("testing against legacy code", {
  test <- function(x, y, ...) {
    expect_equal(multiplicity(x, ...), y, tolerance = 1e-5)
  }

  test(c(60, 64, 67), 2.843946)
  test(c(60, 61, 62), 1.293558)
  test(c(80, 83, 86), 3.242737)
  test(c(40, 54, 67), 2.220517)

  test(c(60, 64, 67), 2.835933, par = parn94_params(unit_amplitude_in_dB = 50))
  test(c(61, 62, 63), 1.293558, par = parn94_params(unit_amplitude_in_dB = 70))

  test(hrep::sparse_pi_spectrum(c(60, 64, 67), roll_off = 2, digits = 0),
       2.899631)

  # HarmonyParncutt::get_parncutt_sonority_analysis(
  #   c(60, 64, 67),
  #   # amplitude = 70,
  #   # parncutt_params = HarmonyParncutt::get_parncutt_params(),
  #   midi_params = HarmonyParncutt::get_midi_params(roll_off = 2),
  #   cache = FALSE)$multiplicity
})
