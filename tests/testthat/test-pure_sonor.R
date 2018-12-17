context("test-pure-sonor")

test_that("testing against legacy code", {
  test <- function(x, y, ...) {
    expect_equal(pure_sonor(x, ...), y, tolerance = 1e-5)
  }

  test(c(60, 64, 67), 0.6157366)
  test(c(60, 61, 62), 0.005490503)
  test(c(80, 83, 86), 0.6535714)
  test(c(1, 4, 7), 0)
  test(c(40, 54, 67), 0.5373542)

  test(c(60, 64, 67), 0.6061362, par = parn94_params(unit_amplitude_in_dB = 50))
  test(c(61, 62, 63), 0.01086485, par = parn94_params(unit_amplitude_in_dB = 70))

  test(hrep::pi_sparse_spectrum(c(60, 64, 67), roll_off = 2, round = TRUE),
       0.6136948)

  # HarmonyParncutt::get_parncutt_sonority_analysis(
  #   c(60, 64, 67),
  #   parncutt_params = HarmonyParncutt::get_parncutt_params(),
  #   midi_params = HarmonyParncutt::get_midi_params(roll_off = 2),
  #   cache = FALSE)$pure_sonorousness
})
