context("test-complex-sonor")

test_that("testing against legacy code", {
  test <- function(x, y, ...) {
    expect_equal(complex_sonor(x, ...), y, tolerance = 1e-5)
  }

  test(c(60, 64, 67), 0.309965)
  test(c(60, 61, 62), 0.0007320671)
  test(c(80, 83, 86), 0.181884)
  test(c(1, 4, 7), 0)
  test(c(40, 54, 67), 0.44699)

  test(c(60, 64, 67), 0.3045464, par = parn94_params(unit_amplitude_in_dB = 50))
  test(c(61, 62, 63), 0.001448646, par = parn94_params(unit_amplitude_in_dB = 70))

  test(c(60, 64, 67), 0.2271383, par = parn94_params(template_num_harmonics = 5))
  test(c(60, 64, 67), 0.1523711, par = parn94_params(template_roll_off = 2))

  # HarmonyParncutt::get_parncutt_sonority_analysis(
  #   c(60, 64, 67),
  #   parncutt_params = HarmonyParncutt::get_parncutt_params(
  #     template_roll_off = 2),
  #   cache = FALSE)
})
