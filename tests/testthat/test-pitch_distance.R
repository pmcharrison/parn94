context("test-pitch_distance")

test_that("pitch_distance", {
  expect_equal(
    pitch_distance(c(60, 64, 67), c(60, 64, 67)),
    0
  )
  expect_gt(
    # Presumably C# major should be closer in pitch to
    # C major than e.g. F major
    pitch_distance(c(60, 64, 67), c(65, 69, 72)),
    pitch_distance(c(60, 64, 67), c(61, 65, 68))
  )
  # These numbers are taken from previous versions of this package,
  # and have not been compared to other literature/software
  expect_equal(
    pitch_distance(c(60, 64, 67), c(65, 69, 72)),
    3.86723877405512
  )
  expect_equal(
    pitch_distance(c(60, 64, 67), c(65, 63, 83)),
    37.8133050960468
  )
})
