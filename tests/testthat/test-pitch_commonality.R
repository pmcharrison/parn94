context("test-pitch_commonality")

test_that("pitch_commonality", {
  expect_equal(
    pitch_commonality(c(60, 64, 67), c(60, 64, 67)),
    1
  )
  expect_gt(
    # G major should be closer to C major than F# major is to C major
    pitch_commonality(c(60, 64, 67), c(59, 62, 67)),
    pitch_commonality(c(60, 64, 67), c(61, 66, 68))
  )
  expect_gt(
    # G major vs C# major
    pitch_commonality(c(60, 64, 67), c(59, 62, 67)),
    pitch_commonality(c(60, 64, 67), c(61, 65, 68))
  )
  expect_gt(
    # G major vs C transposed
    pitch_commonality(c(60, 64, 67), c(48, 76, 79)),
    pitch_commonality(c(60, 64, 67), c(59, 62, 67))
  )
  # These numbers are taken from previous versions of this package,
  # and have not been compared to other literature/software
  expect_equal(
    pitch_commonality(c(60, 64, 67), c(48, 76, 79)),
    0.894901857522212,
    tolerance = 1e-4
  )
  expect_equal(
    pitch_commonality(c(60, 64, 67), c(59, 62, 67)),
    0.349625432417314
  )
})
