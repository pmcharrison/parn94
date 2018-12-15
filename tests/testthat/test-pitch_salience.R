context("test-pitch_salience")

library(magrittr)

test_that("pitch_salience", {
  x <- .parn94()
  x$combined_spectrum <- data.frame(pitch = 1:5, salience = 1)
  x$par <- list(min_midi = 0, max_midi = 10)
  expect_equal(
    pitch_salience(x) %>% as.numeric,
    c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )
})
