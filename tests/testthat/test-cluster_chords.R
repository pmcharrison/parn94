context("test-cluster_chords")

library(magrittr)

# These are cases where there are too many tones masking each other,
# producing an empty complex sonority.

test_that("examples", {
  chord_1 <-  c(57, 60, 61, 62, 64, 65)
  chord_2 <-  c(57, 60, 61, 62, 63, 64, 65)

  complex_sonor(chord_1) %>% expect_equal(0)
  pure_sonor(chord_1) %>% expect_equal(0)
  multiplicity(chord_1) %>% expect_equal(0)

  expect_equal(
    pitch_commonality(
      chord_1, chord_2
    ),
    as.numeric(NA)
  )
  expect_equal(
    pitch_distance(
      chord_1, chord_2
    ),
    0
  )
})
