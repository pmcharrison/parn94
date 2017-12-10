context("Parncutt")

test_that("midi_to_kHz maps to appropriate frequencies", {
  expect_equal(midi_to_freq(57, stretched_octave = FALSE), 0.440)
  expect_equal(midi_to_freq(69, stretched_octave = FALSE), 0.880)
  expect_equal(midi_to_freq(c(57, 69), stretched_octave = FALSE), c(0.440, 0.880))
})

test_that("pure_tone_height matches figures given in Parncutt & Strasburger (1994)", {
  expect_equal(round(pure_tone_height(0)), 0)
  expect_equal(round(pure_tone_height(16)), 36)
})

test_that("Replicate calculation on pg. 101 of Parncutt & Strasburger (1994)", {
  expect_equal(partial_masking_level(
    masker_auditory_level = 50,
    masker_pure_tone_height = pure_tone_height(kHz = 0.4),
    maskee_auditory_level = 60,
    maskee_pure_tone_height = pure_tone_height(kHz = 0.5)
  ) %>% as.numeric %>% round,
  33)
  expect_equal(partial_masking_level(
    maskee_auditory_level = 50,
    maskee_pure_tone_height = pure_tone_height(kHz = 0.4),
    masker_auditory_level = 60,
    masker_pure_tone_height = pure_tone_height(kHz = 0.5)
  ) %>% as.numeric %>% round,
  43)
})

test_that("Check matrix aspects of partial_masking_level", {
  mat <- partial_masking_level(
    masker_auditory_level = c(50, 60),
    masker_pure_tone_height = pure_tone_height(c(0.4, 0.5)),
    maskee_auditory_level = c(50, 60),
    maskee_pure_tone_height = pure_tone_height(c(0.4, 0.5))
  )
  expect_equal(
    mat[1, 2] %>% round, 43
  )
  expect_equal(
    mat[2, 1] %>% round, 33
  )
})

test_that("overall_masking_level", {
  expect_equal(overall_masking_level(
    auditory_level = 50,
    pure_tone_height = pure_tone_height(0.5)
  ), 0)
})


# In elementary acoustics, the intensity of a tone of level L is given by 10^(L/10) and the amplitude by 10^*(L/20) (relative to specified reference points).
