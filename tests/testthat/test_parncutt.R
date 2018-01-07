context("Parncutt")

test_that("get_pure_tone_height matches figures given in Parncutt & Strasburger (1994)", {
  expect_equal(round(get_pure_tone_height(0)), 0)
  expect_equal(round(get_pure_tone_height(16)), 36)
})

test_that("Replicate calculation on pg. 101 of Parncutt & Strasburger (1994)", {
  expect_equal(get_partial_masking_level(
    masker_auditory_level = 50,
    masker_pure_tone_height = get_pure_tone_height(kHz = 0.4),
    maskee_auditory_level = 60,
    maskee_pure_tone_height = get_pure_tone_height(kHz = 0.5)
  ) %>% as.numeric %>% round,
  33)
  expect_equal(get_partial_masking_level(
    maskee_auditory_level = 50,
    maskee_pure_tone_height = get_pure_tone_height(kHz = 0.4),
    masker_auditory_level = 60,
    masker_pure_tone_height = get_pure_tone_height(kHz = 0.5)
  ) %>% as.numeric %>% round,
  43)
})

test_that("Check matrix aspects of partial_masking_level", {
  mat <- get_partial_masking_level(
    masker_auditory_level = c(50, 60),
    masker_pure_tone_height = get_pure_tone_height(c(0.4, 0.5)),
    maskee_auditory_level = c(50, 60),
    maskee_pure_tone_height = get_pure_tone_height(c(0.4, 0.5))
  )
  expect_equal(
    mat[1, 2] %>% round, 43
  )
  expect_equal(
    mat[2, 1] %>% round, 33
  )
})

test_that("get_overall_masking_level", {
  expect_equal(get_overall_masking_level(
    auditory_level = 50,
    pure_tone_height = get_pure_tone_height(0.5)
  ), 0)
})

test_that("get_pure_tone_audible_level", {
  expect_equal(
    get_pure_tone_audible_level(auditory_level = 20, overall_masking_level = 10),
    10
  )
  expect_equal(
    get_pure_tone_audible_level(auditory_level = 30, overall_masking_level = 50),
    0
  )
})

test_that("get_pure_tone_audibility", {
  expect_equal(
    get_pure_tone_audibility(pure_tone_audible_level = 0),
    0
  )
  expect_gt(
    get_pure_tone_audibility(pure_tone_audible_level = 20),
    0
  )
  expect_gt(
    get_pure_tone_audibility(pure_tone_audible_level = 30),
    get_pure_tone_audibility(pure_tone_audible_level = 20)
  )
})

test_that("sum_sound_levels", {
  expect_equal(sum_sound_levels(60, 60) %>% round, 63)
})

test_that("get_expanded_salience_vector", {
  expect_equal(
    get_expanded_salience_vector(data.frame(pitch_midi = 1:5, salience = 1),
                                 min_midi = 0, max_midi = 10) %>% as.numeric,
    c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )
})

test_that("get_pitch_distance", {
  expect_equal(
    get_pitch_distance(c(60, 64, 67), c(60, 64, 67)),
    0
  )
})
