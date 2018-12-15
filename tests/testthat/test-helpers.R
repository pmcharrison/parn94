context("test-helpers")

library(magrittr)

test_that("get_pure_tone_height matches figures given in Parncutt & Strasburger (1994)", {
  expect_equal(round(get_pure_tone_height(0)), 0)
  expect_equal(round(get_pure_tone_height(16)), 36)
})

test_that("Replicate calculation on pg. 101 of Parncutt & Strasburger (1994)", {
  expect_equal(get_partial_masking_level(
    masker_auditory_level = 50,
    masker_pure_tone_height = get_pure_tone_height(kHz = 0.4),
    maskee_auditory_level = 60,
    maskee_pure_tone_height = get_pure_tone_height(kHz = 0.5),
    k_m = 12
  ) %>% as.numeric %>% round,
  33)
  expect_equal(get_partial_masking_level(
    maskee_auditory_level = 50,
    maskee_pure_tone_height = get_pure_tone_height(kHz = 0.4),
    masker_auditory_level = 60,
    masker_pure_tone_height = get_pure_tone_height(kHz = 0.5),
    k_m = 12
  ) %>% as.numeric %>% round,
  43)
})

test_that("Check matrix aspects of partial_masking_level", {
  mat <- get_partial_masking_level(
    masker_auditory_level = c(50, 60),
    masker_pure_tone_height = get_pure_tone_height(c(0.4, 0.5)),
    maskee_auditory_level = c(50, 60),
    maskee_pure_tone_height = get_pure_tone_height(c(0.4, 0.5)),
    k_m = 12
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
    pure_tone_height = get_pure_tone_height(0.5),
    k_m = 12
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
    get_pure_tone_audibility(pure_tone_audible_level = 0, al_0 = 15),
    0
  )
  expect_gt(
    get_pure_tone_audibility(pure_tone_audible_level = 20, al_0 = 15),
    0
  )
  expect_gt(
    get_pure_tone_audibility(pure_tone_audible_level = 30, al_0 = 15),
    get_pure_tone_audibility(pure_tone_audible_level = 20, al_0 = 15)
  )
})
