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

test_that("get_expanded_salience_vector", {
  expect_equal(
    get_expanded_salience_vector(data.frame(pitch_midi = 1:5, salience = 1),
                                 min_midi = 0, max_midi = 10) %>% as.numeric,
    c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )
})

test_that("get_parncutt_pitch_distance", {
  expect_equal(
    get_parncutt_pitch_distance(c(60, 64, 67), c(60, 64, 67), cache_outer = FALSE),
    0
  )
  expect_gt(
    # Presumably C# major should be closer in pitch to
    # C major than e.g. F major
    get_parncutt_pitch_distance(c(60, 64, 67), c(65, 69, 72), cache_outer = FALSE),
    get_parncutt_pitch_distance(c(60, 64, 67), c(61, 65, 68), cache_outer = FALSE)
  )
  # These numbers are taken from previous versions of this package,
  # and have not been compared to other literature/software
  expect_equal(
    get_parncutt_pitch_distance(c(60, 64, 67), c(65, 69, 72), cache_outer = FALSE),
    3.86723877405512
  )
  expect_equal(
    get_parncutt_pitch_distance(c(60, 64, 67), c(65, 63, 83), cache_outer = FALSE),
    37.8133050960468
  )
  # It shouldn't make a difference whether you cache or not
  expect_equal(
    get_parncutt_pitch_distance(c(60, 64, 67), c(48, 76, 79), cache_outer = FALSE),
    get_parncutt_pitch_distance(c(60, 64, 67), c(48, 76, 79), cache_outer = TRUE)
  )
})

test_that("get_parncutt_pitch_commonality", {
  expect_equal(
    get_parncutt_pitch_commonality(c(60, 64, 67), c(60, 64, 67), cache_outer = FALSE),
    1
  )
  expect_gt(
    # G major should be closer to C major than F# major is to C major
    get_parncutt_pitch_commonality(c(60, 64, 67), c(59, 62, 67), cache_outer = FALSE),
    get_parncutt_pitch_commonality(c(60, 64, 67), c(61, 66, 68), cache_outer = FALSE)
  )
  expect_gt(
    # G major vs C# major
    get_parncutt_pitch_commonality(c(60, 64, 67), c(59, 62, 67), cache_outer = FALSE),
    get_parncutt_pitch_commonality(c(60, 64, 67), c(61, 65, 68), cache_outer = FALSE)
  )
  expect_gt(
    # G major vs C transposed
    get_parncutt_pitch_commonality(c(60, 64, 67), c(48, 76, 79), cache_outer = FALSE),
    get_parncutt_pitch_commonality(c(60, 64, 67), c(59, 62, 67), cache_outer = FALSE)
  )
  # These numbers are taken from previous versions of this package,
  # and have not been compared to other literature/software
  expect_equal(
    get_parncutt_pitch_commonality(c(60, 64, 67), c(48, 76, 79), cache_outer = FALSE),
    0.894901857522212
  )
  expect_equal(
    get_parncutt_pitch_commonality(c(60, 64, 67), c(59, 62, 67), cache_outer = FALSE),
    0.349625432417314
  )
  # It shouldn't make a difference whether you cache or not
  expect_equal(
    get_parncutt_pitch_commonality(c(60, 64, 67), c(48, 76, 79), cache_outer = FALSE),
    get_parncutt_pitch_commonality(c(60, 64, 67), c(48, 76, 79), cache_outer = TRUE)
  )
})

test_that("style", {
  functions <- lsf.str("package:HarmonyParncutt") %>% as.character
  sapply(functions, function(x) expect_true(checkFun(x)))
})

test_that("get_parncutt_sonority_analysis should be invariant to unit switches", {
  expect_equal(
    get_parncutt_sonority_analysis(
      c(60, 64, 67),
      frequency_scale = "midi",
      cache = FALSE
    ),
    get_parncutt_sonority_analysis(
      c(60, 64, 67) %>% HarmonyUtils::convert_midi_to_freq(),
      frequency_scale = "Hz",
      cache = FALSE
    )
  )
  expect_equal(
    get_parncutt_sonority_analysis(
      c(60, 64, 67),
      amplitude = 1,
      dB = FALSE,
      midi_params = get_midi_params(unit_amplitude_in_dB = 60),
      cache = FALSE
    ),
    get_parncutt_sonority_analysis(
      c(60, 64, 67),
      amplitude = 60,
      dB = TRUE,
      midi_params = get_midi_params(unit_amplitude_in_dB = 60),
      cache = FALSE
    )
  )
})

test_that("get_parncutt_sonority_analysis should be invariant to expansion of amplitude argument", {
  expect_equal(
    get_parncutt_sonority_analysis(
      c(60, 64, 67),
      amplitude = 60,
      dB = TRUE,
      cache = FALSE
    ),
    get_parncutt_sonority_analysis(
      c(60, 64, 67),
      amplitude = c(60, 60, 60),
      dB = TRUE,
      cache = FALSE
    )
  )
  expect_equal(
    get_parncutt_sonority_analysis(
      c(60, 64, 67),
      amplitude = 1,
      dB = FALSE,
      cache = FALSE
    ),
    get_parncutt_sonority_analysis(
      c(60, 64, 67),
      amplitude = c(1, 1, 1),
      dB = FALSE,
      cache = FALSE
    )
  )
})

test_that("Cases where there are too many tones masking each other, producing an empty complex sonority", {
  chord_1 <-  c(57, 60, 61, 62, 64, 65)
  chord_2 <-  c(57, 60, 61, 62, 63, 64, 65)
  expect_equal(
    get_parncutt_sonority_analysis(
      chord_1
    ),
    list(
      pure_sonorousness = 0,
      complex_sonorousness = 0,
      multiplicity = as.numeric(NA)
    )
  )
  expect_equal(
    get_parncutt_pitch_commonality(
      chord_1, chord_2
    ),
    as.numeric(NA)
  )
  expect_equal(
    get_parncutt_pitch_distance(
      chord_1, chord_2
    ),
    as.numeric(NA)
  )
})

clear_cache()
