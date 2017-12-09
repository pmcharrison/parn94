context("Parncutt")

test_that("midi_to_kHz maps to appropriate frequencies", {
  expect_equal(get.kHz(midi_to_freq(57, stretched_octave = FALSE)), 0.440)
  expect_equal(get.kHz(midi_to_freq(69, stretched_octave = FALSE)), 0.880)
  expect_equal(get.kHz(midi_to_freq(c(57, 69), stretched_octave = FALSE)), c(0.440, 0.880))
})

test_that("parncutt_pure_tone_height matches figures given in Parncutt & Strasburger (1994)", {
  expect_equal(round(get.erb(parncutt_pure_tone_height(0))), 0)
  expect_equal(round(get.erb(parncutt_pure_tone_height(16))), 36)
})

test_that("Replicate calculation on pg. 101 of Parncutt & Strasburger (1994)", {
  lower <- new("pure_tone",
               frequency = as.kHz(0.4),
               level = as.auditory_level(50))
  higher <- new("pure_tone",
                frequency = as.kHz(0.5),
                level = as.auditory_level(60))

  expect_equal(parncutt_partial_masking_level(masker = lower,
                                              maskee = higher) %>%
                 get.dB_SPL %>% round,
               33)
  expect_equal(parncutt_partial_masking_level(masker = higher,
                                              maskee = lower) %>%
                 get.dB_SPL %>% round,
               43)
})


# In elementary acoustics, the intensity of a tone of level L is given by 10^(L/10) and the amplitude by 10^*(L/20) (relative to specified reference points).
