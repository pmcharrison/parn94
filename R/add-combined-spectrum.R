add_combined_spectrum <- function(x, par) {
  df <- merge_spectra(x$pure_spectrum, x$complex_spectrum)
  df$combined_audibility <- pmax(df$pure_tone_audibility,
                                 df$complex_tone_audibility,
                                 0, na.rm = TRUE)
  df$salience <- get_tone_salience(df$combined_audibility, par$k_s)
  x$combined_spectrum <- df[, c("pitch", "combined_audibility", "salience")]
  x
}

merge_spectra <- function(pure_spectrum, complex_spectrum) {
  merge(pure_spectrum[, c("pitch", "pure_tone_audibility")],
        complex_spectrum[, c("pitch", "complex_tone_audibility")],
        by = "pitch",
        all = TRUE)
}
