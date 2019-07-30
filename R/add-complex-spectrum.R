add_complex_spectrum <- function(x, par) {
  spectrum <- x$pure_spectrum[, c("pitch", "pure_tone_audibility")]
  template <- get_template(par)
  df <- tibble::tibble(
    pitch = seq(from = par$min_midi,
                to = par$max_midi),
    complex_tone_audibility = purrr::map_dbl(.data$pitch,
                                             template_match,
                                             template,
                                             spectrum,
                                             par)
  )
  x$complex_spectrum <- df[df$complex_tone_audibility > 0, ]
  x
}

template_match <- function(fundamental, template, spectrum, par) {
  transposed_template <- tibble::tibble(pitch = template$interval + fundamental,
                                        weight = template$weight)
  df <- merge(transposed_template, spectrum,
              all.x = FALSE, all.y = FALSE)
  ((sum(sqrt(df$weight * df$pure_tone_audibility))) ^ 2) / par$k_t
}

get_template <- function(par) {
  hrep::pi_chord(0) %>%
    {hrep::sparse_pi_spectrum(.,
                              num_harmonics = par$template_num_harmonics,
                              roll_off = par$template_roll_off,
                              round = TRUE)} %>%
    (tibble::as_tibble) %>%
    {magrittr::set_names(., c("interval", "weight"))}
}
