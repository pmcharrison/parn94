library(magrittr)

lerdahl_1996 <- read.csv("inst/extdata/lerdahl-1996-data.csv", stringsAsFactors = FALSE)
lerdahl_1996_reference_chord <- read.csv("inst/extdata/lerdahl-1996-reference-chord.csv",
                                         stringsAsFactors = FALSE)

convert_chord_type_to_pc_set <- function(chord_type) {
  assertthat::assert_that(
    assertthat::is.scalar(chord_type),
    is.character(chord_type) || is.factor(chord_type)
  )
  if (chord_type == "diminished") {
    c(0, 3, 6)
  } else if (chord_type == "major") {
    c(0, 4, 7)
  } else if (chord_type == "minor") {
    c(0, 3, 7)
  } else if (chord_type == "minor_seventh") {
    c(0, 3, 7, 10)
  } else if (chord_type == "revoiced") {
    c(0, 4, 7)
  } else if (chord_type == "seventh") {
    c(0, 4, 7, 10)
  } else stop("Unrecognised chord type")
}

lerdahl_1996$root_pc <- lerdahl_1996$bass_interval_size # because all chords were root position
lerdahl_1996$pc_set <- mapply(function(root_pc, chord_type) {
  convert_chord_type_to_pc_set(chord_type) %>%
    add(root_pc) %>%
    mod(., 12) %>%
    sort
}, lerdahl_1996$root_pc, lerdahl_1996$chord_type) %>% I
