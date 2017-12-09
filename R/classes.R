#' @import methods

## Classes

# Frequency
setClass("frequency", slots = list(value = "numeric"))
setClass("kHz", contains = "frequency")
setClass("Hz", contains = "frequency")

# ERB
setClass("erb", slots = list(value = "numeric"))

#' Level of a sound.
#' @slot value The value of the level attribute (possibly vectorised for multiple sounds)
setClass("level", list(value = "numeric"))
#' Level of a sound in units dB SPL
setClass("dB_SPL", contains = "level")

# Sounds
setClass("complex_tone",
         slots = list(frequencies = "frequency",
                      levels = "level"))
setClass("pure_tone",
         slots = list(frequency = "frequency",
                      level = "level"))
setClass("chord",
         slots = list(bass_pc = "numeric",
                      non_bass_pcs = "numeric"))

## Methods

# Frequency
setAs(from = "numeric", to = "kHz",
      def = function(from) new("kHz", value = from))
setAs(from = "numeric", to = "Hz",
      def = function(from) new("Hz", value = from))
setAs(from = "Hz", to = "kHz",
      def = function(from) new("kHz", value = from@value / 1000))
setAs(from = "kHz", to = "Hz",
      def = function(from) new("Hz", value = from@value * 1000))
#' @export
as.kHz <- function(x) as(x, "kHz")
#' @export
as.Hz <- function(x) as(x, "Hz")
#' @export
get.kHz <- function(x) as.kHz(x)@value
#' @export
get.Hz <- function(x) as.Hz(x)@value

# ERB
setAs(from = "numeric", to = "erb",
      def = function(from) new("erb", value = from))
as.erb <- function(x) as(x, "erb")
get.erb <- function(x) as.erb(x)@value

# Level
setAs(from = "numeric", to = "dB_SPL",
      def = function(from) new("dB_SPL", value = from))
#' @export
as.dB_SPL <- function(x) as(x, "dB_SPL")
#' @export
get.dB_SPL <- function(x) as(x, "dB_SPL")@value
#' Auditory level of a sound
setClass("auditory_level", contains = "level")
#' @export
setAs(from = "numeric", to = "auditory_level",
      def = function(from) new("auditory_level", value = from))
#' @export
as.auditory_level <- function(x) as(x, "auditory_level")
#' @export
setGeneric("get.auditory_level", valueClass = "numeric",
           function(object) standardGeneric("get.auditory_level"))
setMethod("get.auditory_level", signature("pure_tone"),
          function(object) {
            ensure_level_is_auditory_level(object) %>%
              (function(x) x@level) %>%
              (function(x) get.auditory_level(x))
          })
setMethod("get.auditory_level", signature("auditory_level"),
          function(object) object@value)

