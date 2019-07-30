
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parn94: Implementation of Richard Parncutt’s Psychoacoustic Harmony Algorithms

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/pmcharrison/parn94.svg?branch=master)](https://travis-ci.org/pmcharrison/parn94)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/parn94?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/parn94)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/parn94/badge.svg)](https://coveralls.io/r/pmcharrison/parn94?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2545759.svg)](https://doi.org/10.5281/zenodo.2545759)

`parn94` is an R package that implements Richard Parncutt’s
psychoacoustic harmony algorithms, as described in Parncutt &
Strasburger (1994).

## Installation

You can install the current version of `parn94` from Github by entering
the following commands into R:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/parn94")
```

## Example usage

This package provides a variety of methods for analysing chords in
isolation and in combination.

Most functions can be used with numeric inputs, which will be
interpreted as MIDI note numbers. Methods are also provided for various
chord classes in the `hrep` package, such as `pi_chord`,
`sparse_pi_spectrum`, and so on. These inputs are internally coerced to
`sparse_pi_spectrum` before continuing the analysis.

Key functions include:

  - `pure_sonor()` - returns the *pure sonorousness* of a sonority the
    amount of pitch content in a sonority, corresponding to the
    audibility of its pure tone components after accounting for auditory
    masking.
  - `complex_sonor()` - returns the *complex sonorousness* of a
    sonority, the extent to which a sonority resembles a harmonic
    series.
  - `multiplicity()` - estimates *multiplicity*, how many tones are
    perceived in a sonority.
  - `pitch_commonality()` - estimates the *pitch commonality* of a pair
    of sonorities.
  - `pitch_distance()` - estimates the *pitch distance* between a pair
    of sonorities.

<!-- end list -->

``` r
library(parn94)

c_maj <- c(60, 64, 67) # C major triad
c_dim <- c(60, 63, 66) # C diminished triad
g_maj <- c(59, 62, 67) # G major triad

# Pure sonorousness
pure_sonor(c_maj)
#> [1] 0.6157366
pure_sonor(c_dim)
#> [1] 0.4758778

# Complex sonorousness
complex_sonor(c_maj)
#> [1] 0.309965
complex_sonor(c_dim)
#> [1] 0.147792

# Multiplicity
multiplicity(c_maj)
#> [1] 2.843946
multiplicity(c_dim)
#> [1] 3.192587

# Pitch commonality
pitch_commonality(c_maj, g_maj)
#> [1] 0.3496254
pitch_commonality(c_maj, c_dim)
#> [1] 0.2535183

# Pitch distance
pitch_distance(c_maj, g_maj)
#> [1] 1.495423
pitch_distance(c_maj, c_dim)
#> [1] 1.003077
```

## References

Parncutt, R., & Strasburger, H. (1994). Applying psychoacoustics in
composition: “Harmonic” progressions of “nonharmonic” sonorities.
Perspectives of New Music, 32(2), 88–129.
