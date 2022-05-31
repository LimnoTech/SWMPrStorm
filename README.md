
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/SWMPrStorm)](https://CRAN.R-project.org/package=SWMPrStorm)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/LimnoTech/SWMPrStorm/workflows/R-CMD-check/badge.svg)](https://github.com/LimnoTech/SWMPrStorm/actions)
<!-- badges: end -->

<!-- logos: start -->
<img src="man/figures/Storm-Story-logo.png" align="center" width = "50%"/>
<!-- logos: end -->

## Overview

The System Wide Monitoring Program
([SWMP](http://nerrs.noaa.gov/RCDefault.aspx?ID=18)) was implemented by
the National Estuarine Research Reserve System
([NERRS](http://nerrs.noaa.gov/)) in 1995 to provide continuous
monitoring data at over 140 continuous monitoring stations in 30
estuaries across the United States. SWMPrStorm (pronounced “swamper
storm”) is an R package that provides functions to organize and analyze
SWMP data on a storm event basis. It is a companion package to
[SWMPr](https://github.com/fawda123/SWMPr) and
[SWMPrExtension](https://github.com/NOAA-OCM/SWMPrExtension/).
Currently, there is no citation for SWMPrStorm

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LimnoTech/SWMPrStorm")
```

## Using the Package

SWMPrStorm adds several functions to existing concepts in SWMPr and
builds upon the “Reporting” concept introduced in SWMPrExtension.

This package and its functions rely heavily on the workspace structure
that is available in the StormStories organization: 
https://github.com/StormStories/SWMPr_Storm_Story_Template 

### Analyze

The core analyses available within the SWMprStorm R package and used to
create the StormStories report fall into two general categories:
timeseries plots, summary tables, and wind roses. There is also an
additional category called “mapping” which contains functions that
generate the storm track map. The analyses, methods, and map in this
section were selected by and approved by a technical advisory committee
composed of staff from a subset of the Southeast Atlantic and Caribbean
NERRs.

Quick Guides are available in the 'doc' directory of the workspace
template: https://github.com/StormStories/SWMPr_Storm_Story_Template 

### Reporting

The reporting concept refers to functions that were specifically
developed for use with NERRS StormStories reporting scripts that are
used to generate the StormStories report. They are included as part of
this package in case users find them useful for their own purposes.
