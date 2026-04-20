# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-15
## Date Edited:   2026-02-19
## Description:   Activates renv and loads packages.

# Packages ----------------------------------------------------------------
source("renv/activate.R")
library(tidyverse)
library(data.table)
library(fixest)
library(kableExtra)
library(cobalt)
library(modelsummary)
library(nleqslv)
library(SAScii)
options(modelsummary_factory_default = "kableExtra")
