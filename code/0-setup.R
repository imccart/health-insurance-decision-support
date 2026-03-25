# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-02-15
## Date Edited:   2026-02-19
## Description:   Activates renv and loads packages.

# renv activation ---------------------------------------------------------
source("renv/activate.R")

# Packages ----------------------------------------------------------------
library(tidyverse)
library(data.table)
library(SAScii)
library(fixest)
library(kableExtra)
library(cobalt)
library(modelsummary)
library(broom)
library(mlogit)
options(modelsummary_factory_default = "kableExtra")
