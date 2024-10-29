
## About this file ----

# Jamie Evans, 29/10/24
# The purpose of this code is to prepare various contextual datasets (primarily at LSOA-level) for use in our Mapping Energy Consumption project. The datasets are mostly from the 2021 Census and have been downloaded into DATA/Contextual/Raw from the ONS website and from Nomisweb. Some datasets are in long format and others are in wide format. We will produce a wide dataset of key variables at LSOA-level.




# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,readr,tidyverse)