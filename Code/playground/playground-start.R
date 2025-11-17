################################################################################
####################### playground start ##########################
################################################################################

############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 2025-11-16
#
#
# Script Description: 
#       This script is starting point of project to create funcitons.
#
#
# Last Updated: 2025-11-16
#
#
# Notes:
#   To-Do
#     - Add additional visualizations for DE metrics
#     - Review facet labeling consistency across plots
#
#   Done:
#     - Added bias and coverage plots for individual- and cluster-average TNIE
#     - Saved all figures to /Figures directory
#     - Generated summary tables and exported to CSV
# 
################################################################################

# Load packages & functions ----------------------------------------------------

# ══════════════════════════════
#    Load packages 
# ══════════════════════════════
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # Packages 
  doParallel, 
  foreach,
  parallel, 
  purrr, # for map()
  glue, # for glue()
  dplyr, 
  readr, 
  ggplot2, 
  fastDummies, # for (specifically, for dummy_cols() in fastDummies package)
  stringr, # for str_detect() 
  tibble # for rownames_to_column() 
)

# Load data generation function 
devtools::install_github("cameronmccann/simMed")

# 
# # Set date, reps, & folders ----------------------------------------------
# 
# # Date of simulation
# sim_date <- "2025-09-03" 







