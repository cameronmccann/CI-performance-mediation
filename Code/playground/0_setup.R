################################################################################
#######################           Repo Setup            #######################
################################################################################

############################ Script Description ################################
#
# Author: Cameron
# 
# Date Created: 2025-11-16
#
#
# Script Description:  
#   This R script sets up the folder structure for this project.
# 
# Last Updated: 2025-11-16
#
#
# Notes:
# 
################################################################################


# Set Up (Load packages, functions, &/or data) ----------------------------

# # Load Packages 
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   # Packages 
#   tidyverse, 
#   ggplot2, 
#   extrafont, 
#   stringr, 
#   mice, 
#   cobalt, 
#   WeightIt, 
#   boot, 
#   utils, 
#   lme4, 
#   WeMix, 
#   parallel, 
#   kable
# )

# library(glue)
# library(tidyverse)
# library(mvtnorm)
# library(SuperLearner)
# library(origami)
# library(fastDummies)




# Create folders 
if (!dir.exists("Code")) {
  dir.create("Code")
}
if (!dir.exists("Code/playground")) {
  dir.create("Code/playground")
}

if (!dir.exists("Functions")) {
  dir.create("Functions")
}
if (!dir.exists("Output")) {
  dir.create("Output")
}
if (!dir.exists("Application")) {
  dir.create("Application")
}
if (!dir.exists("Output/S1_simulation-output")) {
  dir.create("Output/S1_simulation-output")
}
if (!dir.exists("Output/S1_results")) {
  dir.create("Output/S1_results")
}





