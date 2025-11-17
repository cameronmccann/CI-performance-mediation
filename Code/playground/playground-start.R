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
#     - try other (other than MC) CI approaches 
#     - Turn estimation & CI approaches into functions
#
#   Done:
#     - 
# 
################################################################################

# Load packages & functions ----------------------------------------------------

# ══════════════════════════════
#    Load packages 
# ══════════════════════════════
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # Packages 
  Matrix,
  MASS, 
  
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




# Generate data -----------------------------------------------------------

# generate 
test_lst <- simMed::generate_data(
  J = 10,
  njrange = c(10, 20),
  Mfamily = "gaussian",
  Yfamily = "gaussian",
  seed = 124578,
  num_x = 3,
  ensure_cluster_positivity = T
)

# Extract df 
data <- test_lst$data





# set parameters ----------------------------------------------------------

# set parameter names
Aname <- "A"
Mname <- "M"
Yname <- "Y"
Sname <- "school"
Xnames <- paste0(rep("X"), seq(1:3))

n_MC <- 1000 
seed_MC <- 123456


# FE with MC CI test run -------------------------------------------------------------

# ══════════════════════════════
#    fit models 
# ══════════════════════════════
# ps model 
## Fixed-Effect
psmod <- glm(
  formula = paste0(Aname, " ~ ", paste0(Xnames, collapse = " + "), " + as.factor(", Sname, ")"),
  family = "binomial",
  data = data
)
data$ps <- predict(psmod, type = "response")
data$ps_logit <- predict(psmod, type = "link")
## IPTW
data <- cbind(data, iptw = with(data,
                                (A / ps) +
                                  (1 - A) / (1 - ps)))

# mediator model 
## Fixed-Effect
med <- glm(formula = paste0(Mname, " ~ ", paste0(c(Aname, Xnames), collapse = " + "), " + as.factor(", Sname, ")"),
           data = data, 
           weights = iptw)

# outcome model 
## Fixed-Effect
out <- glm(formula = paste0(Yname, " ~ ", paste0(c(Mname, Aname, Xnames), collapse = " + "), " + as.factor(", Sname, ")"), 
           data = data, 
           weights = iptw)

## Extract point estimates (a, b, & direct effect) -------------------------
# ══════════════════════════════
#    extract estimates 
# ══════════════════════════════

# a-path
a_path_est = summary(med)$coef[Aname, "Estimate"] 
a_path_se = summary(med)$coef[Aname, "Std. Error"] 
# b-path
b_path_est = summary(out)$coef[Mname, "Estimate"] 
b_path_se = summary(out)$coef[Mname, "Std. Error"] 
# direct effect
direct_est = summary(out)$coef[Aname, "Estimate"] 
direct_se = summary(out)$coef[Aname, "Std. Error"] 

### Point estimates of NDE & NIE --------------------------------------------
NIE_est <- a_path_est * b_path_est
NDE_est <- direct_est


## Monte Carlo confidence intervals ----------------------------------------
set.seed(seed_MC)

# ══════════════════════════════
#    MC CI 
# ══════════════════════════════

# cov for a (from mediator model)
cov_a <- matrix(a_path_se^2, nrow = 1, ncol = 1)
mean_a <- a_path_est

# cov for driect & b (from outcome model)
out_coef_names <- names(coef(out))
idx_A <- which(out_coef_names == Aname)
idx_M <- which(out_coef_names == Mname)

# var covariance matrix
# if (Outcomemodel %in% c("SL", "FE")) {
  vcov_out <- vcov(out)  #
# }
# if (Outcomemodel %in% c("RE", "RE-Mean")) {
#   vcov_out <- out$cov_mat
# }

# store direct & b
out_cov_sub <- vcov_out[c(idx_A, idx_M), c(idx_A, idx_M)]
# means for direct & b (same order as cov)
mean_out_sub <- c(direct_est, b_path_est)

#
library(Matrix)
block_cov <- Matrix::bdiag(cov_a, out_cov_sub) # 3x3 block diagonal
# convert sparse matrix to regular matrix
block_cov <- as.matrix(block_cov)
# combine means
big_mean <- c(mean_a, mean_out_sub)

# draw from 3D distr
library(MASS) #param_draws columns: [ ,1] = a, [ ,2] = direct, [ ,3] = b
param_draws <- MASS::mvrnorm(n_MC, mu = big_mean, Sigma = block_cov)
# compute NDE & NIE
a_draw     <- param_draws[, 1]
direct_draw<- param_draws[, 2]
b_draw     <- param_draws[, 3]

NIE_draw <- a_draw * b_draw
NDE_draw <- direct_draw

# 95% MC confidence intervals
NIE_CI <- stats::quantile(NIE_draw, c(0.025, 0.975))
NDE_CI <- stats::quantile(NDE_draw, c(0.025, 0.975))



# Display estimates & 95% CI
NDE_est; NDE_CI
NIE_est; NIE_CI











# create function  --------------------------------------------------------


estimate <- function(psmodel = "FE", 
                     medmodel = "FE", 
                     outcomemodel = "FE", 
                     data, 
                     Sname, 
                     Xnames, 
                     Aname, 
                     Mname, 
                     Yname,
                     ps_formula = NULL, 
                     med_formula = NULL, 
                     out_formula = NULL) {
  
  # 
  # ═══════════════════
  #    PS Model - NEED TO CHANGE TO MATCH ARGUMENTS ABOVE
  # ═══════════════════
  
  ## Fixed-Effect
  psmod <- glm(
    formula = paste0("A ~ ", paste0(colnames(data)[grep("^X\\d", colnames(data))], collapse = " + "), " + as.factor(school)"),
    family = "binomial",
    data = data
  )
  data$ps <- predict(psmod, type = "response")
  data$ps_logit <- predict(psmod, type = "link")
  ## IPTW
  data <- cbind(data, iptw = with(data,
                                  (A / ps) +
                                    (1 - A) / (1 - ps)))
  
  
}








# OLD FUNC ----------------------------------------------------------------




AnalysisFunc_Sim1c <- function(
    PSmodel = "FE",
    Medmodel = "FE",
    Outcomemodel = "FE",
    data = data, 
    condition = cond, 
    condition_num = cond_num, 
    n_MC = 1000, 
    seed_MC = 123456
) {
  # PS Models ---------------------------------------------------------------
  ## Single-Level
  if (PSmodel == "SL") {
    ## Estimate PS model
    psmod <- glm(
      formula = paste0("t ~ ",
                       paste0(colnames(data)[grep("^x\\d$", colnames(data))],
                              collapse = " + ")),
      family = "binomial",
      data = data
    )
    data$ps <- predict(psmod, type = "response")
    data$ps_logit <- predict(psmod, type = "link")
    ## IPTW
    data <- cbind(data, iptw = with(data,
                                    (t / ps) +
                                      (1 - t) / (1 - ps)))
  }
  ## Fixed-Effect
  if (PSmodel == "FE") {
    ## Estimate PS model
    psmod <- glm(
      formula = paste0("t ~ ",
                       paste0(colnames(data)[grep("^x\\d$", colnames(data))],
                              collapse = " + "),
                       " + as.factor(school)"),
      family = "binomial",
      data = data
    )
    data$ps <- predict(psmod, type = "response")
    data$ps_logit <- predict(psmod, type = "link")
    ## IPTW
    data <- cbind(data, iptw = with(data,
                                    (t / ps) +
                                      (1 - t) / (1 - ps)))
  }
  ## Random-Effect
  if (PSmodel == "RE") {
    ## Estimate PS model
    psmod <- lme4::glmer(
      formula = paste0("t ~ ",
                       paste0(colnames(data)[grep("^x\\d$", colnames(data))],
                              collapse = " + "),
                       " + (1 | school)"),
      family = "binomial",
      data = data
    )
    data$ps <- predict(psmod, type = "response")
    data$ps_logit <- predict(psmod, type = "link")
    ## IPTW
    data <- cbind(data, iptw = with(data,
                                    (t / ps) +
                                      (1 - t) / (1 - ps)))
  }
  ## Random-Effect Mean
  if (PSmodel == "RE-Mean") {
    ### Add mean columns 
    data$x1_mean <- ave(data$x1, data$school, FUN = mean)
    data$x2_mean <- ave(data$x2, data$school, FUN = mean)
    data$x3_mean <- ave(data$x3, data$school, FUN = mean)
    # data <- merge(data, 
    #               aggregate(cbind(x1, x2, x3) ~ school, data = data, FUN = mean), 
    #               by = "school",
    #               suffixes = c("", "_mean"))
    ## Estimate PS model
    psmod <- lme4::glmer(
      formula = paste0("t ~ ", 
                       paste0(colnames(data)[grep("^x\\d+$", colnames(data))], 
                              collapse = " + "), 
                       " + x1_mean + x2_mean + x3_mean + (1 | school)"), 
      family = "binomial", 
      data = data
    )
    data$ps <- predict(psmod, type = "response")
    data$ps_logit <- predict(psmod, type = "link")
    ## IPTW
    data <- cbind(data, iptw = with(data,
                                    (t / ps) +
                                      (1 - t) / (1 - ps)))
  }
  
  # Med models  -------------------------------------------------------------
  ## Single-Level
  if (Medmodel == "SL") {
    med <- glm(formula = m ~ t + x1 + x2 + x3,
               data = data,
               weights = iptw)
  }
  ## Fixed-Effect
  if (Medmodel == "FE") {
    med <- glm(
      formula = m ~ t + x1 + x2 + x3 + as.factor(school),
      data = data,
      weights = iptw
    )
  }
  ## Random-Effect
  if (Medmodel == "RE") {
    ### Add column of just 1s for level-2 weight
    data$L2weight <- 1 #data <- cbind(data, L2weight = rep(1, nrow(data)))
    med <- WeMix::mix(
      formula = m ~ t + x1 + x2 + x3 + (1 | school),
      data = data,
      weights = c("iptw", "L2weight")
    )
  }
  ## Random-Effect Mean
  if (Medmodel == "RE-Mean") {
    ### Add mean columns 
    data$t_mean <- ave(data$t, data$school, FUN = mean)
    # data <- merge(data, 
    #               setNames(aggregate(x = data$t, 
    #                                  by = list(data$school), 
    #                                  FUN = mean), 
    #                        c("school", "t_mean")), 
    #               by = "school")
    ### Add column of just 1s for level-2 weight
    data$L2weight <- 1 #data <- cbind(data, L2weight = rep(1, nrow(data)))
    med <- WeMix::mix(
      formula = m ~ t + t_mean + x1 + x2 + x3 + (1 | school),
      data = data,
      weights = c("iptw", "L2weight")
    )
  }
  
  # Outcome model  ----------------------------------------------------------
  ## Single-Level
  if (Outcomemodel == "SL") {
    out <- glm(
      formula = y ~ m + t + x1 + x2 + x3,
      data = data,
      weights = iptw
    )
  }
  ## Fixed-Effect
  if (Outcomemodel == "FE") {
    out <- glm(
      formula = y ~ m + t + x1 + x2 + x3 + as.factor(school),
      data = data,
      weights = iptw
    )
  }
  ## Random-Effect
  if (Outcomemodel == "RE") {
    ### Add column of just 1s for level-2 weight
    data$L2weight <- 1 #data <- cbind(data, L2weight = rep(1, nrow(data)))
    out <-
      WeMix::mix(
        formula = y ~ m + t + x1 + x2 + x3 + (1 | school),
        data = data,
        weights = c("iptw", "L2weight")
      )
  }
  ## Random-Effect Mean
  if (Outcomemodel == "RE-Mean") {
    ### Add mean columns 
    data$t_mean <- ave(data$t, data$school, FUN = mean)
    data$m_mean <- ave(data$m, data$school, FUN = mean)
    # data <- merge(data, 
    #               setNames(aggregate(x = data$m, 
    #                                  by = list(data$school), 
    #                                  FUN = mean), 
    #                        c("school", "m_mean")), 
    #               by = "school")
    # data <- merge(data, 
    #               setNames(aggregate(x = data$t, 
    #                                  by = list(data$school), 
    #                                  FUN = mean), 
    #                        c("school", "t_mean")), 
    #               by = "school")
    ### Add column of just 1s for level-2 weight
    data$L2weight <- 1 #data <- cbind(data, L2weight = rep(1, nrow(data)))
    out <-
      WeMix::mix(
        formula = y ~ m + m_mean + t + t_mean + x1 + x2 + x3 + (1 | school),
        data = data,
        weights = c("iptw", "L2weight")
      )
  }
  
  
  # Extract point estimates (a, b, & direct effect) -------------------------
  # a-path
  a_path_est = summary(med)$coef["t", "Estimate"] 
  a_path_se = summary(med)$coef["t", "Std. Error"] 
  # b-path
  b_path_est = summary(out)$coef["m", "Estimate"] 
  b_path_se = summary(out)$coef["m", "Std. Error"] 
  # direct effect
  direct_est = summary(out)$coef["t", "Estimate"] 
  direct_se = summary(out)$coef["t", "Std. Error"] 
  
  
  # Point estimates of NDE & NIE --------------------------------------------
  NIE_est <- a_path_est * b_path_est
  NDE_est <- direct_est
}  












