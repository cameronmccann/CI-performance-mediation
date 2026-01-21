# estimate_mediation


# Helpers 


## PS helper 
.fit_ps <- function(data, treat, covariates, cluster, ps_model) {
  
  # create code to make sure variable names are in dataset 
  # Add RE-Mean (commented out and not tested)
  
  # Base formula
  ps_formula <- as.character(paste0(treat, " ~ ", 
                                  paste0(covariates, collapse = " + ")))
  # Formula 
  if (ps_model == "FE") {
    ps_formula <- paste0(ps_formula, " + as.factor(", cluster, ")")
  } else if (ps_model == "RE") {
    ps_formula <- paste0(ps_formula, " + (1 | ", cluster, ")")
  } #else if (ps_model == "RE-Mean") {
    ## Add code for RE-Mean
  # }
  
  # create formula 
  ps_formula <- as.formula(ps_formula)
  
  # fit ps model 
  if (ps_model %in% c("SL", "FE")) {
    psmod <- glm(formula = ps_formula, 
                 family = "binomial", 
                 data = data)
  } else if (ps_model %in% c("RE", "RE-Mean")) {
    psmod <- lme4::glmer(formula = ps_formula, 
                         family = "binomial", 
                         data = data)
  }
  
  # PS, PS logit
  data$ps <- predict(psmod, type = "response")
  data$ps_logit <- predict(psmod, type = "link")
  # IPTW
  data$iptw <- (data[[treat]] / data$ps) + (1 - data[[treat]]) / (1 - data$ps)
  
  list(data = data, psmod = psmod)
}

## Mediator helper 
.fit_mediator <- function(data, treat, covariates, mediator, cluster, med_model) {
  
  # Add RE-Mean (commented out and not tested)
  
  # Base formula
  med_formula <- as.character(paste0(mediator, " ~ ", treat, " + ", 
                                     paste0(covariates, collapse = " + ")))
  
  # Formula 
  if (med_model == "FE") {
    med_formula <- paste0(med_formula, " + as.factor(", cluster, ")")
  } else if (med_model == "RE") {
    med_formula <- paste0(med_formula, " + (1 | ", cluster, ")")
  } #else if (med_model == "RE-Mean") {
  ## Add code for RE-Mean
  # }
  
  # create formula 
  med_formula <- as.formula(med_formula)
  
  # Fit mediator model 
  if (med_model %in% c("SL", "FE")) {
    medmod <- glm(formula = med_formula, 
                  data = data, 
                  weights = iptw)
  } else if (med_model == "RE") {
    ### Add column of just 1s for level-2 weight
    data$L2weight <- 1 
    medmod <- WeMix::mix(formula = med_formula, 
                         data = data, 
                         weights = c("iptw", "L2weight"))
    
  } #else if (med_model == "RE-Mean") {
    # ### Add mean columns 
    # data$t_mean <- ave(data$t, data$school, FUN = mean)
    # # data <- merge(data, 
    # #               setNames(aggregate(x = data$t, 
    # #                                  by = list(data$school), 
    # #                                  FUN = mean), 
    # #                        c("school", "t_mean")), 
    # #               by = "school")
    # ### Add column of just 1s for level-2 weight
    # data$L2weight <- 1 #data <- cbind(data, L2weight = rep(1, nrow(data)))
    # med <- WeMix::mix(
    #   formula = m ~ t + t_mean + x1 + x2 + x3 + (1 | school),
    #   data = data,
    #   weights = c("iptw", "L2weight")
    # )

  list(data = data, medmod = medmod)
}

## Outcome helper 
.fit_outcome <- function(data, treat, covariates, mediator, outcome, cluster, out_model) {
  
  # Base formula
  out_formula <- as.character(paste0(outcome, " ~ ", treat, " + ", mediator, " + ", 
                                     paste0(covariates, collapse = " + ")))
  
  # Formula 
  if (out_model == "FE") {
    out_formula <- paste0(out_formula, " + as.factor(", cluster, ")")
  } else if (out_model == "RE") {
    out_formula <- paste0(out_formula, " + (1 | ", cluster, ")")
  } #else if (out_model == "RE-Mean") {
  ## Add code for RE-Mean
  # }
  
  # create formula 
  out_formula <- as.formula(out_formula)
  
  # Fit mediator model 
  if (out_model %in% c("SL", "FE")) {
    outmod <- glm(formula = out_formula, 
                  data = data, 
                  weights = iptw)
  } else if (out_model == "RE") {
    ### Add column of just 1s for level-2 weight
    data$L2weight <- 1 
    outmod <- WeMix::mix(formula = out_formula, 
                         data = data, 
                         weights = c("iptw", "L2weight"))
    
  } #else if (out_model == "RE-Mean") {
  #   ### Add mean columns 
  #   data$t_mean <- ave(data$t, data$school, FUN = mean)
  #   data$m_mean <- ave(data$m, data$school, FUN = mean)
  #   # data <- merge(data, 
  #   #               setNames(aggregate(x = data$m, 
  #   #                                  by = list(data$school), 
  #   #                                  FUN = mean), 
  #   #                        c("school", "m_mean")), 
  #   #               by = "school")
  #   # data <- merge(data, 
  #   #               setNames(aggregate(x = data$t, 
  #   #                                  by = list(data$school), 
  #   #                                  FUN = mean), 
  #   #                        c("school", "t_mean")), 
  #   #               by = "school")
  #   ### Add column of just 1s for level-2 weight
  #   data$L2weight <- 1 #data <- cbind(data, L2weight = rep(1, nrow(data)))
  #   out <-
  #     WeMix::mix(
  #       formula = y ~ m + m_mean + t + t_mean + x1 + x2 + x3 + (1 | school),
  #       data = data,
  #       weights = c("iptw", "L2weight")
  #     )
  # }
  
  list(data = data, outmod = outmod)
}

# Estimation function 
estimate_mediation <- function(data, ps_model, med_model, out_model, treat, covariates, mediator, outcome, cluster) {
  
  # PS 
  ps_res <- .fit_ps(data, treat, covariates, cluster, ps_model)
  # Mediator
  med_res <- .fit_mediator(data, treat, covariates, mediator, cluster, med_model)
  # Outcome 
  out_res <- .fit_outcome(data, treat, covariates, mediator, outcome, cluster, out_model)
  
  # Point estimates
  a_est <- med_res$medmod$coefficients[[treat]]
  b_est <- out_res$outmod$coefficients[[mediator]]
  c_est <- out_res$outmod$coefficients[[treat]]
  
  # # SE 
  # a_se <- summary(med_res$medmod)$coef[treat, "Std. Error"]
  # b_se <- summary(out_res$outmod)$coef[mediator, "Std. Error"]
  # c_se <- summary(out_res$outmod)$coef[treat, "Std. Error"]
  
  list(
    NIE_est = a_est * b_est,
    NDE_est = c_est,
    a_est, 
    b_est, 
    c_est
  )
}











