################################################################################
# mediation_bootstrap_functions.R
# Bootstrap CIs for mediation effects (NIE, NDE) with IPTW weighting in multilevel data
# 
# Last Updated: 2026-05-11
# 
# Structure:
#   1. Setup helpers      – extract model metadata into a reusable list
#   2. Draw functions     – one bootstrap iteration per method (case / wild)
#   3. Fit functions      – refit model to bootstrap data, return target param
#   4. Orchestrator       – run_bootstrap() loops over draw + fit
#   5. Inference helpers  – CIs, summaries
#
# Extending to new methods: add a draw_<method>() function + fit_<method>() if needed
# Extending to mediation:   wrap two fit calls inside a custom extractor function
# 
# 
# To-Dos:
# 
#   # test on 1 dataset (with cluster-level confounder in model)
#   # add option to show progress bar (check how its done in bootMer from lme4 package: https://rdrr.io/cran/lme4/man/bootMer.html)
# 
# From cluade: 
# 
# Design: mirrors the setup -> draw -> fit -> extract pipeline in
# bootstrap_functions.R. The case bootstrap re-uses draw_case() unchanged.
# Only two new pieces are needed:
#   - setup_mediation()  : stores original fits + variable names + cluster info
#   - fit_mediation()    : refits PS -> mediator -> outcome on a bootstrap draw,
#                          returning a named numeric vector (NIE, NDE, a, b, c)
# Existing extractors / extractor factories (extractor_mc_one-style) work as-is
# because fit_mediation() already returns a named numeric vector.
#
# Requires: bootstrap_functions.R and estimate_mediation.R to be sourced first.
#
# Extending to other CI methods (positioning notes):
#   * Monte Carlo: would need a joint vcov for (a, b, c). Two options:
#         (i)  draw from each model's robust vcov independently (ignores
#              covariance across models but is simple);
#         (ii) build a joint sandwich vcov via stacked estimating equations
#              (PS scores + mediator scores + outcome scores). Skeleton noted
#              in setup_mediation_mc() stub at bottom of file.
#   * Residual bootstrap: same structure -- swap draw_case() for a residual
#         draw built on the outcome (or mediator+outcome) model fits. The
#         fit_mediation() function below does NOT need to change.
#   * Bayesian: replaces this file entirely with posterior draws from joint
#         Bayesian models of T, M, Y. Use the same inference helpers
#         (ci_percentile, bootstrap_summary) on the posterior matrix.
################################################################################

################################################################################
# mediation_bootstrap_functions.R
# Bootstrap CIs for mediation effects (NIE, NDE) with IPTW weighting in multilevel data
# 
# Last Updated: 2026-05-11
# 
# Structure:
#   1. Setup helpers      – extract model metadata into a reusable list
#   2. Draw functions     – one bootstrap iteration per method (case / wild)
#   3. Fit functions      – refit model to bootstrap data, return target param
#   4. Orchestrator       – run_bootstrap() loops over draw + fit
#   5. Inference helpers  – CIs, summaries
#
################################################################################


# 1. Setup helpers --------------------------------------------------------

#' glm extractor
#' 
#' Extract metadata needed for bootstrapping from a fitted glm
#'
#' @param fit       A fitted glm object
#' @param data      The original data frame used to fit the model
#' @param clust_name Character; name of the clustering variable in data
#' @return A named list of metadata
setup_glm <- function(fit, data, clust_name) {
  
  # index of used observations/individuals 
  used_idx <- which(rownames(data) %in% rownames(fit$model))
  # filter data to only keep used observations 
  data <- data[used_idx, ]
  # cluster idx 
  clust <- as.vector(unlist(data[[clust_name]]))
  
  list(
    fit = fit, 
    data = data, 
    clust_name = clust_name, 
    clust = clust, 
    clust_vals = unique(clust), 
    num_clust = length(unique(clust)), # number of clusters
    cluster_list = split(data, data[[clust_name]]), # pre-split data by cluster
    coef_names = rownames(summary(fit)$coefficients), 
    formula = fit$formula, 
    yhat = predict(fit), 
    resid = residuals(fit)
  )
}


#' lmer extractor
#' 
#' Extract metadata needed for bootstrapping from a fitted lmer model
#'
#' @param fit        A fitted lmerMod object (from lme4::lmer)
#' @param data       The original data frame
#' @param clust_name Character; name of the clustering variable
#' @return A named list of metadata
setup_lmer <- function(fit, data, clust_name) {
  
  # cluster idx 
  clust <- as.vector(unlist(data[[clust_name]]))
  
  list(
    fit = fit, 
    data = data, 
    clust_name = clust_name, 
    clust = clust, 
    clust_vals = unique(clust), 
    num_clust = length(unique(clust)), # number of clusters
    cluster_list = split(data, data[[clust_name]]), # pre-split data by cluster
    # coef_names = rownames(summary(fit)$coefficients), 
    formula = fit@call$formula, 
    REML = lme4::isREML(fit) 
  )
}


#' #' Setup for case bootstrap of a mediation analysis
#' #'
#' #' Stores everything fit_mediation() needs to refit the PS, mediator, and
#' #' outcome models on a bootstrap draw. Also stores the cluster metadata that
#' #' draw_case() expects (cluster_list, clust_vals, num_clust, clust_name), so
#' #' draw_case() from bootstrap_functions.R works without modification.
#' #'
#' #' @param med_fit    Output of estimate_mediation() on the original data
#' #' @param data       Original data frame
#' #' @param treat      Character; treatment variable name
#' #' @param mediator   Character; mediator variable name
#' #' @param outcome    Character; outcome variable name
#' #' @param covariates Character vector of covariate names
#' #' @param cluster    Character; clustering variable name
#' #' @param ps_model   "SL", "FE", or "RE" (see estimate_mediation())
#' #' @param med_model  "SL", "FE", or "RE"
#' #' @param out_model  "SL", "FE", or "RE"
#' #' @return Named list usable as the `setup` argument to run_bootstrap()
#' setup_mediation <- function(med_fit, data,
#'                             treat, mediator, outcome, covariates, cluster,
#'                             ps_model, med_model, out_model) {
#'   
#'   clust <- as.vector(unlist(data[[cluster]]))
#'   
#'   list(
#'     # original fit + point estimates (handy for ci_basic / summary)
#'     med_fit    = med_fit,
#'     theta_hat  = c(NIE = med_fit$NIE_est,
#'                    NDE = med_fit$NDE_est,
#'                    a   = med_fit$a_est,
#'                    b   = med_fit$b_est,
#'                    c   = med_fit$c_est),
#'     
#'     # cluster bits (match setup_glm / setup_lmer field names so draw_case works)
#'     data         = data,
#'     clust_name   = cluster,
#'     clust        = clust,
#'     clust_vals   = unique(clust),
#'     num_clust    = length(unique(clust)),
#'     cluster_list = split(data, data[[cluster]]),
#'     
#'     # everything fit_mediation() needs to rebuild the three models
#'     treat      = treat,
#'     mediator   = mediator,
#'     outcome    = outcome,
#'     covariates = covariates,
#'     cluster    = cluster,
#'     ps_model   = ps_model,
#'     med_model  = med_model,
#'     out_model  = out_model
#'   )
#' }


#' Setup for bootstrap of a mediation analysis
#'
#' Reads all model specifications off `med_fit$spec`, mirroring how setup_glm()
#' reads everything off the fit object. User only supplies (med_fit, data,
#' cluster). Adding new arguments to estimate_mediation() therefore requires
#' no changes here -- they will be carried through automatically via $spec.
#'
#' @param med_fit  Output of estimate_mediation() on the original data
#' @param data     Original data frame
#' @param cluster  Character; clustering variable name (optional override --
#'                 defaults to whatever was used in med_fit)
#' @return Named list usable as the `setup` argument to run_bootstrap()
setup_mediation <- function(med_fit, data, cluster = NULL) {
  
  spec <- med_fit$spec
  if (is.null(spec))
    stop("med_fit has no $spec element; re-run estimate_mediation().")
  
  # allow user override of cluster, else use what was used originally
  if (is.null(cluster)) cluster <- spec$cluster
  spec$cluster <- cluster   # keep spec self-consistent
  
  clust <- as.vector(unlist(data[[cluster]]))
  
  # Pull out the fitted mediator/outcome models to store hat & residual values
  medmod <- med_fit$mediator$medmod
  outmod <- med_fit$outcome$outmod
  
  list(
    # original fit + point estimates (handy for ci_basic / summary)
    med_fit = med_fit,
    theta_hat = c(NIE = med_fit$NIE_est,
                  NDE = med_fit$NDE_est,
                  a = med_fit$a_est,
                  b = med_fit$b_est,
                  c = med_fit$c_est),
                  # PNDE = med_fit$PNDE,
                  # TNDE = med_fit$TNDE,
                  # PNIE = med_fit$PNIE,
                  # TNIE = med_fit$TNIE,
                  # TE = med_fit$TE
    
    # cluster bits (match setup_glm / setup_lmer field names so draw_case works)
    data = data,
    clust_name = cluster,
    clust = clust,
    clust_vals = unique(clust),
    num_clust = length(unique(clust)),
    cluster_list = split(data, data[[cluster]]),
    
    # mediator equation
    m_hat = as.numeric(predict(medmod, type = "response")),
    m_resid = as.numeric(residuals(medmod, type = "response")),
    
    # outcome equation
    y_hat = as.numeric(predict(outmod, type = "response")),
    y_resid = as.numeric(residuals(outmod, type = "response")),
    
    # everything fit_mediation() needs to rebuild the three models
    spec = spec
  )
}








# 2. Draw functions -------------------------------------------------------
#     (one bootstrap dataset per method) 
#     Each returns a list: $data = bootstrap dataset, $meta = anything the
#     fit function needs that cannot be derived from $data alone.

#' Case (cluster) bootstrap draw
#'
#' Resample J clusters with replacement and stack their observations.
#' Works for both glm and lmer setups.
#'
#' @param setup  Output of setup_glm() or setup_lmer()
#' @return List with $data (bootstrap data frame) and $clust (cluster id vector)
draw_case <- function(setup) {
  
  # Draw clusters
  boot_clust_ids <- sample(setup$clust_vals, size = setup$num_clust, replace = TRUE)
  # Obtain one bootstrap draw 
  boot_data <- do.call(rbind, setup$cluster_list[boot_clust_ids])
  # Obtain vector of cluster IDs
  boot_clust <- as.vector(unlist(boot_data[[setup$clust_name]]))
  
  list(data = boot_data, 
       clust = boot_clust)
}



#' Wild bootstrap draw
#'
#' Applies Rademacher weights (+/-1) at the cluster level to residuals,
#' constructs a synthetic outcome, returns a modified copy of the original data.
#' Intended for glm (fixed-effect) models only.
#'
#' @param setup  Output of setup_glm()
#' @return List with $data (bootstrap data frame with synthetic outcome)
draw_wild <- function(setup) {
  
  # 1) Draw weights (-1 or 1) from an auxiliary distribution. 
  # Wild weights
  clust_weights <- sample(c(-1L, 1L), size = setup$num_clust, 
                          replace = TRUE, prob = c(0.5, 0.5))
  # note: this might be computationally faster (maybe test later): ifelse(runif(num_clust) < 0.5, 1, -1)
  
  # Map cluster-level weights to observation level
  weight <- clust_weights[match(setup$clust, setup$clust_vals)]
  
  # 2) Generate bootstrap data
  # Generate bootstrap outcome 
  y_star <- setup$yhat + setup$resid * weight
  
  # Create bootstrap dataset from model.frame
  boot_data <- model.frame(setup$fit)
  
  # Rename as.factor(clust_name) column back to clust_name if present
  factor_col <- paste0("as.factor(", setup$clust_name, ")")
  if (factor_col %in% names(boot_data)) {
    names(boot_data)[names(boot_data) == factor_col] <- setup$clust_name
  }
  
  # Replace outcome column with y*
  boot_data[[1]] <- y_star
  
  list(data = boot_data, 
       clust = setup$clust)
}


#' Wild bootstrap draw for a mediation analysis
#'
#' Applies Rademacher weights (+/-1) to mediator and outcome residuals (same weight 
#' for mediator & outcome and same weight for all in the same cluster). Note: 
#' M* is used when computing Y*. 
#'
#' Requires Gaussian mediator and Gaussian outcome. 
#' 
#' Note: fit_mediation() will still refits PS on the bootstrap data & might not 
#' need to; thus a future optimization could reuse the original IPTW.
#'
#' @param setup            Output of setup_mediation()
#' @return List with $data (boot data frame, A and X unchanged, M and Y
#'         replaced by M* and Y*) and $clust (cluster id vector).
draw_wild_mediation <- function(setup) {
  
  # 1) Draw Rademacher weights (-1 or 1) from an auxiliary distribution. 
  # Wild weights
  clust_weights <- sample(c(-1L, 1L), size = setup$num_clust, 
                          replace = TRUE, prob = c(0.5, 0.5))
  # note: this might be computationally faster (maybe test later): ifelse(runif(num_clust) < 0.5, 1, -1)
  
  # Map cluster-level weights to observation level
  weight <- clust_weights[match(setup$clust, setup$clust_vals)]
  
  # 2a) Generate bootstrap data - mediator 
  m_star <- setup$m_hat + setup$m_resid * weight
  
  # 2b) Generate bootstrap data - outcome 
  boot_data <- setup$data
  med_name <- setup$spec$mediator
  out_name <- setup$spec$outcome
  boot_data[[med_name]] <- m_star
  
  # recompute outcome using m_star
  outmod <- setup$med_fit$outcome$outmod
  y_hat_new <- as.numeric(predict(outmod, newdata = boot_data, type = "response"))
  
  y_star <- y_hat_new + setup$y_resid * weight 
  boot_data[[out_name]] <- y_star
  
  list(data = boot_data,
       clust = setup$clust)

}








# 3. Fit functions  -------------------------------------------------------
#     (refit model to bootstrap data & return target parameter)
#     Signature: fit_<type>(draw, setup, extract_fn)
#     extract_fn is a user-supplied function(model, setup) -> named numeric vector



#' Refit the full mediation pipeline on a bootstrap draw
#'
#' Signature matches fit_glm() / fit_lmer(): (draw, setup, extract_fn).
#' Refits PS -> mediator -> outcome on draw$data and returns
#' a named numeric vector. By default returns all five quantities
#' (NIE, NDE, a, b, c); pass an extract_fn to subset.
#'
#' @param draw       Output of draw_case() (or future draw_wild/draw_residual)
#' @param setup      Output of setup_mediation()
#' @param extract_fn Optional function(vec, setup) -> named numeric vector.
#'                   Defaults to identity (returns all five quantities).
#' @return Named numeric vector
# fit_mediation <- function(draw, setup, extract_fn = NULL) {
#   
#   boot_est <- estimate_mediation(
#     data       = draw$data,
#     ps_model   = setup$ps_model,
#     med_model  = setup$med_model,
#     out_model  = setup$out_model,
#     treat      = setup$treat,
#     covariates = setup$covariates,
#     mediator   = setup$mediator,
#     outcome    = setup$outcome,
#     cluster    = setup$cluster
#   )
#   
#   out <- c(NIE = boot_est$NIE_est,
#            NDE = boot_est$NDE_est,
#            a   = boot_est$a_est,
#            b   = boot_est$b_est,
#            c   = boot_est$c_est)
#   
#   if (is.null(extract_fn)) out else extract_fn(out, setup)
# }


#' Refit the full mediation pipeline on a bootstrap draw
#'
#' Signature matches fit_glm() / fit_lmer(): (draw, setup, extract_fn).
#' Calls estimate_mediation() using arguments stored in setup$spec, so any
#' new arguments added to estimate_mediation() are picked up automatically
#' via do.call().
#'
#' @param draw       Output of draw_case() (or future draw_wild/draw_residual)
#' @param setup      Output of setup_mediation()
#' @param extract_fn Optional function(vec, setup) -> named numeric vector.
#'                   Defaults to identity (returns all quantities).
#' @return Named numeric vector
fit_mediation <- function(draw, setup, extract_fn = NULL) {
  
  boot_est <- do.call(estimate_mediation,
                      c(list(data = draw$data), setup$spec))
  
  out <- c(NIE = boot_est$NIE_est,
           NDE = boot_est$NDE_est,
           a = boot_est$a_est,
           b = boot_est$b_est,
           c = boot_est$c_est
           # PNDE = boot_est$PNDE,
           # TNDE = boot_est$TNDE,
           # PNIE = boot_est$PNIE,
           # TNIE = boot_est$TNIE,
           # TE = boot_est$TE,
           )
  
  if (is.null(extract_fn)) out else extract_fn(out, setup)
}





## 3a. Extractor functions -------------------------------------------------

#' Mediation extractor: return everything (default)
extractor_med_all <- function(vec, setup) vec

#' Mediation extractor: return the four natural effects + TE
extractor_med_effects <- function(vec, setup) {
  vec[c("NIE", "NDE")]
  # vec[c("PNDE", "TNDE", "PNIE", "TNIE", "TE")]
}

#' Mediation extractor: return one named quantity
#' Usage: extractor_med_one("TNIE")
extractor_med_one <- function(term) {
  function(vec, setup) vec[term]
}

#' Mediation extractor: return a user-specified subset
#' Usage: extractor_med_subset(c("PNDE", "TNIE"))
extractor_med_subset <- function(terms) {
  function(vec, setup) vec[terms]
}






# 4. Orchestrator/Convenience wrapper ---------------------------------------------------------
# Run bootstraps 
# run_bootstrap() loops over draw + fit

# Run bootstraps 
# run_bootstrap() loops over draw + fit

#' Run bootstrap (optionally parallelized)
#'
#' @param setup      Output of setup_glm() or setup_lmer()
#' @param draw_fn    Function(setup) -> list($data, $clust)
#' @param fit_fn     Function(draw, setup, extract_fn) -> named numeric vector
#' @param extract_fn Function(boot_mod, setup) -> named numeric vector
#' @param B          Integer; number of bootstrap replications
#' @param seed       Optional integer for reproducibility
#' @param verbose    Logical; print progress (only applies when parallel = FALSE)
#' @param parallel   Logical; whether to parallelize (default FALSE)
#' @param n_cores    Integer; number of cores to use. Defaults to parallel::detectCores() - 1
#'
#' @return A matrix of bootstrap estimates: B rows x p cols
# run_bootstrap_OLD <- function(setup, 
#                           draw_fn, 
#                           fit_fn, 
#                           extract_fn, 
#                           B = 1000, # nsim (or R)
#                           seed = NULL, 
#                           verbose = FALSE, 
#                           parallel = FALSE, 
#                           n_cores = NULL) {
#   
#   if (!is.null(seed)) set.seed(seed)
#   boot_mat <- NULL   # will grow to B x p on first successful iteration
#   n_fail   <- 0L
#   
#   # sequential 
#   for (b in seq_len(B)) {
#     
#     if (verbose && b %% 100 == 0)
#       message(sprintf("Bootstrap iteration %d / %d  (failures so far: %d)", b, B, n_fail))
#     
#     result <- tryCatch({
#       draw <- draw_fn(setup)
#       fit_fn(draw, setup, extract_fn)
#     }, error = function(e) {
#       warning(sprintf("Iteration %d failed: %s", b, conditionMessage(e)))
#       NULL
#     })
#     
#     if (is.null(result)) {
#       n_fail <- n_fail + 1L
#       next
#     }
#     
#     if (is.null(boot_mat)) {
#       boot_mat <- matrix(NA_real_, nrow = B, ncol = length(result),
#                          dimnames = list(NULL, names(result)))
#     }
#     boot_mat[b, ] <- result
#   }
#   
#   if (n_fail > 0)
#     message(sprintf("Total failed iterations: %d / %d", n_fail, B))
#   
#   boot_mat
# }

run_bootstrap <- function(setup, 
                          draw_fn, 
                          fit_fn, 
                          extract_fn, 
                          B = 1000, # nsim (or R)
                          seed = NULL, 
                          verbose = FALSE, 
                          parallel = FALSE, 
                          n_cores = NULL) {
  
  # single iteration helper (used in serial & parallel) ---------------------
  one_iter <- function(b, setup, draw_fn, fit_fn, extract_fn) {
    tryCatch({
      draw <- draw_fn(setup)
      fit_fn(draw, setup, extract_fn)
    }, error = function(e) {
      warning(sprintf("Iteration %d failed: %s", b, conditionMessage(e)))
      NULL
    })
  }
  
  # parallel ----------------------------------------------------------------
  if (parallel) {
    n_cores <- if (is.null(n_cores)) max(1L, parallel::detectCores() - 1L) else n_cores
    
    os <- .Platform$OS.type
    
    if (os == "windows") {
      # parLapply: explicit cluster, must export everything
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)  # always clean up
      
      # L'Ecuyer-CMRG gives each worker its own independent RNG stream
      parallel::clusterSetRNGStream(cl, iseed = seed)
      
      # Export all objects workers need
      parallel::clusterExport(cl,
                              varlist = c("setup", "draw_fn", "fit_fn", "extract_fn", "one_iter"),
                              envir   = environment())
      
      # Export any packages workers need
      parallel::clusterEvalQ(cl, {
        library(lme4)
        library(sandwich)
        library(lmtest)
      })
      
      results <- parallel::parLapply(cl, seq_len(B), one_iter,
                                     setup      = setup,
                                     draw_fn    = draw_fn,
                                     fit_fn     = fit_fn,
                                     extract_fn = extract_fn)
      
    } else {
      # mclapply: fork-based, simpler — no export needed
      # Set seed via RNGkind for reproducibility across forks
      if (!is.null(seed)) set.seed(seed, kind = "L'Ecuyer-CMRG")
      
      results <- parallel::mclapply(seq_len(B), one_iter,
                                    setup = setup,
                                    draw_fn = draw_fn,
                                    fit_fn = fit_fn,
                                    extract_fn = extract_fn,
                                    mc.cores = n_cores)
    }
  } else {
    # serial ------------------------------------------------------------------  
    if (!is.null(seed)) set.seed(seed)
    
    results <- vector("list", B)
    n_fail <- 0L
    
    for (b in seq_len(B)) {
      if (verbose && b %% 100 == 0)
        message(sprintf("Iteration %d / %d  (failures: %d)", b, B, n_fail))
      results[[b]] <- one_iter(b, setup, draw_fn, fit_fn, extract_fn)
    }
  }
  
  # assemble output matrix --------------------------------------------------
  # Filter failures
  failed <- vapply(results, is.null, logical(1))
  n_fail <- sum(failed)
  
  if (n_fail > 0)
    message(sprintf("Total failed iterations: %d / %d", n_fail, B))
  
  # Use first successful result to get dimension names
  first_ok <- results[[which(!failed)[1]]]
  
  boot_mat <- matrix(NA_real_, nrow = B, ncol = length(first_ok),
                     dimnames = list(NULL, names(first_ok)))
  
  for (b in which(!failed)) boot_mat[b, ] <- results[[b]]
  
  boot_mat
}



#' Run a case bootstrap for mediation (thin wrapper around run_bootstrap)
#'
#' This is purely a convenience -- you can call run_bootstrap() directly with
#' draw_<method>() function + fit_mediation. Kept so the call site reads cleanly.
#'
#' @inheritParams run_bootstrap
#' @param setup      Output of setup_mediation()
#' @param extract_fn One of extractor_med_all (default), extractor_med_effects,
#'                   or extractor_med_one("NIE"). Pass NULL to get all five.
#' @return Matrix of bootstrap estimates: B x p
run_bootstrap_mediation <- function(setup,
                                    draw_fn = draw_case, # or draw_wild_mediation
                                    extract_fn = extractor_med_all,
                                    B = 1000,
                                    seed = NULL,
                                    verbose = FALSE,
                                    parallel = FALSE,
                                    n_cores = NULL) {
  
  run_bootstrap(setup = setup,
                draw_fn = draw_fn,
                fit_fn = fit_mediation,
                extract_fn = extract_fn,
                B = B,
                seed = seed,
                verbose = verbose,
                parallel = parallel,
                n_cores = n_cores)
}



# 5. Inference helpers ----------------------------------------------------

#' Percentile confidence intervals from a bootstrap matrix
#'
#' @param boot_mat  Matrix returned by run_bootstrap()
#' @param level     Confidence level (default 0.95)
#' @return Matrix of lower/upper bounds, one row per parameter
ci_percentile <- function(boot_mat, level = 0.95) {
  alpha <- (1 - level) / 2
  probs <- c(alpha, 1 - alpha)
  t(apply(boot_mat, 2, quantile, probs = probs, na.rm = TRUE))
}


#' Basic-method (reflected) confidence intervals
#'
#' @param boot_mat    Matrix returned by run_bootstrap()
#' @param theta_hat   Named numeric vector of original point estimates
#' @param level       Confidence level
#' @return Matrix of lower/upper bounds
ci_basic <- function(boot_mat, theta_hat, level = 0.95) {
  alpha <- (1 - level) / 2
  probs <- c(alpha, 1 - alpha)
  q <- t(apply(boot_mat, 2, quantile, probs = probs, na.rm = TRUE))
  ci <- cbind(
    lower = 2 * theta_hat - q[, 2],
    upper = 2 * theta_hat - q[, 1]
  )
  ci
}


#' Summarise bootstrap results
#'
#' @param boot_mat   Matrix returned by run_bootstrap()
#' @param theta_hat  Named numeric vector of original estimates (optional)
#' @param level      Confidence level
#' @return Data frame with estimate, bias, SE, and percentile CI
bootstrap_summary <- function(boot_mat, theta_hat = NULL, level = 0.95) {
  
  boot_means <- colMeans(boot_mat, na.rm = TRUE)
  boot_se <- apply(boot_mat, 2, sd, na.rm = TRUE)
  ci <- ci_percentile(boot_mat, level = level)
  
  out <- data.frame(
    term = colnames(boot_mat),
    boot_mean = boot_means,
    boot_se = boot_se,
    ci_lower = ci[, 1],
    ci_upper = ci[, 2],
    stringsAsFactors = FALSE
  )
  
  if (!is.null(theta_hat)) {
    out$estimate <- theta_hat[colnames(boot_mat)]
    out$bias <- boot_means - out$estimate
    out <- out[, c("term", "estimate", "bias", "boot_mean", "boot_se", "ci_lower", "ci_upper")]
  }
  
  rownames(out) <- NULL
  out
}



################################## END #########################################


