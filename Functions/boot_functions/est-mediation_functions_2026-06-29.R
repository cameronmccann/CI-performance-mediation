################################################################################
# est-mediation_functions.R
# Estimation & helper functions for mediation in multilevel models (1-1-1)
# 
# Last Updated: 2026-06-29
# 
# Structure:
#   1. Setup helpers      – extract model metadata into a reusable list
#   2. Draw functions     – one draw 
#   3. Extractor functions– 
#   4. Orchestrator       – run_mc() 
# 
# [UPDATE STRUCTURE & HEADERS]
# 
# To-Dos:
# 
# # Add checks to estimate_mediation() to ensure data is in stacked format 
#     (if not, maybe stack the data or refer users to function to stack the data?)
# 




# # create code to make sure variable names are in dataset (for all helpers or in main function)
# # Add RE-Mean (commented out and not tested)
# # Add code to support lower & upper case of model (e.g., sl or SL)
# # Return other coefficients so you can compute TNIE, PNIE, etc 
# 
# # Adapt code so users can write in formula (or something) for PS, mediator, & outcome models separately
#   (e.g., if they want different covariates for PS model but not outcome model)
# 
################################################################################


# Helpers -----------------------------------------------------------------

# ══════════════════════════════
#    stack_data() 
# ══════════════════════════════
# Transform data to long format for joint model (Bauer et al., 2006); Modified 
# from Falk et al. (2024) stack_bpg(). 
stack_data <- function(data, 
                       L2ID, 
                       A, M, Y, 
                       covars.M = NULL, 
                       covars.Y = NULL, 
                       cluster.covars = NULL, 
                       L1ID = NULL) {
  
  # ── 0. Input validation ───────────────────────────────────────────────────
  # Build vector of cols
  required_cols <- c(L2ID, A, M, Y)
  if (!is.null(L2ID))           required_cols <- c(required_cols, L2ID)
  if (!is.null(covars.M))       required_cols <- c(required_cols, covars.M)
  if (!is.null(covars.Y))       required_cols <- c(required_cols, covars.Y)
  if (!is.null(cluster.covars)) required_cols <- c(required_cols, cluster.covars)
  
  # Check if missing any required_cols
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following columns were not found in data: ",
         paste(missing_cols, collapse = ", "))
  }
  
  if (!is.numeric(data[[A]])) {
    stop("A ('", A, "') is of type ", class(data[[A]]),
         ". Currently only numeric A is supported.")
  }
  if (!is.numeric(data[[M]])) {
    stop("M ('", M, "') must be numeric.")
  }
  if (!is.numeric(data[[Y]])) {
    stop("Y ('", Y, "') must be numeric.")
  }
  
  # Checking for variable names that will be used later
  reserved <- c("Outcome", "Z", "Sy", "Sm", "SmA", "SyA", "SyM", "Md")
  colliding <- intersect(required_cols, reserved)
  if (length(colliding) > 0) {
    stop("The following column names are reserved internally by stack_data() ",
         "and cannot be used as variable names: ",
         paste(colliding, collapse = ", "))
  }
  
  # ?check this: 
  # # Warn if a covariate duplicates a role variable (rather than hard-stopping,
  # # since the same column might legitimately appear in both, e.g., U as both
  # # a cluster.covar and a covar.y)
  # role_vars <- c(A, M, Y)
  # dup_m <- intersect(covars.M, role_vars)
  # dup_y <- intersect(covars.Y, role_vars)
  # if (length(dup_m) > 0) {
  #   warning("covars.M contains variable(s) already assigned a role (A/M/Y): ",
  #           paste(dup_m, collapse = ", "),
  #           ". This is usually unintentional.")
  # }
  # if (length(dup_y) > 0) {
  #   warning("covars.Y contains variable(s) already assigned a role (A/M/Y): ",
  #           paste(dup_y, collapse = ", "),
  #           ". This is usually unintentional.")
  # }
  
  # ── 1. Build working dataframe with standardised column names ────────────
  tmp <- data.frame(
    A = data[[A]],
    Y = data[[Y]],
    M = data[[M]],
    L2id = data[[L2ID]],
    Md = data[[M]]   # preserve mediator value before pivoting
  )
  
  if (!is.null(L1ID)) {
    tmp$L1id <- data[[L1ID]]
  } else {
    tmp$L1id <- seq_len(nrow(data))
  }
  
  # ── 2. Cluster-level covariates ───────────────────────────────────────────
  # Carried through as-is; identical in both stacked rows for each person.
  # Users can reference them directly (e.g., Sy * U) in model formulas.
  if (!is.null(cluster.covars)) {
    for (cv in cluster.covars) {
      tmp[[cv]] <- data[[cv]]
    }
  }
  
  # ── 2. Individual-level covariates for M and Y models ────────────────────
  # Union of covars.M and covars.Y — both sets need to survive the pivot.
  # Selector-based interactions (Sm:covar vs Sy:covar) are handled in the
  # model formula, not here.
  all_covars <- unique(c(covars.M, covars.Y))
  for (cv in all_covars) {
    if (cv %in% names(tmp)) next   # already present (e.g., cluster covar)
    tmp[[cv]] <- data[[cv]]
  }
  
  # ── 3. Pivot to stacked (BPG) format ─────────────────────────────────────
  # restructure data such that both m and y are in the Z column
  tmp <- tidyr::pivot_longer(
    tmp,
    cols = c("Y", "M"),
    names_to = "Outcome",
    values_to = "Z"
  )
  
  # ── 4. Construct path-indicator columns ───────────────────────────────────
  # Create variables similar to Bauer et al (2006) syntax
  tmp$Sy  <- as.integer(tmp$Outcome == "Y")
  tmp$Sm  <- as.integer(tmp$Outcome == "M")
  tmp$SmA <- tmp$Sm * tmp$A   # "a" path
  tmp$SyA <- tmp$Sy * tmp$A   # direct effect
  tmp$SyM <- tmp$Sy * tmp$Md  # "b" path
  
  # # ── 5. Attach metadata as attributes (useful for downstream functions) ────
  # attr(tmp, "bpg_roles") <- list(
  #   L2ID          = "L2id",
  #   L1ID          = if (!is.null(L1ID)) "L1id" else NULL,
  #   A             = "A",
  #   Y             = "Y",
  #   M             = "M",
  #   covars.M      = covars.M,
  #   covars.Y      = covars.Y,
  #   cluster.covars = cluster.covars
  # )
  # class(tmp) <- c("bpg_stacked", class(tmp))
  
  return(tmp)
}

# # ══════════════════════════════
# #    test stack_data() 
# # ══════════════════════════════
# 
# stack_data(data = d, 
#            A = "A", 
#            M = "M", 
#            Y = "Y",
#            L2ID = "school", 
#            L1ID = "id",
#            covars.M = c("A", "X1"), 
#            covars.Y = c("A", "M", "X1", "X2")) |> 
#   head()
# 
# head(d, 3)




# ══════════════════════════════
#    build_formula() 
# ══════════════════════════════
# 
build_formula <- function(
    a.random.M = FALSE, 
    m.random.Y = FALSE, 
    a.random.Y = FALSE, 
    # Intercepts 
    int.random.M = FALSE, 
    int.random.Y = FALSE, 
    # Covariates 
    covars.M = NULL, 
    covars.Y = NULL, 
    cluster.covars = NULL, 
    random.covars.M = NULL, 
    random.covars.Y = NULL, 
    # AM interaction 
    treat.med.interaction = FALSE   # treatment x mediator interaction (SyA:SyM)
) {
  
  # ── Fixed effects ──────────────────────────────────────────────────────────
  # Fixed-effect as default 
  fixed <- "Z ~ 0 + Sm + Sy + SmA + SyA + SyM" 
  
  if (treat.med.interaction) 
    fixed <- paste(fixed, "+ SyA:SyM")
  
  if (!is.null(covars.M)) 
    fixed <- paste(fixed, paste0("+ Sm:", covars.M, collapse = " ")) 
  
  if (!is.null(covars.Y)) 
    fixed <- paste(fixed, paste0("+ Sy:", covars.Y, collapse = " ")) 
  
  # Check that cluster-level covariates are in covars.M or covars.Y
  if (!is.null(cluster.covars)) {
    missing_from_M <- cluster.covars[!paste0("Sm:", cluster.covars) %in% 
                                       strsplit(fixed, " \\+ ")[[1]]]
    missing_from_Y <- cluster.covars[!paste0("Sy:", cluster.covars) %in% 
                                       strsplit(fixed, " \\+ ")[[1]]]
    
    if (length(missing_from_M) > 0 & length(missing_from_Y) > 0) {
      missing_vars <- unique(c(missing_from_M, missing_from_Y))
      stop(
        "Cluster-level covariates must be included in covars.M and/or covars.Y. \n", 
        "The following cluster-level covariate(s) were specified in cluster.covars ",
        "but not added to the model via covars.M or covars.Y: \n",
        paste(missing_vars, collapse = ", "), ".\n"
        
      )
    }
  }
  
  # ── Random effects ─────────────────────────────────────────────────────────
  # Adding random-effects 
  random_terms <- c(
    if (int.random.M) "Sm",
    if (int.random.Y) "Sy",
    if (a.random.M) "SmA",
    if (m.random.Y) "SyM",
    if (a.random.Y) "SyA",
    if (!is.null(random.covars.M)) paste0("Sm:", random.covars.M),
    if (!is.null(random.covars.Y)) paste0("Sy:", random.covars.Y)
  )
  
  if (!is.null(random_terms)) {
    random <- as.formula(paste("~ 0 +", paste(random_terms, collapse = " + "), "| L2id"))
  } else {
    random <- NULL 
  }
  
  # ── Fixed-effect cluster dummies ───────────────────────────────────────────
  # Add cluster dummies if no random effects are present
  if (is.null(random_terms)) {
    fixed <- sub(" \\+ Sm \\+ Sy", "", fixed) 
    fixed <- paste(fixed, "+ Sy:as.factor(L2id) + Sm:as.factor(L2id)")
  }
  
  list(
    fixed  = as.formula(fixed),
    random = random
  )
}

# # ══════════════════════════════
# #    test build_formula() 
# # ══════════════════════════════
# # TEst 
# build_formula(a.random.M = T, m.random.Y = T)
# build_formula()
# ## including different covariates for M-model & Y-model 
# build_formula(covars.M = c("X1"), 
#               covars.Y = c("X1", "X2")) 
# ## adding cluster-level variable 
# build_formula(covars.M = c("X1"), 
#               covars.Y = c("X1", "X2", "U"), 
#               treat.med.interaction = T,
#               cluster.covars = c("U"))





# ══════════════════════════════
#    estimate_mediation() 
# ══════════════════════════════
# 
estimate_mediation <- function(data, 
                               estimator = c("lme"), # "glmmTMB"? or "lme4"? 
                               model_formula, 
                               method = c("REML", "ML"), 
                               control = NULL) {
  
  estimator <- match.arg(estimator)
  method <- match.arg(method)
  
  # Set controls (same as Falk et al., 2024)
  if (estimator == "lme" & is.null(control)){
    control <- nlme::lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
                                msMaxEval = 10000, tolerance = 1e-6)
  } 
  
  # Estimate fixed effect model 
  if (estimator == "lme" & is.null(model_formula$random)) {
    model_tmp <- try(nlme::gls(
      model = as.formula(model_formula$fixed),
      weights = nlme::varIdent(form = ~ 1 | Sm),
      method = method,
      data = data#,
      # control =
    ))
  } else if (estimator == "lme") {
    # Random effect model 
    model_tmp <- try(nlme::lme(
      fixed = as.formula(model_formula$fixed),
      random = as.formula(model_formula$random),
      weights = nlme::varIdent(form = ~ 1 | Sm),
      data = data,
      method = method,
      control = control
    ))
  }
  
  # Create output 
  out <- list()
  # handle error
  if (inherits(model_tmp, "try-error")) {
    out$model <- NULL
    out$conv <- FALSE 
  } else {
    out$model <- model_tmp
    out$conv <- TRUE 
  }
  
  # Add arguments provided by user 
  out$args <- list(
    estimator = estimator, 
    model_formula = model_formula, 
    method = method, 
    control = control
    # insert all arguments
  )
  
  return(out)
}

# # ══════════════════════════════
# #    test estimate_mediation() 
# # ══════════════════════════════
# # Fixed effect model 
# testFE <- estimate_mediation(data = d_stacked, 
#                              estimator = "lme", 
#                              model_formula = build_formula(), 
#                              method = "REML")
# 
# # Test random effects model 
# testRE <- estimate_mediation(
#   data = d_stacked,
#   estimator = "lme",
#   model_formula = build_formula(
#     int.random.M = T,
#     int.random.Y = T,
#     a.random.M = T,
#     m.random.Y = T,
#     a.random.Y = T
#   ),
#   method = "REML"
# )
# 
# # obtain ab covariance 
# nlme::getVarCov(testRE$model)["SmA", "SyM"]





# ══════════════════════════════
#    Example Usage  
# ══════════════════════════════

# # ═══════════════════
# #    simulate & adjust data & set controls 
# # ═══════════════════
# # packages required 
# library(nlme) 
# library(tidyr)
# 
# # sim data 
# data <- simMed::generate_data(
#   J = 10,
#   njrange = c(25, 25),
#   Mfamily = "gaussian",
#   Yfamily = "gaussian",
#   seed = 8675309,
#   num_x = 1,
#   ensure_cluster_positivity = TRUE
# )$data
# # rename cluster covariate 
# data <- data |> rename(U = Z)
# 
# # stack data 
# data_stacked <- stack_data(
#   data, 
#   L2ID = "school", 
#   A = "A", M = "M", Y = "Y", 
#   covars.M = c("X1", "U"), covars.Y = c("X1", "U"), 
#   cluster.covars = "U"
# )
# 
# # Controls used in Falk et al 2024 for estimation 
# control <- nlme::lmeControl(maxIter = 10000, msMaxIter = 10000, niterEM = 10000,
#                             msMaxEval = 10000, tolerance = 1e-6)
# 
# # ═══════════════════
# #    Fixed-Effect  
# # ═══════════════════
# # build formula 
# fe.form <- build_formula()
# # fit fe model 
# fe.model2 <- estimate_mediation(
#   data = data_stacked, 
#   # estimator = "lme", # default 
#   model_formula = fe.form, 
#   method = "REML", 
#   control = control
# )
# # view coefficients 
# fe.model2$model$coefficients[1:4]
# 
# # ═══════════════════
# #    Random-Intercept 
# # ═══════════════════
# # build formula 
# ri.form <- build_formula(int.random.M = T, int.random.Y = T)
# # fit RI model 
# ri.model3 <- estimate_mediation(
#   data = data_stacked, 
#   model_formula = ri.form, 
#   method = "REML", 
#   control = control
# )
# # view coefficients 
# ri.model3$model$coefficients
# 
# # ═══════════════════
# #    Random-Slopes  
# # ═══════════════════
# # build formula 
# rs.form <- build_formula(int.random.M = T, int.random.Y = T, 
#                          a.random.M = T, m.random.Y = T, a.random.Y = T)
# # fit RS/RE model 
# rs.model <- estimate_mediation(
#   data = data_stacked, 
#   model_formula = rs.form, 
#   method = "REML", 
#   control = control
# )
# # view coefficients 
# rs.model$model$coefficients






