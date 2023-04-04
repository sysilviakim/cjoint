#' Conditional Estimates
#'
#' Getting conditional estimates given
#' (1) AMCE object
#' (2) LIST containing levels at which conditional effects will be calculated
#' (3) NAME of current conditional variable
#' (4) VALUE of current level of current conditioning variable
#' (5) NAME of profile attribute being modified by conditioning variable

get.conditional.effects <- function(object,
                                    conditional.levels,
                                    current.effect,
                                    current.level,
                                    mod.var) {
  # amce object
  amce_obj <- object
  # make dummy data and set in base levels
  cond.data <- amce_obj$data
  # set factor vars to baselines
  for (var in names(amce_obj$baselines)) {
    cond.data[[var]] <- amce_obj$baselines[[var]]
  }
  # set in conditional vars to their first given level
  for (var in names(conditional.levels)) {
    cond.data[[var]] <- conditional.levels[[var]][1]
  }
  # set current effect to current level
  cond.data[[current.effect]] <- current.level

  # original levels of conditional vars
  orig.levels <- sapply(
    all.vars(amce_obj$cond.formula)[-1][
      all.vars(amce_obj$cond.formula)[-1] %in% names(amce_obj$baselines)
    ],
    function(x) levels(amce_obj$data[[x]]),
    simplify = F
  )
  # coefficients associated with conditional estimates base term
  cond.base <- unlist(
    sapply(
      amce_obj$respondent.varying,
      USE.NAMES = F,
      function(x) colnames(amce_obj$cond.estimates[[x]])
    )
  )
  # estimated coefficients, adding 0 for intercept
  cond.beta <- c(0, do.call(cbind, amce_obj$cond.estimates)[1, ])

  # blank output
  estimates.vector <- c()
  error.vector <- c()
  names.vector <- c()

  # quick covariance getting function
  cov.ij <- function(var1, var2) {
    out <- pred_mat[var1] * pred_mat[var2] * amce_obj$vcov.resp[var1, var2]
    return(out)
  }
  cov.ij <- Vectorize(cov.ij, vectorize.args = c("var1", "var2"))

  # function for NA multiplication to be used in special cases
  na.multiply <- function(x, y) {
    vec <- c(x, y)
    # If either is NA and other is 0, return 0
    if (any(is.na(vec)) && vec[!is.na(vec)] == 0) {
      out <- 0
    } else {
      # otherwise normal (so 1 * NA = NA)
      out <- x * y
    }
  }
  na.multiply <- Vectorize(na.multiply, vectorize.args = c("x", "y"))

  # split up modified variable
  mod_vars <- strsplit(mod.var, ":")[[1]]

  # loop over levels
  for (mod_coef in colnames(amce_obj$cond.estimates[[mod.var]])) {
    ## (1) Edit conditional data
    # split level coefficient into components
    mod_coefs <- strsplit(mod_coef, ":")[[1]]
    # edit cond data to fit this level
    for (x in 1:length(mod_coefs)) {
      mod_lev <- sub(mod_vars[x], "", mod_coefs[x])
      cond.data[[mod_vars[x]]] <- mod_lev
    }

    ## (2) Make model matrix
    pred_mat <-
      model.matrix(amce_obj$cond.formula, cond.data, xlev = orig.levels)

    ## (3) Turn off base term for this conditional var
    turn_off <- rep(1, ncol(pred_mat))
    names(turn_off) <- colnames(pred_mat)
    turn_off[cond.base] <- 0
    # Use to turn off terms in pred_mat that
    # only contain respondent varying items
    pred_mat <- pred_mat[1, ] * turn_off

    ## (4) Calculate coefficient and SE
    ### (a) Coefficient
    if (!any(is.na(cond.beta))) {
      pred_val <- sum(pred_mat * cond.beta)
    } else {
      # otherwise use special function
      pred_val <- sum(na.multiply(pred_mat, cond.beta))
    }
    ### (b) SE
    if (!is.na(pred_val)) {
      # variable names sans intercept
      vars <- colnames(amce_obj$vcov.resp)[2:ncol(amce_obj$vcov.resp)]
      # all other covariance combinations
      all_cov <- outer(vars, vars, FUN = function(x, y) cov.ij(x, y))
      pred_se <- sqrt(sum(all_cov))
    } else {
      pred_se <- NA
    }

    ## And print out
    estimates.vector <- c(estimates.vector, pred_val)
    error.vector <- c(error.vector, pred_se)
    names.vector <- c(names.vector, mod_coef)
  }

  # return list of modified coefficient estimates and SEs
  names(estimates.vector) <- names(error.vector) <- names.vector
  out <- rbind(estimates.vector, error.vector)
  return(out)
}
