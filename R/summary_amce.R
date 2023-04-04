#' Summarizing AMCE Estimates

# Function for summarizing output from main amce function
# LIST given to "covariate.values" contains VECTORS at which ...
# ... conditional effects will be calculated; default is quantiles...
# ... in the case of continuous, levels otherwise
# ... can be given manual names by naming entries
# Note that must be NAMED LIST, entry name is the respondent.varying effect

#' @param object An object of class "amce", a result of a call to \link{amce}.
#' 
#' @param covariate.values An optional list containing a vector at which  
#' conditional effects will be calculated in the case of AMCE and ACIE's 
#' conditional on respondent-varying characteristics. 
#' The class of the values in the vector must match the class of the 
#' respondent-varying characteristic in question. If the "amce" object 
#' contains respondent varying characteristics, when set to NULL (default) 
#' interaction effects will be reported at quantiles in the case of a 
#' continuous variable and levels in the case of a factor. Names of list 
#' entries must correspond to variable names. If there are multiple 
#' respondent-varying characteristics then while each is varied in turn, 
#' all others will be held at first value of their entry in covariate.values. 
#' This is the bottom quantile in the case of a continuous variable and the 
#' baseline in the case of a factor variable.
#' 
#' @param ... Further arguments from other methods.
#' 
#' @method summary amce
#' @export summary.amce
#' @export

summary.amce <- function(object, covariate.values = NULL, ...) {
  amce_obj <- object

  ######################### administrative section

  # Initialize list to store summary object
  summary_results <- list()
  # Create header of data.frame
  header <- c(
    "Attribute", "Level", "Estimate", 
    "Std. Err", "z value", "Pr(>|z|)", " ")

  # checks on user-supplied covariate values 
  # (values at which conditional effects are calculated)
  if (!is.null(covariate.values)) {
    # clean variable names
    names(covariate.values) <- clean.names(names(covariate.values))
    for (i in 1:length(covariate.values)) {
      # make sure they appear AMCE object
      if (!names(covariate.values)[i] %in% amce_obj$respondent.varying) {
        stop(paste(
          c(
            "Error: variable",
            names(covariate.values)[i], 
            "is not a respondent-varying characteristic in AMCE object."
          ),
          collapse = " "
        ))
      }
      # if they do appear and are factors, clean them and make sure valid
      if (names(covariate.values)[i] %in% names(amce_obj$baselines)) {
        covariate.values[[i]] <- clean.names(covariate.values[[i]])
        if (
          any(!covariate.values[[i]] %in% levels(
            amce_obj$data[[names(covariate.values)[i]]]))) {
          stop(
            paste(
              "Error: some level(s) of variable",
              names(covariate.values)[i],
              "do not appear in data."
            )
          )
        }
        if (is.null(names(covariate.values[[i]]))) {
          cov.names <- sapply(
            covariate.values[[i]], 
            function(x) paste(names(covariate.values)[i], x, sep = "")
          )
          names(covariate.values[[i]]) <- sapply(
            cov.names,
            USE.NAMES = F, function(x) amce_obj$user.levels[x]
          )
        }
      }
    }
  }

  ##################### reporting unconditional results

  # all attribute estimates
  all_prof <- names(amce_obj$estimates)
  # get AMCE only
  all_amce <- grep(":", all_prof, value = T, invert = T)
  # get ACIE only
  all_acie <- grep(":", all_prof, value = T, invert = F)

  # How many AMCE?
  namce <- sum(
    sapply(all_amce, function(x) length(unlist(amce_obj$estimates[[x]])) / 2)
  )
  # Create results matrix for AMCE only
  summary_results[["amce"]] <- matrix(nrow = namce, ncol = length(header))
  colnames(summary_results[["amce"]]) <- header
  summary_results[["amce"]] <- as.data.frame(summary_results[["amce"]])
  # amce index
  amce_i <- 1

  # summary results matrix for ACIE only
  if (length(all_acie) > 0) {
    # number affected
    nacie <- sum(
      sapply(all_acie, function(x) length(unlist(amce_obj$estimates[[x]])) / 2)
    )
    # make results matrix
    summary_results[["acie"]] <- matrix(nrow = nacie, ncol = length(header))
    colnames(summary_results[["acie"]]) <- header
    summary_results[["acie"]] <- as.data.frame(summary_results[["acie"]])
    # acie index
    acie_i <- 1
  }

  # objects for storing baselines
  baselines_amce <- c()
  baselines_acie <- c()
  # objects for storing effect names
  names_amce <- c()
  names_acie <- c()

  # Loop over non-respondent varying attributes, 
  # which are all factors by assumption
  for (effect in all_prof) {
    # split terms
    variates <- strsplit(effect, ":")[[1]]
    # Figure out the baseline level(s)
    lev_list <- c()
    print_names <- c()
    for (var in variates) {
      lev_list <- c(
        lev_list,
        amce_obj$user.levels[[
          clean.names(paste0(var, amce_obj$baselines[[var]]))]]
      )
      print_names <- c(print_names, amce_obj$user.names[[var]])
    }
    print_level <- paste(lev_list, sep = "", collapse = ":")
    print_effect <- paste(print_names, sep = "", collapse = ":")
    # If ACIE
    if (grepl(":", effect)) {
      # set entry name
      entry_name <- "acie"
      # report baselines and names
      baselines_acie <- c(baselines_acie, print_level)
      names_acie <- c(names_acie, print_effect)
      # which index?
      index <- acie_i
    } else {
      entry_name <- "amce"
      # report baselines and names
      baselines_amce <- c(baselines_amce, print_level)
      names_amce <- c(names_amce, print_effect)
      # which index?
      index <- amce_i
    }
    # Append results to the estimates dataframe
    for (p in 1:ncol(amce_obj$estimates[[effect]])) {
      variate_levels <-
        strsplit(colnames(amce_obj$estimates[[effect]])[p], ":")[[1]]
      lev_list <- c()
      for (lev in variate_levels) {
        lev_list <- c(lev_list, amce_obj$user.levels[[lev]])
      }
      print_level <- paste(lev_list, sep = "", collapse = ":")
      summary_results[[entry_name]][index, 1] <- print_effect
      summary_results[[entry_name]][index, 2] <- print_level
      summary_results[[entry_name]][index, 3] <-
        amce_obj$estimates[[effect]][1, p]
      summary_results[[entry_name]][index, 4] <-
        amce_obj$estimates[[effect]][2, p]
      zscr <-
        amce_obj$estimates[[effect]][1, p] / amce_obj$estimates[[effect]][2, p]
      summary_results[[entry_name]][index, 5] <- zscr
      pval <- 2 * pnorm(-abs(zscr))
      summary_results[[entry_name]][index, 6] <- pval
      # Stars!
      if (!is.na(pval)) {
        if (pval < .001) {
          summary_results[[entry_name]][index, 7] <- "***"
        } else if (pval < .01) {
          summary_results[[entry_name]][index, 7] <- "**"
        } else if (pval < .05) {
          summary_results[[entry_name]][index, 7] <- "*"
        } else {
          summary_results[[entry_name]][index, 7] <- ""
        }
      } else {
        summary_results[[entry_name]][index, 7] <- ""
      }
      index <- index + 1
    }
    # advance appropriate index
    if (grepl(":", effect)) {
      acie_i <- index
    } else {
      amce_i <- index
    }
  }

  # save as data frame and save baselines
  summary_results[["amce"]] <- as.data.frame(summary_results[["amce"]])
  summary_results[["baselines_amce"]] <-
    data.frame("Attribute" = names_amce, "Level" = baselines_amce)

  # if any acie save those too
  if (grepl(":", effect)) {
    summary_results[["acie"]] <- as.data.frame(summary_results[["acie"]])
    summary_results[["baselines_acie"]] <-
      data.frame("Attribute" = names_acie, "Level" = baselines_acie)
  }

  ################ reporting conditional results

  ## all appearances by the modified variable (say "var1")
  ## have already had the beta and var, cov for any other appearances added in
  ## (such as "dependency:var1:respondent.var" for "var1:respondent.var")
  ## So the only things that need to be grabbed are the profile term
  ## and the interaction with the respondent varying term
  ## (var1 and var1:respondent.var)

  # If there are respondent varying attributes, 
  # add estimates at levels/quantiles
  if (length(amce_obj$cond.estimates) > 0) {
    ### Setting covariate values at which conditional effects will be calculated
    if (is.null(covariate.values)) covariate.values <- list()
    # get levels at which conditional effects will be calculated
    # if no values given or install given names
    # straight levels, NOT coefficient names
    if (any(!amce_obj$respondent.varying %in% names(covariate.values))) {
      for (
        var in amce_obj$respondent.varying[
          !amce_obj$respondent.varying %in% names(covariate.values)
        ]) {
        if (var %in% names(amce_obj$baselines)) {
          # if it's a factor get levels from column names
          cov.vals <- colnames(amce_obj$cond.estimates[[var]])
          # remove effect part
          covariate.values[[var]] <- sub(var, "", cov.vals)
          # add baseline
          covariate.values[[var]] <-
            c(amce_obj$baselines[[var]], covariate.values[[var]])
          # names from user input
          cov.vals <-
            c(paste(var, amce_obj$baselines[[var]], sep = ""), cov.vals)
          names(covariate.values[[var]]) <- sapply(
            cov.vals,
            USE.NAMES = F, function(x) amce_obj$user.levels[[x]]
          )
        } else {
          # otherwise get summary information from "continuous"
          covariate.values[[var]] <- amce_obj$continuous[[var]]
        }
      }
    }

    # empty vectors for table key
    tab_name_amce <- c()
    tab_var_amce <- c()
    tab_val_amce <- c()
    tab_name_acie <- c()
    tab_var_acie <- c()
    tab_val_acie <- c()

    # Loop over respondent-varying characteristics
    for (effect in amce_obj$respondent.varying) {
      # how to print the effect name
      print_effect <- amce_obj$user.names[[effect]]
      # identify all REQUESTED terms involving effect
      all_req_vars <- attr(terms(amce_obj$formula), "term.labels")
      all_resp <- unlist(sapply(all_req_vars, function(x) {
        y <- strsplit(x, ":")[[1]]
        if (any(y == effect)) x
      }))
      # figure out profile attributes these refer to
      all_mod <- unlist(sapply(all_resp, function(x) {
        subs <- strsplit(x, ":")[[1]]
        subs <- subs[is.element(subs, all_prof)]
        if (length(subs) > 0) paste(subs, collapse = ":")
      }))
      # just unique ones
      all_mod <- unique(all_mod)
      # make sure there are some
      if (length(all_mod) == 0) {
        stop(
          paste(
            c(
              "respondent characteristic", 
              effect,
              "not interacted with profile attributes, no interpretation"
            ),
            collapse = " "
          )
        )
      }

      #### split modified terms into AMCE or ACIE
      mod_amce <- grep(":", all_mod, value = T, invert = T)
      mod_acie <- grep(":", all_mod, value = T, invert = F)
      # how many AMCE are affected by this respondent characteristic?
      namce <- sum(
        sapply(
          mod_amce, function(x) length(unlist(amce_obj$estimates[[x]])) / 2
        )
      )
      # how many ACIE are affected by this respondent characteristic?
      if (length(mod_acie) > 0) {
        nacie <- sum(
          sapply(
            mod_acie, function(x) length(unlist(amce_obj$estimates[[x]])) / 2
          )
        )
      }

      ######## Now loop over levels/quantiles of "effect"
      for (i in 1:length(covariate.values[[effect]])) {
        # get name of coefficient and level; also how to print level
        cond_lev <- covariate.values[[effect]][i]
        # how to print level name
        if (!is.null(names(covariate.values[[effect]]))) {
          print_cond_lev <- names(covariate.values[[effect]])[i]
        } else {
          print_cond_lev <- covariate.values[[effect]][i]
        }

        #### prep AMCE results
        # make results matrices
        entry_name_amce <- paste(c(effect, i, "amce"), collapse = "")
        # make empty results matrix
        summary_results[[entry_name_amce]] <-
          matrix(NA, nrow = namce, ncol = length(header))
        colnames(summary_results[[entry_name_amce]]) <- header
        summary_results[[entry_name_amce]] <-
          as.data.frame(summary_results[[entry_name_amce]])
        amce_i <- 1
        # edit table key
        tab_name_amce <- c(tab_name_amce, entry_name_amce)
        tab_var_amce <- c(tab_var_amce, effect)
        tab_val_amce <- c(tab_val_amce, print_cond_lev)

        #### prep ACIE results, if any
        if (length(mod_acie) > 0) {
          # entry name
          entry_name_acie <- paste(c(effect, i, "acie"), collapse = "")
          # results matrix
          summary_results[[entry_name_acie]] <-
            matrix(NA, nrow = nacie, ncol = length(header))
          colnames(summary_results[[entry_name_acie]]) <- header
          summary_results[[entry_name_acie]] <-
            as.data.frame(summary_results[[entry_name_acie]])
          acie_i <- 1
          # edit table key
          tab_name_acie <- c(tab_name_acie, entry_name_acie)
          tab_var_acie <- c(tab_var_acie, effect)
          tab_val_acie <- c(tab_val_acie, print_cond_lev)
        }

        # loop over all attribute effects modified by "effect"
        for (mod_var in all_mod) {
          # get name to print
          mod_vars <- strsplit(mod_var, ":")[[1]]
          print_vars <- c()
          for (var in mod_vars) {
            print_vars <- c(print_vars, amce_obj$user.names[[var]])
          }
          print_mod_var <- paste(c(print_vars), collapse = ":")
          # Set entry to add to
          if (grepl(":", mod_var)) {
            # set entry name
            entry_name <- entry_name_acie
            # which index?
            index <- acie_i
          } else {
            entry_name <- entry_name_amce
            # which index?
            index <- amce_i
          }
          # calculate conditional effects
          cond.effects <- get.conditional.effects(
            amce_obj, covariate.values, effect, cond_lev, mod_var
          )
          # loop over the associated betas
          for (p in 1:ncol(amce_obj$cond.estimates[[mod_var]])) {
            # level to be modified
            mod_coef <- colnames(amce_obj$cond.estimates[[mod_var]])[p]
            # how to print it
            mod_coefs <- strsplit(mod_coef, ":")[[1]]
            print_levs <- c()
            for (v in 1:length(mod_coefs)) {
              print_levs <- c(print_levs, amce_obj$user.levels[[mod_coefs[v]]])
            }
            print_mod_level <- paste(print_levs, sep = "", collapse = ":")
            # get beta and SE
            cond_beta <- cond.effects[1, mod_coef]
            cond_se <- cond.effects[2, mod_coef]
            # modified zscr and pval
            if (!is.na(cond_beta)) {
              zscr <- cond_beta / cond_se
              pval <- 2 * pnorm(-abs(zscr))
            } else {
              zscr <- pval <- NA
            }
            # write results
            summary_results[[entry_name]][index, 1] <- print_mod_var
            summary_results[[entry_name]][index, 2] <- print_mod_level
            summary_results[[entry_name]][index, 3] <- cond_beta
            summary_results[[entry_name]][index, 4] <- cond_se
            summary_results[[entry_name]][index, 5] <- zscr
            summary_results[[entry_name]][index, 6] <- pval
            # Stars!
            if (!is.na(cond_beta)) {
              if (pval < .001) {
                summary_results[[entry_name]][index, 7] <- "***"
              } else if (pval < .01) {
                summary_results[[entry_name]][index, 7] <- "**"
              } else if (pval < .05) {
                summary_results[[entry_name]][index, 7] <- "*"
              } else {
                summary_results[[entry_name]][index, 7] <- ""
              }
            } else {
              summary_results[[entry_name]][index, 7] <- ""
            }
            index <- index + 1
          } # end loop over levels of profile var
          # advance appropriate index
          if (grepl(":", effect)) {
            acie_i <- index
          } else {
            amce_i <- index
          }
        } # end loop over modified profile vars

        # save AMCE results as data frame
        summary_results[[entry_name_amce]] <-
          as.data.frame(summary_results[[entry_name_amce]])
        # and ACIE, if any
        if (length(mod_acie) > 0) {
          summary_results[[entry_name_acie]] <-
            as.data.frame(summary_results[[entry_name_acie]])
        }
      } # end loop over respondent var levels (lev_list)
    } # end loop over respondent varying characteristics

    # save results as data frame and save baselines
    summary_results[["table_values_amce"]] <- data.frame(
      "Table Name" = tab_name_amce,
      "Level Name" = tab_var_amce,
      "Level Value" = tab_val_amce
    )
    summary_results[["table_values_amce"]] <- apply(
      summary_results[["table_values_amce"]],
      c(1, 2), function(x) as.character(x)
    )

    summary_results[["table_values_acie"]] <- data.frame(
      "Table Name" = tab_name_acie,
      "Level Name" = tab_var_acie,
      "Level Value" = tab_val_acie
    )
    summary_results[["table_values_acie"]] <- apply(
      summary_results[["table_values_acie"]],
      c(1, 2), function(x) as.character(x)
    )
  } else {
    summary_results[["table_values_amce"]] <- NULL
    summary_results[["table_values_acie"]] <- NULL
  }

  # Save sample size(s)
  summary_results[["samplesize_estimates"]] <- amce_obj$samplesize_prof
  if (!is.null(amce_obj$samplesize_full)) {
    summary_results[["samplesize_resp"]] <- amce_obj$samplesize_full
  } else {
    summary_results[["samplesize_resp"]] <- NULL
  }

  # If there's a respondent number, add that as well
  if (!is.null(amce_obj$numrespondents)) {
    summary_results[["respondents"]] <- amce_obj$numrespondents
  } else {
    summary_results[["respondents"]] <- NULL
  }

  # Set class
  class(summary_results) <- c("summary.amce")

  # Return
  return(summary_results)
}

#' Printing Summaries of AMCE Estimates
#' 
#' @param x An object of class "summary.amce", 
#' a result of a call to summary.amce.
#' @param digits The number of significant digits to use when printing.
#' @param ... Further arguments from other methods.
#' 
#' @method print summary.amce
#' @export print.summary.amce
#' @export

print.summary.amce <- function(x, digits = 5, ...) {
  summary_result <- x
  
  # basic print for AMCE
  cat("------------------------------------------\n")
  cat("Average Marginal Component Effects (AMCE):\n")
  cat("------------------------------------------\n")
  print(summary_result$amce, digits = digits, row.names = F)
  cat("---\n")
  cat(paste0("Number of Obs. = ", summary_result$samplesize_estimates))
  cat("\n")
  cat("---\n")
  if (!is.null(summary_result$respondents)) {
    cat(paste0("Number of Respondents = ", summary_result$respondents))
    cat("\n")
    cat("---\n")
  }
  
  # add extra tables for AMCE interactions with respondent varying
  if (!is.null(summary_result$table_values_amce)) {
    if (nrow(summary_result$table_values_amce) > 0) {
      all.vars <- unique(summary_result$table_values_amce[, 2])
      for (i in 1:nrow(summary_result$table_values_amce)) {
        # How to print changing level
        print.lev <- paste(
          c(
            "Conditional AMCE's ", "(",
            summary_result$table_values_amce[i, 2], " = ",
            summary_result$table_values_amce[i, 3]
          ),
          collapse = ""
        )
        # How to print stable level
        if (length(all.vars) > 1) {
          this.var <- summary_result$table_values_amce[i, 2]
          other.vars <- all.vars[!is.element(all.vars, this.var)]
          for (x in other.vars) {
            lev <- summary_result$table_values_amce[
              summary_result$table_values_amce[, "Level.Name"] == x,
              "Level.Value"
            ][1]
            print.lev <- paste(c(print.lev, "; ", x, " = ", lev), collapse = "")
          }
        }
        print.lev <- paste(c(print.lev, "):\n"), collapse = "")
        
        cat("------------------------------------------------------------\n")
        cat(print.lev)
        cat("------------------------------------------------------------\n")
        print(
          summary_result[[summary_result$table_values_amce[i, 1]]],
          digits = digits, row.names = F
        )
        cat("---\n")
        cat(paste0("Number of Obs. = ", summary_result$samplesize_resp))
        cat("\n")
        if (!is.null(summary_result$respondents)) {
          cat(paste("Number of Respondents = ", summary_result$respondents))
          cat("\n")
        }
        cat("---\n")
        cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05")
        cat("\n")
        cat("\n")
      }
    }
  }
  
  # print AMCE baselines
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05")
  cat("\n")
  cat("\n")
  cat("--------------------\n")
  cat("AMCE Baseline Levels:\n")
  cat("--------------------\n")
  print(summary_result$baselines_amce, row.names = F)
  cat("\n")
  cat("\n")
  
  
  # Tables for UNCONDITIONAL interaction
  if (!is.null(summary_result$acie)) {
    cat("---------------------------------------------\n")
    cat("Average Component Interaction Effects (ACIE):\n")
    cat("---------------------------------------------\n")
    print(summary_result$acie, digits = digits, row.names = F)
    cat("---\n")
    cat(paste0("Number of Obs. = ", summary_result$samplesize_estimates))
    cat("\n")
    if (!is.null(summary_result$respondents)) {
      cat(paste0("Number of Respondents = ", summary_result$respondents))
      cat("\n")
    }
  }
  
  # add extra tables for ACIE interactions with respondent varying
  if (!is.null(summary_result$table_values_acie)) {
    if (nrow(summary_result$table_values_acie) > 0) {
      for (i in 1:nrow(summary_result$table_values_acie)) {
        cat("------------------------------------------------------------\n")
        cat(
          paste(
            c(
              "Conditional ACIE's",
              "(",
              summary_result$table_values_acie[i, 2],
              "=",
              summary_result$table_values_acie[i, 3],
              "):\n"
            ),
            collapse = " "
          )
        )
        cat("------------------------------------------------------------\n")
        print(
          summary_result[[summary_result$table_values_acie[i, 1]]],
          digits = digits, row.names = F
        )
        cat("---\n")
        cat(
          paste0("Number of Obs. = ", summary_result$samplesize_resp)
        )
        cat("\n")
        if (!is.null(summary_result$respondents)) {
          cat(
            paste0(
              "Number of Respondents = ",
              summary_result$respondents
            )
          )
          cat("\n")
        }
        cat("---\n")
        cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05")
        cat("\n")
        cat("\n")
      }
    }
  }
  
  # baselines for ACIE
  if (!is.null(summary_result$acie)) {
    cat("---\n")
    cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05")
    cat("\n")
    cat("\n")
    cat("--------------------\n")
    cat("ACIE Baseline Levels:\n")
    cat("--------------------\n")
    print(summary_result$baselines_acie, row.names = F)
    cat("\n")
    cat("\n")
  }
}
