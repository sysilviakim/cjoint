#' Print Summary Function for Results of Main AMCE Function

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
