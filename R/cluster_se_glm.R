#' Clustered Standard Errors
#'
#' @param model Input model
#' @param cluster Cluster variable
#' 
#' @import sandwich
#' @importFrom utils read.csv packageDescription

cluster_se_glm <- function(model, cluster) {
  #  Drop unused cluster indicators, if cluster var is a factor
  if ("factor" %in% class(cluster)) {
    cluster <- droplevels(cluster)
  }

  if (nrow(model.matrix(model)) != length(cluster)) {
    stop(
      paste0(
        "Check your data: cluster variable has different N than model - ",
        "you may have observations with missing data."
      )
    )
  }

  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank

  dfc <- (M / (M - 1)) * ((N - 1) / (N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse.cov <- dfc * sandwich(model, meat. = crossprod(uj) / N)
  return(rcse.cov)
}
