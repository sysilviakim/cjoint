#' Dependencies
#' 
#' @param J Input attribute
#' @param tol Tolerance

compute_dependencies <- function(J, tol = 1e-14) {
  # Get attribute names
  attribute_names <- names(dimnames(J))
  # If only one attribute, no dependence
  if (length(attribute_names) == 1) {
    dependency_list <- list()
    dependency_list[[attribute_names[1]]] <- c()
    return(dependency_list)
  } else {
    # Create list for each attribute_name
    dependency_list <- list()
    for (k in attribute_names) {
      dependency_list[[k]] <- c()
    }
    # Loop over each pair of attributes - figure out if they're independent
    for (i in 1:(length(attribute_names) - 1)) {
      for (j in (i + 1):length(attribute_names)) {
        attr1 <- attribute_names[i]
        attr2 <- attribute_names[j]
        cross_tab <- apply(J, c(attr1, attr2), sum)
        # Standardize
        sums <- apply(cross_tab, 1, sum)
        cross_tab_std <- cross_tab / sums
        # Compute similarities
        is_equal <- TRUE
        r_1 <- cross_tab_std[1, ]
        if (nrow(cross_tab_std) > 1) {
          for (m in 2:nrow(cross_tab_std)) {
            if (any(as.vector(r_1) - as.vector(cross_tab_std[m, ]) > tol)) {
              is_equal <- FALSE
            }
          }
        }
        
        # If not the same, append to dependency dictionary
        if (!is_equal) {
          dependency_list[[attr1]] <- c(dependency_list[[attr1]], attr2)
          dependency_list[[attr2]] <- c(dependency_list[[attr2]], attr1)
        }
      }
    }
    return(dependency_list)
  }
}
