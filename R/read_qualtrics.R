#' Read in Qualtrics Data
#'
#' @importFrom stats reshape
#' @importFrom utils read.csv
#' @export read.qualtrics

re.escape <- function(strings) {
  vals <- c(
    "\\\\", "\\[", "\\]", "\\(", "\\)",
    "\\{", "\\}", "\\^", "\\$", "\\*",
    "\\+", "\\?", "\\.", "\\|"
  )
  replace.vals <- paste0("\\\\", vals)
  for (i in seq_along(vals)) {
    strings <- gsub(vals[i], replace.vals[i], strings)
  }
  strings
}

read.qualtrics <- function(filename,
                           responses = NULL,
                           covariates = NULL,
                           respondentID = NULL,
                           letter = "F",
                           new.format = FALSE,
                           ranks = NULL) {
  ###### Load data and detect dimensions of things

  # Load CSV Results
  qualtrics_results <- read.csv(filename, stringsAsFactors = F)

  # Test whether it is of new format and modify the data
  if (new.format) {
    new.format.test <- grepl("ImportId", qualtrics_results[2, 1])
    if (new.format == new.format.test) {
      print("New qualtrics format detected.")
      qualtrics_results <- qualtrics_results[-2, ]
    } else {
      stop(
        paste0(
          "You indicate the data is of new qualtrics format, ",
          "but it seems to be in old format."
        )
      )
      return(NULL)
    }
  }

  if (!new.format) {
    new.format.test <- grepl("ImportId", qualtrics_results[2, 1])
    if (new.format == new.format.test) {
      print("Old qualtrics format detected.")
    } else {
      stop(
        paste0(
          "You indicate the data is of old qualtrics format, ",
          "but it seems to be in new format."
        )
      )
      return(NULL)
    }
  }

  # Extract variable names/question names
  var_names <- as.character(qualtrics_results[1, ])
  q_names <- colnames(qualtrics_results)
  # The rest is the raw data
  qualtrics_data <- qualtrics_results[2:nrow(qualtrics_results), ]
  colnames(qualtrics_data) <- var_names
  # Make respondent index
  respondent_index <- 1:nrow(qualtrics_data)

  # Find the attribute names and number of tasks
  attr_regexp <- paste0(c("^", letter, "-[0-9]+-[0-9]+(?!-)"), collapse = "")
  attr_name_cols <- grep(attr_regexp, var_names, perl = TRUE)
  # remove trailing whitespace
  qualtrics_data[attr_name_cols] <-
    lapply(qualtrics_data[attr_name_cols], function(x) sub("\\s+$", "", x))
  # Parse to matrix
  attr_name_matrix <- matrix(
    unlist(strsplit(var_names[attr_name_cols], "-")),
    nrow = 3, ncol = length(attr_name_cols)
  )
  colnames(attr_name_matrix) <- var_names[attr_name_cols]
  attr_name_matrix <- attr_name_matrix[2:nrow(attr_name_matrix), ]
  attr_name_matrix <- as.data.frame(t(attr_name_matrix))

  num_tasks <- unique(as.integer(attr_name_matrix[, 1]))

  # Find the level names and number of profiles
  level_regexp <- paste0(c("^", letter, "-[0-9]+-[0-9]+-[0-9]+"), collapse = "")
  level_name_cols <- grep(level_regexp, var_names, perl = TRUE)
  num_profiles <- length(
    unique(do.call(rbind, strsplit(var_names[level_name_cols], "-"))[, 3])
  )

  # Convert to matrix
  level_name_matrix <- matrix(
    unlist(strsplit(var_names[level_name_cols], "-")),
    nrow = 4, ncol = length(level_name_cols)
  )
  colnames(level_name_matrix) <- var_names[level_name_cols]
  level_name_matrix <- level_name_matrix[2:nrow(level_name_matrix), ]
  level_name_matrix <- as.data.frame(t(level_name_matrix))

  # Unique attributes
  all_attr <- c()
  for (attr_vec in attr_name_cols) {
    all_attr <- c(all_attr, qualtrics_data[, attr_vec])
  }
  ## Remove any trailing white spaces in strings
  all_attr <- gsub(pattern = "\\s+$", replacement = "", all_attr)
  # no missing values
  unique_attr <- unique(all_attr)[nchar(unique(all_attr)) != 0]
  # and no na's
  unique_attr <- unique_attr[!is.na(unique_attr)]

  ####### Checks on input

  # Are there any responses or ranks
  if (is.null(responses) & is.null(ranks)) {
    stop("Either responses or ranks must be non-NULL")
    return(NULL)
  }

  # If there are responses, are there the right number?
  if (!is.null(responses) && length(num_tasks) != length(responses)) {
    # If number of responses doesn't match num_tasks
    stop(
      paste0(
        "Error: Number of response columns ",
        "doesn't equal number of tasks in data."
      )
    )
    return(NULL)
  }

  # If there are ranks, are there the right number?
  if (!is.null(ranks) && length(num_tasks) != length(ranks) / num_profiles) {
    # If number of ranks doesn't match num_tasks
    stop(
      paste0(
        "Error: Number of rank columns doesn't equal ",
        "number of tasks times number of profiles in data."
      )
    )
    return(NULL)
  }

  # If no attributes fit the description
  if (length(attr_name_cols) == 0) {
    stop(
      paste0(
        "Error: Cannot find any columns designating attributes and levels. ",
        "Please make sure the input file originated from a Qualtrics survey ",
        "designed using the Conjoint SDT."
      )
    )
    return(NULL)
  }

  # Check whether attribute columns are empty or not
  for (attr_column in attr_name_cols) {
    if (is.null(unique(qualtrics_data[, attr_column]))) {
      stop(
        paste0(
          "Error, attribute column ",
          var_names[attr_column],
          " has no attribute names - recommend deleting this column."
        )
      )
    } else if (
      unique(qualtrics_data[, attr_column])[1] == "" &
        length(unique(qualtrics_data[, attr_column])) == 1) {
      stop(
        paste0(
          "Error, attribute column ",
          var_names[attr_column],
          " has no attribute names - recommend deleting this column."
        )
      )
    }
  }

  # Check whether level columns are empty or not
  for (lev_column in level_name_cols) {
    if (is.null(unique(qualtrics_data[, lev_column]))) {
      stop(
        paste0(
          "Error, level column ", var_names[lev_column],
          " has no attribute names - recommend deleting this column."
        )
      )
    } else if (
      unique(qualtrics_data[, lev_column])[1] == "" &
        length(unique(qualtrics_data[, lev_column])) == 1) {
      stop(
        paste0(
          "Error, level column ", var_names[lev_column],
          " has no attribute names - recommend deleting this column."
        )
      )
    }
  }

  # If respondentID is not null
  if (!is.null(respondentID)) {
    respondent_index <- qualtrics_data[, which(q_names %in% respondentID)]
  } else {
    respondent_index <- 1:nrow(qualtrics_data)
  }

  # Get the response rows
  if (is.character(responses[1])) {
    response_vars <- which(q_names %in% responses)
  } else {
    response_vars <- responses
  }

  # Make Sure no reserved characters are used in attribute names
  if (sum(grepl("[\\'\"]", unique(all_attr))) > 0) {
    stop(
      paste0(
        "Error, attribute name has special characters. ",
        "Some special characters are reserved for this function ",
        "(if cjoint>v2.0.6) for the purpose of efficiency. ",
        "If you still want to display special character in your plot, ",
        "use the argument attribute.names in the plot function. ",
        "See manual for more details."
      )
    )
  } else {
    # grepl(
    #   paste0(
    #     "^", unique(all_attr[unique(all_attr) != ""]), "_[0-9]+-[0-9]+$"
    #   ),
    # )
    if (sum(grepl("^attribute_[0-9]+$", unique(all_attr))) > 0) {
      stop(
        paste0("Error, attribute_[0-9]+ is reserved for the function.")
      )
    }

    if (sum(grepl("^selected_[0-9]+-[0-9]+$", unique(all_attr))) > 0) {
      stop(
        paste0("Error, selected_[0-9]+-[0-9]+ is reserved for the function.")
      )
    }
  }

  # Initialize output dataframe
  colnames(qualtrics_data)[which(q_names %in% covariates)] <- covariates
  out_data_set_cols <- c(
    which(q_names %in% respondentID),
    which(q_names %in% covariates),
    (attr_name_cols),
    (level_name_cols)
  )
  out_data_dataset <- qualtrics_data[, out_data_set_cols]

  # Take care of null respondentID case
  if (!is.null(respondentID)) {
    id_var_name <- colnames(out_data_dataset)[which(q_names %in% respondentID)]
  } else {
    out_data_dataset <- cbind(out_data_dataset, respondent_index)
    id_var_name <- "respondent_index"
  }

  # Parameters
  num_tasks <- unique(as.integer(attr_name_matrix[, 1]))
  num_profiles <- as.integer(unique(level_name_matrix[, 2]))
  num_attr <- unique(as.integer(attr_name_matrix[, 2]))

  # Replace all - with _ in "F-X-Y"
  attr_regexp <-
    paste0(c("^", letter, "-[0-9]+-[0-9]+$"), collapse = "")
  temp.col.index <- grep(attr_regexp, colnames(out_data_dataset), perl = TRUE)
  colnames(out_data_dataset)[temp.col.index] <-
    gsub("-", "_", colnames(out_data_dataset)[temp.col.index])

  # Replace all - with _ in "F-X-Y-Z"
  level_regexp <-
    paste0(c("^", letter, "-[0-9]+-[0-9]+-[0-9]+$"), collapse = "")
  temp.col.index <- grep(level_regexp, colnames(out_data_dataset), perl = TRUE)
  colnames(out_data_dataset)[temp.col.index] <-
    gsub("-", "_", colnames(out_data_dataset)[temp.col.index])

  # Clean attribute names
  for (i in num_attr) {
    temp.cmd <- paste0(
      "out_data_dataset['attribute_", i, "']<-",
      "out_data_dataset['", letter, "_1_", i, "']"
    )
    eval(parse(text = temp.cmd))
  }

  temp_regexp <- paste0(c("^", letter, "_[0-9]+_[0-9]+$"), collapse = "")
  temp.col.index <- grep(temp_regexp, colnames(out_data_dataset), perl = TRUE)
  out_data_dataset <- out_data_dataset[, -temp.col.index]

  # Test Selected
  test.selected <- sum(
    !unique(unlist(qualtrics_data[, response_vars])) %in% c("", num_profiles)
  ) == 0

  if (!test.selected) {
    stop(
      paste0(
        "Responses can only take values among (",
        paste0(num_profiles, collapse = ","), ")"
      )
    )
    return(NULL)
  }

  # Generate Selected
  if (is.null(ranks)) {
    for (i in num_tasks) {
      temp.cmd <- paste0(
        "temp.selected", "<-", "qualtrics_data[,", response_vars[i], "]"
      )
      eval(parse(text = temp.cmd))
      for (j in num_profiles) {
        temp.cmd <- paste0(
          "out_data_dataset$'selected_",
          j, "-", i, "'<-", "ifelse(temp.selected==j,1,0)"
        )
        eval(parse(text = temp.cmd))
        temp.cmd <- paste0(
          "out_data_dataset$'selected_",
          j, "-", i, "'[temp.selected", "=='']", "<-", "''"
        )
        eval(parse(text = temp.cmd))
      }
    }
  } else {
    ranks_col <- which(q_names %in% ranks)
    for (i in num_tasks) {
      for (j in num_profiles) {
        temp.cmd <- paste0(
          "out_data_dataset$'selected_",
          j, "-", i, "'<-", "qualtrics_data[,",
          ranks_col[(i - 1) * length(num_profiles) + j], "]"
        )
        eval(parse(text = temp.cmd))
      }
    }
  }

  # Remove row if attribute name is empty and trim all attribute entries
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)

  for (i in num_attr) {
    temp.cmd <- paste0(
      "out_data_dataset",
      "<-subset(out_data_dataset, attribute_", i, "!='')"
    )
    eval(parse(text = temp.cmd))
    temp.cmd <- paste0(
      "out_data_dataset['attribute_", i, "'] <- ",
      "trim(out_data_dataset$'attribute_", i, "')"
    )
    eval(parse(text = temp.cmd))
  }

  # Generate Attributes
  attribute_var_names <- unique(
    unlist(
      out_data_dataset[, grep("attribute_[0-9]+$", colnames(out_data_dataset))]
    )
  )
  attribute_var_names_label <- gsub(" ", ".", attribute_var_names)

  for (i in num_tasks) {
    for (j in num_profiles) {
      for (r in 1:length(attribute_var_names)) {
        temp.cmd <- paste0(
          "out_data_dataset['",
          attribute_var_names[r], "_", j, "-", i, "']<-''"
        )
        eval(parse(text = temp.cmd))
        temp.cmd <- paste0(
          "out_data_dataset['",
          attribute_var_names[r], ".rowpos_", j, "-", i, "']<-''"
        )
        eval(parse(text = temp.cmd))
      }
    }
  }

  for (i in num_tasks) {
    for (j in num_profiles) {
      for (k in num_attr) {
        for (r in attribute_var_names) {
          temp.cmd <- paste0(
            "out_data_dataset['", r, "_", j, "-", i, "']",
            "[out_data_dataset['attribute_", k, "']=='", r,
            "']<-out_data_dataset['", letter, "_", i, "_", j, "_", k,
            "'][out_data_dataset['attribute_", k, "']=='", r, "']"
          )
          eval(parse(text = temp.cmd))

          temp.cmd <- paste0(
            "out_data_dataset['", r, ".rowpos", "_", j, "-", i, "'",
            "][out_data_dataset['attribute_", k, "']=='", r,
            "']<-k"
          )
          eval(parse(text = temp.cmd))
        }
      }
    }
  }

  temp_regexp <- paste0(c("^", letter, "_[0-9]+_[0-9]+_[0-9]+$"), collapse = "")
  temp.col.index <- grep(temp_regexp, colnames(out_data_dataset), perl = TRUE)
  out_data_dataset <- out_data_dataset[, -temp.col.index]

  # Delete attribute names
  regex.temp <- paste0("^attribute", "_[0-9]+", "$")
  out_data_dataset <-
    out_data_dataset[, -grep(regex.temp, colnames(out_data_dataset))]

  # Reshape the dataset Batch 1 - Round/Task
  regex.temp <- paste0(
    paste0("^", re.escape(attribute_var_names), "_[0-9]+-[0-9]+", "$"),
    collapse = "|"
  )
  regex.temp.2 <- paste0(
    paste0("^", re.escape(attribute_var_names), ".rowpos_[0-9]+-[0-9]+", "$"),
    collapse = "|"
  )

  varying.temp <-
    colnames(out_data_dataset)[grep(regex.temp, colnames(out_data_dataset))]
  varying.temp.2 <-
    colnames(out_data_dataset)[grep(regex.temp.2, colnames(out_data_dataset))]
  varying.temp.3 <-
    colnames(out_data_dataset)[
      grep("^selected_[0-9]+-[0-9]+$", colnames(out_data_dataset))
    ]

  varying.temp <- c(varying.temp, varying.temp.2, varying.temp.3)

  v.names.temp <- unique(gsub("-[0-9]+$", "", varying.temp))
  v.names.temp <- v.names.temp[order(v.names.temp)]

  varying.temp <- paste0(
    rep(v.names.temp, length(num_tasks)), "-",
    rep(num_tasks, each = length(v.names.temp))
  )

  out_data_dataset <- reshape(
    out_data_dataset,
    idvar = id_var_name,
    varying = varying.temp,
    sep = "-",
    timevar = "task",
    times = num_tasks,
    v.names = v.names.temp,
    new.row.names = 1:(length(num_tasks) * nrow(out_data_dataset)),
    direction = "long"
  )

  # Reshape the dataset Batch 2 - Profile
  regex.temp <- paste0(
    paste0("^", re.escape(attribute_var_names), "_[0-9]+", "$"),
    collapse = "|"
  )
  regex.temp.2 <- paste0(
    paste0("^", re.escape(attribute_var_names), ".rowpos_[0-9]+", "$"),
    collapse = "|"
  )

  varying.temp <-
    colnames(out_data_dataset)[grep(regex.temp, colnames(out_data_dataset))]
  varying.temp.2 <-
    colnames(out_data_dataset)[grep(regex.temp.2, colnames(out_data_dataset))]
  varying.temp.3 <-
    colnames(out_data_dataset)[
      grep("^selected_[0-9]+$", colnames(out_data_dataset))
    ]
  varying.temp <- c(varying.temp, varying.temp.2, varying.temp.3)

  v.names.temp <- unique(gsub("_[0-9]+$", "", varying.temp))
  v.names.temp <- v.names.temp[order(v.names.temp)]

  varying.temp <- paste0(
    rep(v.names.temp, length(num_profiles)), "_",
    rep(num_profiles, each = length(v.names.temp))
  )

  out_data_dataset <- reshape(out_data_dataset,
    idvar = id_var_name,
    varying = varying.temp,
    sep = "_",
    timevar = "profile",
    times = num_profiles,
    v.names = v.names.temp,
    new.row.names = 1:(length(num_profiles) * nrow(out_data_dataset)),
    direction = "long"
  )

  ## Post-processiong
  colnames(out_data_dataset) <- gsub(" ", ".", colnames(out_data_dataset))

  for (m in attribute_var_names_label) {
    out_data_dataset[[m]] <- as.factor(out_data_dataset[[m]])
  }

  colnames(out_data_dataset)[
    which(colnames(out_data_dataset) == id_var_name)
  ] <- "respondent"
  out_data_dataset$respondentIndex <-
    as.factor(out_data_dataset$respondent)
  out_data_dataset$respondentIndex <-
    as.integer(out_data_dataset$respondentIndex)
  out_data_dataset$selected <-
    as.integer(out_data_dataset$selected)
  out_data_dataset$task <-
    as.integer(out_data_dataset$task)
  out_data_dataset$profile <-
    as.integer(out_data_dataset$profile)

  # Return dataset
  return(out_data_dataset)
}

#' Read in Qualtrics Data (The Output from qualtRics Package)
#'
#' @export read.with.qualtRics

read.with.qualtRics <- function(filename,
                                responses = NULL,
                                covariates = NULL,
                                respondentID = NULL,
                                letter = "F",
                                new.format = FALSE,
                                ranks = NULL) {
  # Load CSV Results
  qualtrics_results <- filename

  # Replace all . with -
  replace.regexp <- paste0("^", letter, ".[0-9]+.[0-9]+$")
  replace.col <- grep(replace.regexp, colnames(filename))
  colnames(qualtrics_results)[replace.col] <-
    gsub("[.]", "-", colnames(qualtrics_results)[replace.col])

  replace.regexp <- paste0("^", letter, ".[0-9]+.[0-9]+.[0-9]+$")
  replace.col <- grep(replace.regexp, colnames(filename))
  colnames(qualtrics_results)[replace.col] <-
    gsub("[.]", "-", colnames(qualtrics_results)[replace.col])

  # Extract variable names/question names
  var_names <- as.character(qualtrics_results[1, ])
  q_names <- colnames(qualtrics_results)
  # The rest is the raw data
  qualtrics_data <- qualtrics_results[2:nrow(qualtrics_results), ]
  colnames(qualtrics_data) <- var_names
  # Make respondent index
  respondent_index <- 1:nrow(qualtrics_data)

  # Find the attribute names and number of tasks
  attr_regexp <- paste0(c("^", letter, "-[0-9]+-[0-9]+(?!-)"), collapse = "")
  attr_name_cols <- grep(attr_regexp, var_names, perl = TRUE)
  # remove trailing whitespace
  qualtrics_data[attr_name_cols] <- lapply(
    qualtrics_data[attr_name_cols], function(x) sub("\\s+$", "", x)
  )
  # Parse to matrix
  attr_name_matrix <- matrix(
    unlist(strsplit(var_names[attr_name_cols], "-")),
    nrow = 3, ncol = length(attr_name_cols)
  )
  colnames(attr_name_matrix) <- var_names[attr_name_cols]
  attr_name_matrix <- attr_name_matrix[2:nrow(attr_name_matrix), ]
  attr_name_matrix <- as.data.frame(t(attr_name_matrix))

  num_tasks <- unique(as.integer(attr_name_matrix[, 1]))

  # Find the level names and number of profiles
  level_regexp <- paste0(c("^", letter, "-[0-9]+-[0-9]+-[0-9]+"), collapse = "")
  level_name_cols <- grep(level_regexp, var_names, perl = TRUE)
  num_profiles <- length(
    unique(do.call(rbind, strsplit(var_names[level_name_cols], "-"))[, 3])
  )

  # Convert to matrix
  level_name_matrix <- matrix(
    unlist(strsplit(var_names[level_name_cols], "-")),
    nrow = 4, ncol = length(level_name_cols)
  )
  colnames(level_name_matrix) <- var_names[level_name_cols]
  level_name_matrix <- level_name_matrix[2:nrow(level_name_matrix), ]
  level_name_matrix <- as.data.frame(t(level_name_matrix))

  # Unique attributes
  all_attr <- c()
  for (attr_vec in attr_name_cols) {
    all_attr <- c(all_attr, qualtrics_data[, attr_vec])
  }
  ## Remove any trailing white spaces in strings
  all_attr <- gsub(pattern = "\\s+$", replacement = "", all_attr)
  # no missing values
  unique_attr <- unique(all_attr)[nchar(unique(all_attr)) != 0]
  # and no na's
  unique_attr <- unique_attr[!is.na(unique_attr)]

  ####### Checks on input

  # Are there any responses or ranks
  if (is.null(responses) & is.null(ranks)) {
    stop("Either responses or ranks must be non-NULL.")
    return(NULL)
  }

  # If there are responses, are there the right number?
  if (!is.null(responses) && length(num_tasks) != length(responses)) {
    # If number of responses doesn't match num_tasks
    stop(
      paste0(
        "Error: Number of response columns ",
        "doesn't equal number of tasks in data."
      )
    )
    return(NULL)
  }

  # If there are ranks, are there the right number?
  if (!is.null(ranks) && length(num_tasks) != length(ranks) / num_profiles) {
    # If number of ranks doesn't match num_tasks
    stop(
      paste0(
        "Error: Number of rank columns doesn't equal ",
        "number of tasks times number of profiles in data."
      )
    )
    return(NULL)
  }

  # If no attributes fit the description
  if (length(attr_name_cols) == 0) {
    stop(
      paste0(
        "Error: Cannot find any columns designating attributes and levels. ",
        "Please make sure the input file originated from a Qualtrics survey ",
        "designed using the Conjoint SDT."
      )
    )
    return(NULL)
  }

  # Check whether attribute columns are empty or not
  for (attr_column in attr_name_cols) {
    if (is.null(unique(qualtrics_data[, attr_column]))) {
      stop(
        paste0(
          "Error, attribute column ",
          var_names[attr_column],
          " has no attribute names - recommend deleting this column"
        )
      )
    } else if (
      unique(qualtrics_data[, attr_column])[1] == "" &
        length(unique(qualtrics_data[, attr_column])) == 1) {
      stop(
        paste0(
          "Error, attribute column ",
          var_names[attr_column],
          " has no attribute names - recommend deleting this column"
        )
      )
    }
  }

  # Check whether level columns are empty or not
  for (lev_column in level_name_cols) {
    if (is.null(unique(qualtrics_data[, lev_column]))) {
      stop(
        paste0(
          "Error, level column ",
          var_names[lev_column],
          " has no attribute names - recommend deleting this column"
        )
      )
    } else if (
      unique(qualtrics_data[, lev_column])[1] == "" &
        length(unique(qualtrics_data[, lev_column])) == 1) {
      stop(
        paste0(
          "Error, level column ",
          var_names[lev_column],
          " has no attribute names - recommend deleting this column"
        )
      )
    }
  }


  # If respondentID is not null
  if (!is.null(respondentID)) {
    respondent_index <- qualtrics_data[, which(q_names %in% respondentID)]
  } else {
    respondent_index <- 1:nrow(qualtrics_data)
  }

  # Get the response rows
  if (is.character(responses[1])) {
    response_vars <- which(q_names %in% responses)
  } else {
    response_vars <- responses
  }

  # Make Sure no reserved characters are used in attribute names
  if (sum(grepl("[\\'\"]", unique(all_attr))) > 0) {
    stop(
      paste0(
        "Error, attribute name has special characters. ",
        "Some special characters are reserved for this function ",
        "(if cjoint>v2.0.6) for the purpose of efficiency. ",
        "If you still want to display special character in your plot, ",
        "use the argument attribute.names in the plot function. ",
        "See manual for more details."
      )
    )
  } else {
    # grepl(
    #   paste0(
    #     "^", unique(all_attr[unique(all_attr) != ""]), "_[0-9]+-[0-9]+$"
    #   ),
    # )
    if (sum(grepl("^attribute_[0-9]+$", unique(all_attr))) > 0) {
      stop(
        paste0("Error, attribute_[0-9]+ is reserved for the function.")
      )
    }

    if (sum(grepl("^selected_[0-9]+-[0-9]+$", unique(all_attr))) > 0) {
      stop(
        paste0("Error, selected_[0-9]+-[0-9]+ is reserved for the function.")
      )
    }
  }

  # Initialize output dataframe
  colnames(qualtrics_data)[which(q_names %in% covariates)] <- covariates
  out_data_set_cols <- c(
    which(q_names %in% respondentID),
    which(q_names %in% covariates),
    (attr_name_cols),
    (level_name_cols)
  )
  out_data_dataset <- qualtrics_data[, out_data_set_cols]

  # Take care of null respondentID case
  if (!is.null(respondentID)) {
    id_var_name <- colnames(out_data_dataset)[which(q_names %in% respondentID)]
  } else {
    out_data_dataset <- cbind(out_data_dataset, respondent_index)
    id_var_name <- "respondent_index"
  }

  # Parameters
  num_tasks <- unique(as.integer(attr_name_matrix[, 1]))
  num_profiles <- as.integer(unique(level_name_matrix[, 2]))
  num_attr <- unique(as.integer(attr_name_matrix[, 2]))

  # Replace all - with _ in "F-X-Y"
  attr_regexp <-
    paste0(c("^", letter, "-[0-9]+-[0-9]+$"), collapse = "")
  temp.col.index <- grep(attr_regexp, colnames(out_data_dataset), perl = TRUE)
  colnames(out_data_dataset)[temp.col.index] <-
    gsub("-", "_", colnames(out_data_dataset)[temp.col.index])

  # Replace all - with _ in "F-X-Y-Z"
  level_regexp <-
    paste0(c("^", letter, "-[0-9]+-[0-9]+-[0-9]+$"), collapse = "")
  temp.col.index <- grep(level_regexp, colnames(out_data_dataset), perl = TRUE)
  colnames(out_data_dataset)[temp.col.index] <-
    gsub("-", "_", colnames(out_data_dataset)[temp.col.index])

  # Clean attribute names
  for (i in num_attr) {
    temp.cmd <- paste0(
      "out_data_dataset['attribute_", i,
      "']<-", "out_data_dataset['", letter, "_1_", i, "']"
    )
    eval(parse(text = temp.cmd))
  }

  temp_regexp <- paste0(c("^", letter, "_[0-9]+_[0-9]+$"), collapse = "")
  temp.col.index <- grep(temp_regexp, colnames(out_data_dataset), perl = TRUE)
  out_data_dataset <- out_data_dataset[, -temp.col.index]

  # Test Selected
  test.selected <- sum(
    !unique(unlist(qualtrics_data[, response_vars])) %in%
      c("", num_profiles)
  ) == 0

  if (!test.selected) {
    stop(
      paste0(
        "Responses can only take values among (",
        paste0(num_profiles, collapse = ","), ")"
      )
    )
    return(NULL)
  }

  # Generate Selected
  if (is.null(ranks)) {
    for (i in num_tasks) {
      temp.cmd <- paste0(
        "temp.selected", "<-", "qualtrics_data[,", response_vars[i], "]"
      )
      eval(parse(text = temp.cmd))
      for (j in num_profiles) {
        temp.cmd <- paste0(
          "out_data_dataset$'selected_",
          j, "-", i, "'<-", "ifelse(temp.selected==j,1,0)"
        )
        eval(parse(text = temp.cmd))
        temp.cmd <- paste0(
          "out_data_dataset$'selected_",
          j, "-", i, "'[temp.selected", "=='']", "<-", "''"
        )
        eval(parse(text = temp.cmd))
      }
    }
  } else {
    ranks_col <- which(q_names %in% ranks)
    for (i in num_tasks) {
      for (j in num_profiles) {
        temp.cmd <- paste0(
          "out_data_dataset$'selected_",
          j, "-", i, "'<-", "qualtrics_data[,",
          ranks_col[(i - 1) * length(num_profiles) + j], "]"
        )
        eval(parse(text = temp.cmd))
      }
    }
  }

  # Remove row if attribute name is empty and trim all attribute entries
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)

  for (i in num_attr) {
    temp.cmd <- paste0(
      "out_data_dataset",
      "<-subset(out_data_dataset, attribute_", i, "!='')"
    )
    eval(parse(text = temp.cmd))
    temp.cmd <- paste0(
      "out_data_dataset['attribute_", i, "'] <- ",
      "trim(out_data_dataset$'attribute_", i, "')"
    )
    eval(parse(text = temp.cmd))
  }

  # Generate Attributes
  attribute_var_names <- unique(
    unlist(
      out_data_dataset[
        , grep("attribute_[0-9]+$", colnames(out_data_dataset))
      ]
    )
  )
  attribute_var_names_label <- gsub(" ", ".", attribute_var_names)

  for (i in num_tasks) {
    for (j in num_profiles) {
      for (r in 1:length(attribute_var_names)) {
        temp.cmd <- paste0(
          "out_data_dataset['",
          attribute_var_names[r], "_", j, "-", i, "']<-''"
        )
        eval(parse(text = temp.cmd))
        temp.cmd <- paste0(
          "out_data_dataset['",
          attribute_var_names[r], ".rowpos_", j, "-", i, "']<-''"
        )
        eval(parse(text = temp.cmd))
      }
    }
  }

  for (i in num_tasks) {
    for (j in num_profiles) {
      for (k in num_attr) {
        for (r in attribute_var_names) {
          temp.cmd <- paste0(
            "out_data_dataset['", r, "_", j, "-", i, "']",
            "[out_data_dataset['attribute_", k, "']=='", r,
            "']<-out_data_dataset['", letter, "_", i, "_", j, "_", k,
            "'][out_data_dataset['attribute_", k, "']=='", r, "']"
          )
          eval(parse(text = temp.cmd))

          temp.cmd <- paste0(
            "out_data_dataset['", r, ".rowpos", "_", j, "-", i, "'",
            "][out_data_dataset['attribute_", k, "']=='", r,
            "']<-k"
          )
          eval(parse(text = temp.cmd))
        }
      }
    }
  }

  temp_regexp <- paste0(c("^", letter, "_[0-9]+_[0-9]+_[0-9]+$"), collapse = "")
  temp.col.index <- grep(temp_regexp, colnames(out_data_dataset), perl = TRUE)
  out_data_dataset <- out_data_dataset[, -temp.col.index]

  # Delete attribute names
  regex.temp <- paste0("^attribute", "_[0-9]+", "$")
  out_data_dataset <-
    out_data_dataset[, -grep(regex.temp, colnames(out_data_dataset))]

  # Reshape the dataset Batch 1 - Round/Task
  regex.temp <- paste0(
    paste0("^", re.escape(attribute_var_names), "_[0-9]+-[0-9]+", "$"),
    collapse = "|"
  )
  regex.temp.2 <- paste0(
    paste0("^", re.escape(attribute_var_names), ".rowpos_[0-9]+-[0-9]+", "$"),
    collapse = "|"
  )

  varying.temp <-
    colnames(out_data_dataset)[grep(regex.temp, colnames(out_data_dataset))]
  varying.temp.2 <-
    colnames(out_data_dataset)[grep(regex.temp.2, colnames(out_data_dataset))]
  varying.temp.3 <-
    colnames(out_data_dataset)[
      grep("^selected_[0-9]+-[0-9]+$", colnames(out_data_dataset))
    ]

  varying.temp <- c(varying.temp, varying.temp.2, varying.temp.3)

  v.names.temp <- unique(gsub("-[0-9]+$", "", varying.temp))
  v.names.temp <- v.names.temp[order(v.names.temp)]

  varying.temp <- paste0(
    rep(v.names.temp, length(num_tasks)), "-",
    rep(num_tasks, each = length(v.names.temp))
  )

  out_data_dataset <- reshape(
    out_data_dataset,
    idvar = id_var_name,
    varying = varying.temp,
    sep = "-",
    timevar = "task",
    times = num_tasks,
    v.names = v.names.temp,
    new.row.names = 1:(length(num_tasks) * nrow(out_data_dataset)),
    direction = "long"
  )

  # Reshape the dataset Batch 2 - Profile
  regex.temp <- paste0(
    paste0("^", re.escape(attribute_var_names), "_[0-9]+", "$"),
    collapse = "|"
  )
  regex.temp.2 <- paste0(
    paste0("^", re.escape(attribute_var_names), ".rowpos_[0-9]+", "$"),
    collapse = "|"
  )

  varying.temp <-
    colnames(out_data_dataset)[grep(regex.temp, colnames(out_data_dataset))]
  varying.temp.2 <-
    colnames(out_data_dataset)[grep(regex.temp.2, colnames(out_data_dataset))]
  varying.temp.3 <-
    colnames(out_data_dataset)[
      grep("^selected_[0-9]+$", colnames(out_data_dataset))
    ]
  varying.temp <- c(varying.temp, varying.temp.2, varying.temp.3)

  v.names.temp <- unique(gsub("_[0-9]+$", "", varying.temp))
  v.names.temp <- v.names.temp[order(v.names.temp)]

  varying.temp <- paste0(
    rep(v.names.temp, length(num_profiles)), "_",
    rep(num_profiles, each = length(v.names.temp))
  )

  out_data_dataset <- reshape(
    out_data_dataset,
    idvar = id_var_name,
    varying = varying.temp,
    sep = "_",
    timevar = "profile",
    times = num_profiles,
    v.names = v.names.temp,
    new.row.names = 1:(length(num_profiles) * nrow(out_data_dataset)),
    direction = "long"
  )

  ## Post-processiong
  colnames(out_data_dataset) <- gsub(" ", ".", colnames(out_data_dataset))

  for (m in attribute_var_names_label) {
    out_data_dataset[[m]] <- as.factor(out_data_dataset[[m]])
  }

  colnames(out_data_dataset)[
    which(colnames(out_data_dataset) == id_var_name)
  ] <- "respondent"
  out_data_dataset$respondentIndex <-
    as.factor(out_data_dataset$respondent)
  out_data_dataset$respondentIndex <-
    as.integer(out_data_dataset$respondentIndex)
  out_data_dataset$selected <-
    as.integer(out_data_dataset$selected)
  out_data_dataset$task <-
    as.integer(out_data_dataset$task)
  out_data_dataset$profile <-
    as.integer(out_data_dataset$profile)

  # Return dataset
  return(out_data_dataset)
}
