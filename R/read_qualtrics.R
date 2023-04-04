#' Replace Values for Escape Expressions
#' 
#' @param strings Input strings

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

#' Read Data from a Conjoint Qualtrics Experiment
#' 
#' Converts the raw .CSV data file downloaded from an online conjoint 
#' experiment run using the Qualtrics survey software into a data frame 
#' usable by the amce routine. Each row of the Qualtrics .CSV file is a 
#' single survey respondent. The rows of the resulting dataframe correspond 
#' to individual profile choices. Currently, the function supports various 
#' types of outcomes in a conjoint experiment, with details explained below.
#' 
#' @param filename A .CSV file containing responses exported from a Qualtrics 
#' survey experiment. The first row contains question and variable identifiers 
#' (e.g. V1, Q1). The second row contains variable names and question texts. 
#' Subsequent rows contain the answers of each respondent. 
#' This .CSV file can be exported through the "Download Data" panel in the 
#' Qualtrics web interface. Note that answers should be shown as 
#' "coded values" and not as choice text.
#' 
#' @param responses A character or integer vector with the identifiers of the 
#' CSV columns that contain the conjoint responses. The first element 
#' corresponds to the identifier of the first question, the second element 
#' corresponds to the second question and so on. The length of this vector 
#' must be equal to the number of conjoint tasks answered by each respondent. 
#' Identifiers are typically in the form "Q#" - e.g. c("Q1", "Q3", "Q5") 
#' would represent a three question survey where the conjoint questions are 
#' Q1, Q3 and Q5. If specified as an integer vector, the elements are the 
#' column numbers corresponding to each question response.
#' 
#' @param covariates A character vector denoting the column names of any 
#' other respondent-level characteristics measured in the survey that should 
#' be included in the resulting dataframe.
#' 
#' @param respondentID A character string denoting a column containing a unique 
#' identifier for each respondent (e.g. an IP address). This identifier will 
#' be carried over into the output. If NULL, each respondent will be given an 
#' arbitrary identifier in the output dataframe. Leave as NULL if you do not 
#' want responses to be linked back to a known respondent identifier.
#' 
#' @param new.format Indicator for whether the .csv file is from the new 
#' Qualtrics export format with three title rows (TRUE) or from the old format 
#' (FALSE) with two title rows. Defaults to FALSE.
#' 
#' @param ranks An integer vector with the identifiers of the CSV columns 
#' that contain the conjoint rankings or ratings.
#' 
#' @param letter The beginning letter used in the naming convention of levels 
#' and attributes.
#' 
#' @return A dataframe in which each row corresponds to a single profile. 
#' The column "selected" denotes whether that profile was selected 
#' by the respondent. The columns "respondent" and "task" denote the 
#' respondent and task numbers to which the profile was assigned. 
#' Respondent-level covariates are appended to each row.
#' 
#' @references Strezhnev, A., Hainmueller, J., Hopkins, D., and Yamamoto, T. 
#' (2014) Conjoint Survey Design Tool. 
#' http://scholar.harvard.edu/astrezhnev/conjoint-survey-design-tool
#' 
#' @details {
#'   This function currently only works with experiments that generate profiles using .PHP scripts created by the Conjoint Survey Design Tool. It also is only able to handle standard conjoint designs (binary outcome variable/forced choice).
#'   (http://scholar.harvard.edu/astrezhnev/conjoint-survey-design-tool).
#'
#'   For each respondent in the .CSV file, attribute and level names are stored using the following naming convention:
#'   Level Name: F-[task number]-[profile number]-[attribute number]
#'   Attribute Name: F-[task number]-[attribute number]
#'   Example: F-1-3-2 denotes the level corresponding to Task 1, Profile 3, Attribute 2
#'   F-3-3 denotes the attribute name corresponding to Task 3, Attribute 3
#'
#'   Special Characters: Some special characters have been reserved for this function if (cjoint>v2.0.6) for efficienty purpose. This means some special characters cannot be used in the attribute names in your data. However, if you still want to display special characters in attribute names in your plot, you may want to use the argument attribute.names in function plot.amce to customize the display of your attribute names.
#'
#'   East Asian Language Support: The read.qualtrics function relies on the read.csv function in R-core. The read.csv function only works well for some of character encoding, but not others, for East Asian languages in some Operation systems. In Windows, .csv files containing East Asian languages such as Chinese or Japanese should be stored in the ANSI encoding rather than UTF-8 for the read.csv function to work. Further, if you are reading the csv file in a Windows OS with a different display language as the file you are trying to read in, you will need to reconfigure R into the language of the csv file by Sys.setlocale().
#'
#'   Different types of responses: This function supports various types of responses commonly used in conjoint analyses. Here are some illustrations on some typical types.
#'
#'   1. The respondent is asked to fill in the profile she prefers the most, and her choice is restored in one response variable. In this case, set the argument responses=the response variable.
#'
#'   2. The respondent is asked to give each profile a rank within each task, and her ranks for each profile within each task are restored in J response variables, suppose there are J profiles within each task. In this case, set the argument ranks as a vector restoring the variables names of these responses variables, in the order of rank of profile 1 in task 1, rank of profile 2 in task 1, ... rank of profile J in task 1, rank of profile 1 in task 2, rank of profile 2 in task 2, ... rank of profile J in task 2, ..., rank of profile J in task K.
#' 
#'   3. The respondent is asked to rate each profile within each task, and her ratings for each profile within each task are restored in J response variables, suppose there are J profiles within each task. In this case, set the argument ratings as a vector restoring the variables names of these responses variables, in the order of rating of profile 1 in task 1, rating of profile 2 in task 1, ... rating of profile J in task 1, rating of profile 1 in task 2, rating of profile 2 in task 2, ... rating of profile J in task 2, ..., rating of profile J in task K.
#' 
#'   4. The respondent is asked to select the top L profiles she prefers within each task. L colud be smaller than the number of profiles J available within each task. Her choices are recorded with L variables for each task, indicating the first choice among J profiles in task 1, the second choice among J profiles in task 1, ..., the L-th choice among J profiles in task 1, the first choice among J profiles in task 2, the second choice among J profiles in task 2, ..., the L-th choice among J profiles in task 2, ..., the L-th choice among J profiles in task K. In this case, the read.qualtrics function should be applied in the following step:
#'  
#'     (a) Set the argument response as a vector restoring the variables names of the first choice among J profiles in all tasks, in the order of first choice among J profiles in task 1, first choice among J profiles in task 2, ..., first choice among J profiles in task K. Save the respective data output.
#' 
#'   (b) Set the argument response as a vector restoring the variables names of the second choice among J profiles in all tasks, in the order of second choice among J profiles in task 1, second choice among J profiles in task 2, ..., second choice among J profiles in task K. Save the respective data output.
#' 
#'   (c) Repeat until you have completed the above steps for all top L choices. And then merge all data according to their respondentID. However, you may need to change the variable name of the responses a little bit for a successful merge.
#' 
#'   5. The respondent is asked to select the top L profiles she prefers within each task. L colud be smaller than the number of profiles J available within each task. The questionnaire is designed in such format that the respondent fills in the ranking of top L profiles that she prefers, while leave the ranking of the other J-L profiles blank. In this case, set the argument ranks as a vector restoring the variables names of these J responses variables, in the order of rank of profile 1 in task 1, rank of profile 2 in task 1, ... rank of profile J in task 1, rank of profile 1 in task 2, rank of profile 2 in task 2, ... rank of profile J in task 2, ..., rank of profile J in task K.
#' }
#' 
#' @importFrom stats reshape
#' @importFrom utils read.csv
#' @export read.qualtrics

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
#' @param filename A dataframe containing responses download from a Qualtrics 
#' survey experiment with R package "qualtRics". Each row contains the answers 
#' of each respondent. Note that answers should be shown as "coded values" 
#' and not as choice text.
#' 
#' @param responses A character or integer vector with the identifiers of the 
#' CSV columns that contain the conjoint responses. The first element 
#' corresponds to the identifier of the first question, the second element 
#' corresponds to the second question and so on. The length of this vector 
#' must be equal to the number of conjoint tasks answered by each respondent. 
#' Identifiers are typically in the form "Q#" - e.g. c("Q1", "Q3", "Q5") 
#' would represent a three question survey where the conjoint questions 
#' are Q1, Q3 and Q5. If specified as an integer vector, the elements are the 
#' column numbers corresponding to each question response.
#' 
#' @param covariates A character vector denoting the column names of any other 
#' respondent-level characteristics measured in the survey that should be 
#' included in the resulting dataframe.
#' 
#' @param respondentID A character string denoting a column containing a 
#' unique identifier for each respondent (e.g. an IP address). 
#' This identifier will be carried over into the output. If NULL, 
#' each respondent will be given an arbitrary identifier in the output 
#' dataframe. Leave as NULL if you do not want responses to be linked back to 
#' a known respondent identifier.
#' 
#' @param new.format Indicator for whether the .csv file is from the new  
#' Qualtrics export format with three title rows (TRUE) or from the old 
#' format (FALSE) with two title rows. Defaults to FALSE.
#' 
#' @param ranks An integer vector with the identifiers of the CSV columns 
#' that contain the conjoint rankings or ratings.
#' 
#' @param letter The beginning letter used in the naming convention of 
#' levels and attributes.
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
