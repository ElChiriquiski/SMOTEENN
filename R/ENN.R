# -*- encoding: UTF-8 -*-
#' Perform Edited Nearest Neighbors (ENN) downsampling
#'
#' This function performs Edited Nearest Neighbors (ENN) downsampling
#' for the majority class instances in the dataset.
#'
#' @param data Data frame containing the dataset.
#'        It should include columns for predictors (features) and a response variable.
#'        The response variable should be a factor with two levels.
#' @return A data frame containing the downsampled majority class instance
#'         satisfying the ENN criteria.
#'
#
#'
#' @examples
#'
#' # Recommended - Apply Parallel Processing
#' library(doParallel)
#' library(dplyr)
#' library(magrittr)
#' library(foreach)
#' library(knitr)
#' library(tidyr)
#' #registerDoParallel(detectCores())
#' #Alternative max level of cores, but need to manually set on certain machines due to restrictions
#' registerDoParallel(2)
#'
#' #load data
#' data(iris)
#'
#' # rename df
#' df <- iris
#'
#' #make setosa binary
#' df$setosaYN <- as.numeric(df$Species == "setosa")
#'
#' # Remove the "Species" column
#' df <- df[, -5]
#'
#' # Remove the "Species" column and move "setosaYN" to the beginning
#' df <- df[, c("setosaYN", setdiff(names(df), c("Species", "setosaYN")))]
#'
#' # Preprocess data into specified df
#' X <- df[, -1]
#' y <- df[, 1]
#' data <- data.frame(X, y)
#'
#' # Apply ENN Majority Class Down Sampling and output df
#' data2 <- ENN(data)
#'
#' # notice that it did not detect any samples to be selected for down sampling
#'
#' @export
#' @name ENN


# Add library imports
library(doParallel)
library(dplyr)
library(foreach)
library(knitr)
library(tidyr)
library(magrittr)

ENN <- function(data) {
  majority_class <- names(sort(table(data$y), decreasing = TRUE))[1]
  majority_indices <- which(data$y == majority_class)
  majority_count <- table(data$y)[majority_class]

  k <- sqrt(majority_count)
  i = majority_indices



  # Use foreach for parallel processing
  edited_data <- foreach::foreach(i = majority_indices, .combine = rbind) %dopar% {
    enn_downsample_instance(i, data, k)
  }

  # Remove NULL entries and combine into a data frame
  edited_data <- do.call(rbind, edited_data[sapply(edited_data, Negate(is.null))])

  # Transpose the data frame
  edited_data <- t(edited_data)

  # Make data frame
  edited_data <- as.data.frame(edited_data)

  # Identify the name of the last column
  last_column <- names(edited_data)[ncol(edited_data)]
  # Move the last column to the beginning
  edited_data <- edited_data[, c(last_column, setdiff(names(edited_data), last_column))]

  edited_data <- as.data.frame(apply(edited_data, 2, as.numeric))

  # Calculate the frequency difference
  freq_diff <- majority_count - as.numeric(nrow(edited_data))

  # Print the result
  cat("Frequency down sampled for the majority class '", majority_class, "':", freq_diff, "\n")

  # Delete the majority instances
  data <- data[data$y != edited_data$y[1], ]

  majority_class <- edited_data$y[1]

  # Combine ENN downsampled majority with minority class
  dataENN <- rbind(data, edited_data)

  # Identify the name of the last column
  last_column <- names(data)[ncol(data)]
  # Move the last column to the beginning
  data <- data[, c(last_column, setdiff(names(data), last_column))]

  # Make the variables numeric and create a data frame
  edited_data <- as.data.frame(apply(dataENN, 2, as.numeric))


  return(edited_data)
}
