# -*- encoding: UTF-8 -*-
#' Perform Synthetic Minority Over-Sampling TEchnique (SMOTE) with Edited Nearest Neighbors (ENN) downsampling
#'
#' This function first performs Perform Synthetic Minority Over-Sampling TEchnique (SMOTE), then proceeds to perform Edited Nearest Neighbors (ENN) downsampling for the majority class instances in the dataset.
#'
#' @param data Data frame containing the dataset.
#'        It should include columns for predictors (features) and a response variable.
#'        The response variable should be a factor with two levels.
#' @return A data frame containing the downsampled majority class instance
#'         satisfying the ENN criteria.
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
#' data2 <- SMOTEENN(data)
#'
#' # notice that it did not detect any samples to be selected for down sampling
#'
#' @export
#' @name SMOTEENN

# Add library imports
library(doParallel)
library(dplyr)
library(foreach)
library(knitr)
library(tidyr)
library(magrittr)
library(smotefamily)



SMOTEENN <- function(data) {
  # Identify the minority class
  
  y = y
  
  minority_class <- names(sort(table(data$y), decreasing = FALSE))[1]

  # Count of instances in the minority class
  minority_count <- table(data$y)[minority_class]

  # Set 'K' based on the square root of the minority count
  K <- round(min(sqrt(minority_count)), 0)

  # Move 'y' to the beginning of the dataframe
  data <- data[, c("y", setdiff(names(data), "y"))]

  # Perform SMOTE
  smote_result <- SMOTE(data[, -1], data[, 1], K = K, dup_size = 1)

  # Extract synthesized data
  syn_data <- smote_result$syn_data

  # Rename class column as 'y'
  colnames(syn_data)[colnames(syn_data) == "class"] <- "y"

  # Bind synthetic data with original data
  data <- rbind(data, syn_data)

  # Convert 'y' to numeric
  data$y <- as.numeric(data$y)

  cat("Number of synthetic samples generated for the minority class '", minority_class, "':", sum(syn_data$y == minority_class), "\n")

  # Apply ENN
  edited_data <- SMOTEENN::ENN(data)

  # Delete the majority instances
  data <- data[data$y != edited_data$y[1], ]

  majority_class <- edited_data$y[1]

  # Combine SMOTE-ENN downsampled majority with minority class
  dataENN <- rbind(data, edited_data)

  # Move 'y' to the beginning of the df
  dataENN <- dataENN %>%
    select(y, everything())

  # Rename the column
  dataENN <- dataENN %>%
    rename(y = y)

  # Make the variables numeric and create a data frame
  dfENN <- as.data.frame(apply(dataENN, 2, as.numeric))

  # # Calculate and print the frequency difference
  # freq_diff <- abs(sum(syn_data$y == majority_class) - sum(edited_data$y == majority_class))
  # cat("Frequency downsampled majority class'", majority_class, "':", freq_diff, "\n")


  return(dfENN)
}


