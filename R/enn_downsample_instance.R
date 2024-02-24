# -*- encoding: UTF-8 -*-
#' Perform Edited Nearest Neighbors (ENN) downsampling for a single instance
#'
#' This function performs Edited Nearest Neighbors (ENN) downsampling for a single instance.
#' If the majority class instance is misclassified by its neighbors, it is excluded.
#'
#' @keywords internal
#' @name enn_downsample_instance
#' @param i Index of the instance in the dataset.
#' @param data Data frame containing the dataset.
#' @param k Number of nearest neighbors to consider.
#' @return A data frame containing the instance if it satisfies the ENN criteria, otherwise NULL.
#'
#' @export

# Add library imports
library(doParallel)
library(dplyr)
library(foreach)
library(knitr)
library(tidyr)

enn_downsample_instance <- function(i, data, k) {
  neighbors <- find_neighbors(data[-i, ], data[i, -ncol(data)], k)
  
  # Check if the majority class instance is misclassified by its neighbors
  if (data$y[i] != data$y[neighbors][1]) {
    return(NULL)
  } else {
    return(data[i, ])
  }
}


