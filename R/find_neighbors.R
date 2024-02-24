# -*- encoding: UTF-8 -*-
#' Find k nearest neighbors for a given instance
#'
#' This function finds the indices of the k nearest neighbors for a given instance
#' in a dataset using Euclidean distance.
#'
#' @keywords internal
#' @name find_neighbors
#' @param data Data frame containing the dataset.
#' @param instance Numeric vector representing the instance for which neighbors are sought.
#' @param k Number of neighbors to find.
#' @return A vector of indices corresponding to the k nearest neighbors.
#'
#' @export

# Add library imports
library(doParallel)
library(dplyr)
library(foreach)
library(knitr)
library(tidyr)

find_neighbors <- function(data, instance, k) {
  distances <- apply(data[, -ncol(data)], 1, function(x) euclidean_distance(instance, x))
  neighbors <- order(distances)[1:k]
  return(neighbors)
}