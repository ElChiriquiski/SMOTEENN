# -*- encoding: UTF-8 -*-
#' Calculate Euclidean distance between two points
#'
#' This function calculates the Euclidean distance between two points.
#'
#' @keywords internal
#' @name euclidean_distance
#' @param a Numeric vector representing the first point.
#' @param b Numeric vector representing the second point.
#' @return The Euclidean distance between points \code{a} and \code{b}.
#'
#' @export


# Add library imports

euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}