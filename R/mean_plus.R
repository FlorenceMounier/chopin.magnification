# WARNING - Generated by {fusen} from dev/flat_descriptive_stats.Rmd: do not edit by hand # nolint: line_length_linter.

#' mean_plus Calculation of the rounded mean excluding missing values
#'
#' @param vector Vector containing values to be summarized
#'
#' @return mean value
#' @export

mean_plus <- function(vector){
      round(mean(vector, na.rm = TRUE), digits = 2)
}

#' median_plus Calculation of the rounded median excluding missing values
#'
#' @param vector Vector containing values to be summarized
#'
#' @return median value
#' @importFrom stats median
#' @export

median_plus <- function(vector){
      round(median(vector, na.rm = TRUE), digits = 2)
}

#' min_plus Calculation of the rounded minimum excluding missing values
#'
#' @param vector Vector containing values to be summarized
#'
#' @return minimal value
#' @export

min_plus <- function(vector){
      round(min(vector, na.rm = TRUE), digits = 2)
}

#' max_plus Calculation of the rounded maximum excluding missing values
#'
#' @param vector Vector containing values to be summarized
#'
#' @return maximal value
#' @export

max_plus <- function(vector){
      round(max(vector, na.rm = TRUE), digits = 2)
}

#' stats_mean Calculation of the mean, min and max values of a vector
#'
#' @param vector Vector containing values to be summarized
#'
#' @return character `mean[min-max]`
#' @export

stats_mean <- function(vector){
  paste0(mean_plus(vector), "[", min_plus(vector), "-", max_plus(vector), "]")
}

#' stats_median Calculation of the median, min and max values of a vector
#'
#' @param vector Vector containing values to be summarized
#'
#' @return character `median[min-max]`
#' @export

stats_median <- function(vector){
  paste0(median_plus(vector), "[", min_plus(vector), "-", max_plus(vector), "]")
}
