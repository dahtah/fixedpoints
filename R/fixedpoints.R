#' fixedpoints: a package for fixed points and function iterations
#'
#' Sometimes you need to apply the same mapping over and over again. Sometimes you need to apply the same mapping over and over again until nothing changes anymore. You could write loops to do that, or you could use the fixedpoints package.
#' This package provides a set of utilities for monitoring intermediate values, checking convergence, catching errors, etc. It does nothing you couldn't do with a while loop but it will save you some typing.
#' @docType package
#' @name fixedpoints
NULL

#' @importFrom purrr map map_df map_int map_dbl safely
#' @importFrom methods is
#' @importFrom magrittr "%>%"
NULL

