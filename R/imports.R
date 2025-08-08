# =============================================================================
# IMPORTS
# File: R/imports.R
# Part of CovariateSearcher Package
# Package imports and global definitions
# =============================================================================


#' NULL-coalescing operator
#'
#' @name null-coalesce
#' @rdname null-coalesce
#' @param x Left-hand side
#' @param y Right-hand side
#' @return x if not NULL, otherwise y
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x


