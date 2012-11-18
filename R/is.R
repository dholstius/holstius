#' is.constant
#'
#' Test if all elements of a vector have the same value
#'
#' @param x			vector
#' @param warn.na	warn if some elements are NA?
#'
#' @export
is.constant <- function(x, warn.na=FALSE) {
	if (isTRUE(warn.na)) {
		if (any(is.na(x))) warning("Some elements NA; results probably incorrect")
	}
	if (x[1] == x[length(x)]) {
		unsorted <- is.unsorted(x, strictly=FALSE)
		if (!unsorted) {
			return(TRUE)
		}
	}
	return(FALSE)
}

#' is.odd
#'
#' Test if elements of an integer vector are divisible by 2
#'
#' @param x			a numeric vector
#' @param quiet		suppress warnings about coercing to integer
#'
#' @export
is.odd <- function(x, quiet=TRUE) {
	if (!is.integer(x)) {
		if (!quiet) warning("Coercing to integer")
		x <- as.integer(x)
	}
	as.logical(x %% 2)
}

#' is.decimal
#'
#' Test if elements of a character vector are representations of decimal numbers
#'
#' @param x	a numeric vector
#'
#' @export
is.decimal <- function(x) {
	pattern <- '^[-+]?[0-9]*\\.?[0-9]+$'
	grepl(pattern, x)
}