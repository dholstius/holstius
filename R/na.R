#' na.replace
#'
#' Replace NA values with another value
#'
#' @param x				a vector with NAs
#' @param replacement	replacement value
#' @export
na.replace <- function(x, replacement) 
{
	replace(x, is.na(x), replacement)
}