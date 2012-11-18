#' GM
#'
#' Geometric mean
#'
#' @param	x		numeric vector
#' @param	na.rm	ignore NAs?
#' @export
GM <- function(x, na.rm=TRUE) 
{
	exp(mean(log(x), na.rm=na.rm))
}

#' GSD
#'
#' Geometric standard deviation
#'
#' @param	x		numeric vector
#' @param	na.rm	ignore NAs?
#' @export
GSD <- function(x, na.rm=TRUE) 
{
	if (na.rm) {
		x <- na.omit(x)
	}
	n <- length(x)
	deviations <- scale(log(x), center=TRUE, scale=FALSE)
	exp(sqrt(sum(deviations ^ 2) / n))
}