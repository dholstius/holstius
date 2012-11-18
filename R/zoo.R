#' ggplot.zoo
#'
#' Plot a zoo object using ggplot2
#'
#' @param	data	a \link[zoo]{zoo} object
#' @param	...		further arguments to \link[ggplot2]{ggplot}
#' @return	same thing as \link[ggplot2]{ggplot}
#' @export
ggplot.zoo <- function(data, ...) {
	require(reshape2)
	require(zoo)
	stopifnot(inherits(data, 'zoo'))
	if ('index' %in% names(data)) {
		stop('"index" must not name a column in ', deparse(substitute(z)))
	}
	wide <- data.frame(coredata(data), index=index(data))
	long <- melt(wide, id.vars='index') 
	ggplot(long, aes(index, value), ...)
}