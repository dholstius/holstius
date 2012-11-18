#' melt.zoo
#'
#' Convert zoo object to long-format data frame
#'
#' @param z		a \link[zoo]{zoo} object
#' @param ...	further arguments to melt()
#' @return		a data.frame
#'
#' @export
melt.zoo <- function(z, ...) {
	require(reshape2)
	require(zoo)
	wide <- data.frame(index=index(z), z, row.names=NULL)		
	if (is.null(ncol(z))) {
		names(wide)[-1] <- deparse(substitute(z))
	}
	melt(wide, id.vars="index", ...)
}