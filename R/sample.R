#' sample.data.frame
#' 
#' Sample rows from a data.frame
#'
#' @param	x		A \link{data.frame}
#' @param	size	A non-negative integer, or a fraction in [0, 1) (see Note)
#' @param	replace	Should sampling be with replacement?
#' @param	prob	A vector of probability weights
#'
#' @note	If size is exactly 1, then one row will be returned. You might instead want sample(x, size = nrow(x), replace = TRUE).
#'
#' @export 
sample.data.frame <- function(x, size, replace = FALSE, prob = NULL) {
	if (size < 1) {
		# sample a fraction of the total number of rows
		n <- size * nrow(x)
	} else {
		# sample a specific number of rows
		n <- size
	}
	i <- sample(nrow(x), size = n, replace = replace, prob = prob)
	return(x[i,])
} 

setGeneric('sample')
setMethod('sample', 'data.frame', sample.data.frame)