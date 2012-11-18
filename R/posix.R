#' fast_POSIXct
#'
#' Quickly converts local timestamps to a POSIXct vector
#'
#' @param	x	timestamps (YYYY-mm-dd HH:MM:SS)
#' @param	tz	local timezone
#' @return	POSIXct vector
#' @export
fast_POSIXct <- function(x, tz) {
	require(fasttime)
	stopifnot(is.character(x))
	GMT <- fasttime::fastPOSIXct(x, tz='GMT')
	epoch <- as.numeric(GMT)
	z <- as.POSIXct(epoch, tz=tz, origin='1970-01-01')
	adjusted <- z - as.POSIXlt(z)$isdst * 3600
	return(adjusted)
}