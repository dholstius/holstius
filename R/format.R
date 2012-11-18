#' format_signif
#'
#' Format numbers with a given number of significant digits
#'
#' @param	x		numeric vector
#' @param	digits	number of significant digits
#' @return	character vector
#' @examples
#' x <- c(0.800, 1704.1, 43.5e6, 10, NaN, NA, Inf, 0.00)
#' format_signif(x, digits=3)
#' @export
format_signif <- function(x, digits) {
	require(stringr)
	rounded <- signif(x, digits)
	serialized <- formatC(rounded, digits, format="fg", flag="#")
	trimmed <- str_trim(serialized, side="both")
	str_replace(trimmed, "\\.$", "")
}

#' format_metric
#'
#' Format numbers with significant digits and a metric suffix ("k", "M", etc.)
#'
#' @param	x		numeric vector
#' @param	digits	number of significant digits
#' @return	character vector
#' @examples
#' x <- c(8.8e-3, 12.4e-6, 1704.1, 43.5e6, 10, NaN, NA, Inf, 0.00)
#' format_metric(x, 2)
#' @export
format_metric <- function(x, digits=2) {
	pow <- floor(log10(x) / 3)
	pow <- ifelse(sapply(pow != 0 & abs(pow) < Inf, isTRUE), pow, 0)
	abbr <- c('n', 'u', 'm', '', 'k', 'M', 'G', 'T')[pow + 4]
	repr <- format_signif(x / (10 ** (3 * pow)), digits=digits)
	paste(repr, abbr, sep='')
}