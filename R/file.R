#' file.ext
#' 
#' Given a filename, returns the file extension
#'
#' @param	x			file name or path
#' @param	sep			file extension separator
#' @param	lowercase	convert return value to lowercase first?
#' @return				the file extension, such as .jpg or .txt
#' @export
file.ext <- function(x, sep='.', lowercase=TRUE) {
	require(stringr)
	ext <- as.character(str_match(x, '[^.]+$'))
	if (ext == x) {
		ext = ''
	}
	if (isTRUE(lowercase)) {
		ext <- tolower(ext)
	}
	return(ext)
}
