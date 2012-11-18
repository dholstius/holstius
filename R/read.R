#' read.serlog
#'
#' Read CSV records from a file written by serlog.py
#'
#' @param file		filename
#' @param varnames	column names for the resulting zoo object
#' @export
read.serlog <- function(file, varnames) {
	require(zoo)
	options(digits.secs=3)
	content <- gsub("\\t", ",", readLines(file))
	columns <- c('datetime', 'millis', 'logger', 'loglevel', varnames)
	records <- read.csv(textConnection(content), header=FALSE, stringsAsFactors=FALSE, col.names=columns)
	timestamps <- with(records, paste(datetime, millis, sep='.'))
	times <- as.POSIXct(strptime(timestamps, "%Y-%m-%d %H:%M:%OS"))
	zoo(records[,5:ncol(records)], order.by=times)
}

#' read.layer
#'
#' Read a layer from a shapefile
#'
#' @param dsn		folder or ZIP file
#' @param layer		layer name
#' @param datum		(optional) transform to this proj4string after loading
#' @param ...		further arguments to rgdal::readOGR()
#' @export
read.layer <- function(dsn, layer, datum, ...) {
	require(rgdal)
	require(sp)
	is.zipfile <- function(x) grepl('.zip$', x, ignore.case=TRUE)
	if (is.zipfile(dsn)) {
		tmp <- tempdir()
		unzip(dsn, exdir=tmp)
		spobj <- readOGR(tmp, layer, ...)
	} else {
		spobj <- readOGR(dsn, layer, ...)
	}
	if (missing(datum)) {
		return(spobj) 
	} else {
		return(spTransform(spobj, CRS(datum)))
	}
}