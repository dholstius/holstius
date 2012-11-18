\name{read.layer}
\alias{read.layer}
\title{read.layer}
\usage{
  read.layer(dsn, layer, datum, ...)
}
\arguments{
  \item{dsn}{folder or ZIP file}

  \item{layer}{layer name}

  \item{datum}{(optional) transform to this proj4string
  after loading}

  \item{...}{further arguments to rgdal::readOGR()}
}
\description{
  Read a layer from a shapefile
}

