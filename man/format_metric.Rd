\name{format_metric}
\alias{format_metric}
\title{format_metric}
\usage{
  format_metric(x, digits = 2)
}
\arguments{
  \item{x}{numeric vector}

  \item{digits}{number of significant digits}
}
\value{
  character vector
}
\description{
  Format numbers with significant digits and a metric
  suffix ("k", "M", etc.)
}
\examples{
x <- c(8.8e-3, 12.4e-6, 1704.1, 43.5e6, 10, NaN, NA, Inf, 0.00)
format_metric(x, 2)
}

