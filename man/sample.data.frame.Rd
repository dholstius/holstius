\name{sample.data.frame}
\alias{sample.data.frame}
\title{sample.data.frame}
\usage{
  sample.data.frame(x, size, replace = FALSE, prob = NULL)
}
\arguments{
  \item{x}{A \link{data.frame}}

  \item{size}{A non-negative integer, or a fraction in [0,
  1) (see Note)}

  \item{replace}{Should sampling be with replacement?}

  \item{prob}{A vector of probability weights}
}
\description{
  Sample rows from a data.frame
}
\note{
  If size is exactly 1, then one row will be returned. You
  might instead want sample(x, size = nrow(x), replace =
  TRUE).
}

