% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GAN.R
\name{GAN}
\alias{GAN}
\title{GAN}
\usage{
GAN(deploy = TRUE, model_units = 1)
}
\arguments{
\item{deploy}{boolean, TRUE if the model should be deployed in a live trading environment, FALSE if it's just being used for testing/research, Default: TRUE}

\item{model_units, }{how many units to allocate to this model}
}
\value{
An xts of units to be combined with an unconditional return series to form portfolio (ie condition) returns.
}
\description{
As in GANn, the trader.
}
\details{
3 weeks down, 3 weeks up.
}
