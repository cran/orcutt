\name{orcutt-package}
\alias{orcutt-package}
\alias{orcutt}
\docType{package}
\title{
Estimate procedure in case of first order autocorrelation
}
\description{
This package has been implemented to solve first order autocorrelation problems using an iterative method. This procedure estimates both autocorrelation and beta coefficients recursively until we reach the convergence (8th decimal). The residuals are computed after estimating Beta using EGLS approach and Rho is estimated using the previous residuals.}
\details{
\tabular{ll}{
Package: \tab orcutt\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-09-11\cr
License: \tab GPL-2\cr
}
~~ functions ~~
Cochrane.Orcutt
}
\author{
Stefano Spada, Matteo Quartagno, Marco Tamburini

Maintainer: Stefano Spada <lostefanospada@gmail.com>
}
\references{Verbeek M. (2004) \emph{A guide to modern econometrics}, Jhon Wiley & Sons Ltd
}
\keyword{ cochrane }
\keyword{ orcutt }
\keyword{ autocorrelation }


