\name{cochrane.orcutt}
\alias{cochrane.orcutt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate procedure in case of first order autocorrelation
%%  ~~function to do ... ~~
}
\description{This package has been implemented to solve first order autocorrelation problems using an iterative method. This procedure estimates both autocorrelation and beta coefficients recursively until we reach the convergence (8th decimal). The residuals are computed after estimating Beta using EGLS approach and Rho is estimated using the previous residuals.}

\usage{
cochrane.orcutt(reg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reg}{ a linear model built with lm function
%%     ~~Describe \code{reg} here~~
}
}
\value{
An object of class "summary.lm", basically a list including elements
\item{coefficients}{ a named vector of coefficients }
\item{residuals}{ residuals }
}
\references{Verbeek M. (2004) \emph{A guide to modern econometrics}, Jhon Wiley & Sons Ltd
%% ~put references to the literature/web site here ~
}
\author{Stefano Spada, Matteo Quartagno, Marco Tamburini}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
price<-c( 0.27, 0.28, 0.28, 0.28, 0.27, 0.26, 0.28 ,0.27 ,0.26 ,0.28, 0.28 ,0.27, 0.27, 0.29, 0.28,0.29, 0.28, 0.28, 0.28, 0.28, 0.29, 0.29, 0.28, 0.28, 0.28, 0.26, 0.26 ,0.26, 0.27 ,0.26)
cons<-c(0.39, 0.37, 0.39, 0.42, 0.41, 0.34, 0.33, 0.29, 0.27, 0.26, 0.29 ,0.30, 0.33, 0.32, 0.38,0.38, 0.47, 0.44, 0.39, 0.34, 0.32, 0.31, 0.28, 0.33, 0.31, 0.36, 0.38 ,0.42, 0.44 ,0.55)
income<-c(78, 79, 81, 80 ,76 ,78, 82, 79, 76, 79, 82, 85, 86, 83, 84, 82, 80, 78, 84, 86, 85, 87, 94, 92, 95, 96, 94, 96, 91, 90)
temp<-c(41,56,63,68,69,65,61,47,32,24,28,26,32,40,55,63,72,72,67,60,44,40,32,27,28,33,41,52,64,71)
reg<-lm(cons~price+income+temp)
reg2<-cochrane.orcutt(reg)
reg2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cochrane }
\keyword{ orcutt }
\keyword{ autocorrelation }