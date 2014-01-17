\name{correlate}
\alias{correlate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create multivariate data by permutation
}
\description{
Function to obtain a multivariate dataset by specifiying the relation between univariately specified variables. 
}
\usage{
correlate(data, corm, tol=0.005, conv = 10000, cores=2, splitsize=1000, verbose=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data matrix containing the data}
  \item{corm}{A value containing the desired correlation or a vector or data matrix containing the desired correlations}
  \item{tol}{A single value or a vector of tolerances with length \code{ncol(data) - 1}. The default is 0.005}
  \item{conv}{The maximum iterations allowed. Default is 10000}
  \item{cores}{The number of cores to be used for parallel computing}
  \item{splitsize}{The size to use for splitting the data.}
  \item{verbose}{Logical statement. Default is TRUE} 
}
\details{
Splitting the data is done to obtain a rough permutation from where to carry on. This makes the algorithm much faster. 
}
\value{
Returns a matrix given the specified multivariate relation. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Pascal van Kooten and Gerko Vink, 2013
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# bivariate example, start with zero correlation
correlate(replicate(2, rnorm(100)), .5)

# bivariate example, start with perfect correlation
x <- rnorm(100)
correlate(replicate(2, x), .5)

# multivariate example
correlate(replicate(4, rnorm(1000)), c(.5, .6, .7))}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{correlate}
\keyword{permutation}% __ONLY ONE__ keyword per line
