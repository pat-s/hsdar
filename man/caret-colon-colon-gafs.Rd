% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsdar-package.R
\docType{methods}
\name{caret::gafs}
\alias{caret::gafs}
\alias{gafs-methods}
\alias{get_gafs}
\alias{gafs,Speclib-method}
\alias{gafs,Nri-method}
\alias{gafs,Specfeat-method}
\title{Methods for Function \code{gafs}}
\arguments{
\item{x}{Object of class \code{Speclib}, \code{Nri} or \code{Specfeat}. For
\code{get_gafs}, \code{x} must be the output of \code{gafs} as
\code{Speclib} or \code{Nri}.}

\item{y}{A numeric or factor vector containing the outcome for each sample.
If missing, the response variable set by \code{\link[=setResponse]{setResponse()}} is used.}

\item{cutoff}{The cutoff value of the correlation coefficients between
response variables.}

\item{returnData}{Logical. If TRUE, the updated object of \code{x} is
returned, otherwise only the result of \code{\link[=gafs]{gafs()}} is returned.}

\item{...}{Further aruments passed to \code{\link[=gafs]{gafs()}}.}
}
\value{
If \code{returnData == TRUE}, an object of class \code{Speclib} or
\code{Nri}, otherwise an object of class \code{gafs}. Note that if \code{x}
is an object of class \code{Specfeat}, the function returns an object of
class \code{Speclib} containing the relevant transformed band values.
}
\description{
Methods for function \code{gafs} in package \pkg{caret}.
}
\examples{

\dontrun{
data(spectral_data)

## Set response variable (Chlorophyll content)
spectral_data <- setResponse(spectral_data, "chlorophyll")

## Set additional predictor variables from the attributes
spectral_data <- setPredictor(spectral_data, "season")

## Feature selection using genetic algorithms
## Note that this may take some time!
gafs_res <- gafs(spectral_data)

get_gafs(gafs_res)
}

}
\seealso{
\code{\link[=gafs]{gafs()}}
}
\author{
Lukas Lehnert
}
\keyword{methods}
