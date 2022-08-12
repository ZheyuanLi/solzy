\name{glmnetPath}
\alias{glmnetPath}

\title{
Summary of a "glmnet" coefficient path
}

\description{
Summarize the variable selection process in a fitted "glmnet" model. Identifies (1) when each variable enters the model as \eqn{\lambda} decreases; (2) when each variable leaves the model as \eqn{\lambda} increases; (3) what variables never enter the model, i.e., what variables are effectively ignored.
}

\usage{
glmnetPath(fit)
}

\arguments{
\item{fit}{a model fitted by \code{glmnet} or \code{cv.glmnet}.}
}

\value{
A list of three data frames: \code{enter}, \code{leave} and \code{ignored}.
}

\author{
Zheyuan Li \email{13937871312@163.com}
}

\references{
\url{https://stackoverflow.com/a/73312496/4891738}
}
