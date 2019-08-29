\name{as.factor-methods}
\alias{relevel,db.obj-method}
\alias{as.factor,db.obj-method}

\title{ Convert one column of a \code{\linkS4class{db.obj}} object into a categorical variable }

\description{
  Convert one column of a \code{db.obj} object into a categorical
  variable. The reference level for regressions can be selected using \code{relevel}.
}

\usage{
\S4method{as.factor}{db.obj}(x)

\S4method{relevel}{db.obj}(x, ref, ...)
}

\arguments{
  \item{x}{
    A \code{db.obj} object. It must have only one column.
  }

  \item{ref}{
    A single value, which is the reference level that is used in the regressions.
  }

  \item{\dots}{
    Other arguments passed into the result. Not implemented yet.
  }
}

\value{
  A \code{db.Rquery} object. It has only one column which is categorical. By default, a reference level is automatically selected in regressions, which is usually the minimum of all levels, but one can easily change the reference level using \code{relevel}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{

  When \code{\link{as.db.data.frame}} creates a table/view, it can create dummy variables for a
  categorical variable.
}

\examples{
\dontrun{
## get help for a method
## help("as.factor,db.obj-method")

%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a temporary table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## set sex to be a categorical variable
x$sex <- as.factor(x$sex)

## another temporary table
z <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## using relevel to change the reference level
x$sex <- relevel(x$sex, ref = "M")

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{factor}
\keyword{math}
