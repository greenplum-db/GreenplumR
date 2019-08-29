\name{delete}

\alias{delete}
\alias{delete,db.data.frame-method}
\alias{delete,db.Rquery-method}
\alias{delete,character-method}
\alias{delete,bagging.model-method}

\title{ Safely delete a \code{db.obj} object or a table/view in the database }

\description{
  This function deletes a \code{db.data.frame} object together with the
  table/view that it points to. It deletes a \code{db.Rquery} object. It
  can also directly delete a table or view in the database. When applied
  onto some composite data objects, it deletes the data table wrapped by
  them.
}

\usage{

\S4method{delete}{db.data.frame}(x, cascade = FALSE)

\S4method{delete}{db.Rquery}(x)

\S4method{delete}{character}(x, conn.id = 1, is.temp = FALSE, cascade =
FALSE)

\S4method{delete}{bagging.model}(x)

}

\arguments{
  \item{x}{
    The signature of the method.

    A \code{db.data.frame} object, which
    points to a table or view in the database;

    Or a \code{db.Rquery}
    object, which represents some operations on an existing
    \code{db.data.frame} object;

    Or a string, the table/view name to delete in the database. In the
    this case, the result model tables wrapped by \code{model},
    \code{residuals} and \code{statistics} will be deleted.

    Or an object which is the result of \code{\link{generic.bagging}}.

}

\item{conn.id}{
  An integer, default is 1. The connection ID to the database.
}

\item{is.temp}{
  A logical, default is \code{FALSE}. Whether the table/view is temporary.
}

\item{cascade}{
  A logical, default is \code{FALSE}. Whether to delete objects together
  with all the objects depending on it.
}
}

\details{
  When a \code{db.data.frame} object is deleted, the table/view that is
  associated with it is also deleted.
}

\value{
  When \code{x} is \code{db.data.frame} or table/view name, this
  function returns a logical value. which is \code{TRUE} if the deletion
  is successful.

  No value is returned if \code{x} is \code{db.Rquery}
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
  \code{\link{db.data.frame}} creates an object pointing to a table/view
  in the database.

  \code{\link{db.objects}} lists all tables and views in a database
  together with their schema.

  \code{\link{db.existsObject}} tests whether a table/view exists in the
  database.

  \code{\link{as.db.data.frame}} creates a \code{db.data.frame} from a
  \code{data.frame}, a data file or a \code{db.Rquery}.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

delete("abalone", cid, is.temp = TRUE)

delete("abalone", cid, is.temp = FALSE)

delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid)

lk(x, 10)

y <- as.db.data.frame(abalone, "abalone", conn.id = cid, is.temp = TRUE)

lk(y, 10)

db.existsObject("abalone", cid, is.temp = TRUE)

db.existsObject("abalone", cid, is.temp = FALSE)

delete("abalone", cid)

p <- db.objects()
p[p == "abalone"]

## Example: delete multiple tables
## all table in public schema start with "ornste"
to.delete <- db.objects("public.ornste", conn.id = cid)
for (table.name in to.delete) delete(table.name, conn.id = cid)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{database}
\keyword{utility}
