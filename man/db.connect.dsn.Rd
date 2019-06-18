\name{db.connect.dsn}
\alias{db.connect.dsn}

\title{
  Create a connection to a database
}
\description{
  Create a connection to a PostgreSQL or Greenplum (Pivotal) database. One can create multiple connections
  to multiple databases. The connections are indexed by an integer
  starting from 1.
}
\usage{
db.connect.dsn(dsn.key, db.ini = "~/db.ini", madlib = "madlib",
default.schemas = NULL, verbose = TRUE, quick = FALSE)
}

\arguments{
  \item{dsn.key}{
    A string. The key name of the DSN file, containing info about connections.
    The useful key names of a dsn are: username, password, host, port, dbname, driver. The key names are case sensitive.
}
  \item{db.ini}{
    A string, default is '~/db.ini'. db.ini is the path of the DSN file.
}
   \item{madlib}{
     A string, default is "madlib". The name of the schema where MADlib
is installed.
}
  \item{default.schemas}{
    A string, default is \code{NULL}. The search path or default schemas
    of the database that you want to use. The string must be a set of
  schema names
    separated by commas. One can also use
  \code{\link{db.default.schemas}} or
  \code{\link{db.search.path}}  to display or set the search
  path in the database.
}
\item{verbose}{
  A logival value, default is \code{TRUE}, whether to print some information while connecting to the database.
}
\item{quick}{
  A logical value, default is \code{FALSE}. Whether to skip some of the
  argument checks to speed up the creation of the connection. Useful
  when using this function inside a function, where you have already
  validate all the arguments. It is not recommended to set this value to
  \code{TRUE} when you are using this function directly.
}
 }

\value{
  An integer, the ID number for the newly created connection.
}

\author{
  Author: Extensions Team at Pivotal Inc.

  Maintainer: Hao Wu, Pivotal Inc. \email{hawu@pivotal.io}
}

\note{
  Right only MADlib 0.6 or later is supported. If you have an older
  version of MADlib, you will not be able to use all the functions whose
  names start with "madlib.". However you can still use all the other
  functions.

  Also, right now only PostgreSQL and Greenplum databases are supported.
}

\seealso{
  \code{\link{db.connect}} connects a connection.

  \code{\link{db.disconnect}} disconnects a connection.

  \code{\link{db.list}} lists all active connections.

  \code{\link{connection info}} the functions that extract information
  about the connection.

  \code{\link{conn.eql}} tests whether two connections are the same.

  \code{\link{db.search.path}} and \code{\link{db.default.schemas}}
  displays or sets the search path (i.e. default schemas) in the
  connected database.
}
\examples{
\dontrun{
## connect to a database
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that ~/db.ini contains all information about connections
## and there is a dsn key named foobar in db.ini. Each dsn has
## multiple key-value pairs. The useful key names are:
## username, password, host, port, dbname, driver.
cid <- db.connect.dsn('foobar', db.ini = '~/db.ini', verbose = FALSE)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}
\keyword{connection}
\keyword{database}
