\name{GreenplumR-package}
\alias{GreenplumR-package}
\docType{package}
\title{
  An R client of PL/Container and PL/R to Greenplum database.
}
\description{
  GreenplumR is a package that enables users of R, the most popular open source statistical
programming language and environment to interact with the Pivotal (Greenplum)
Database for Big Data analytics. It does so
by providing an interface to the operations on tables/views in the
database. These operations are almost the same as those of
data.frame. Thus the users of R do not need to learn SQL when they
operate on the objects in the database. The latest code, along with a
training video and a quick-start guide, are available at
\url{https://github.com/greenplum-db/GreenplumR}.
}

\details{
\tabular{ll}{

Package: \tab GreenplumR\cr

Type: \tab Package\cr

Version: \tab 0.1.18\cr

Date: \tab 2016-09-15\cr

License: \tab GPL (>= 2)\cr

Depends: \tab methods, DBI, RPostgreSQL\cr

}

This package enables R users to easily develop, refine and deploy R scripts that
leverage the parallelism and scalability of the database as well as in-database
analytics libraries to operate on big data sets that would otherwise not fit in
R memory - all this without having to learn SQL because the package provides an
interface that they are familiar with.

As an R client to the PL/Container and PL/R, this package
minimizes the amount of data transferred between the database and R. All
the big data is stored in the database. The
user enters their familiar R syntax, and the package translates it into
SQL queries and sends the SQL query into database for parallel
execution. The computation result, which is small (if it is as big as
the original data, what is the point of big data analytics?), is
returned to R to the user.

On the other hand, this package also gives the usual SQL users the
access of utilizing the powerful analytics and graphics functionalities
of R. Although the database itself has difficulty in plotting, the
result can be analyzed and presented beautifully with R.

This current version of GreenplumR provides the core R infrastructure and data
frame functions as well as over 50 analytical functions in R that leverage in-
database execution. These include

* Data Connectivity - db.connect, db.disconnect, db.Rquery

* Data Exploration - db.data.frame, subsets

* R language features - dim, names, min, max, nrow, ncol, summary etc

* Reorganization Functions - merge, by (group-by), samples

* Transformations - as.factor, null replacement

}

\note{
  This package is differernt from PL/R, which is another way of using R
  with PostgreSQL-like databases. PL/R enables the users to run R
  scripts from SQL. In the parallel Greenplum database, one can use PL/R
  to implement parallel algorithms.

  However, PL/R still requires non-trivial knowledge of SQL to use it
  effectively. It is mostly limited to explicitly parallel jobs. And for
  the end user, it is still a SQL interface.

  This package does not require any knowledge of SQL. It is much more scalable.
  And for the end user, it is a pure R
  interface with the conventional R syntax.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.
  \email{user@madlib.incubator.apache.org},  with contributions
  from Data Scientist Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\examples{
\dontrun{
## get the help for the package
help("GreenplumR-package")

## create multiple connections to different databases
db.connect(port = 5433) # connection 1, use default values for the parameters
db.connect(dbname = "test", user = "user1", password = "", host =
"remote.machine.com", port = 5432) # connection 2

db.list() # list the info for all the connections

## list all tables/views that has "ornst" in the name
db.objects("ornst")

## list all tables/views
db.objects(conn.id = 1)

## create a table and the R object pointing to the table
## using the example data that comes with this package
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone")

## OR if the table already exists, you can create the wrapper directly
## x <- db.data.frame("abalone")

dim(x) # dimension of the data table

names(x) # column names of the data table

lk(x, 20) # look at a sample of the data

## look at a sample sorted by id column
lookat(sort(x, decreasing = FALSE, x$id), 20)

lookat(sort(x, FALSE, NULL), 20) # look at a sample ordered randomly

## Merge Examples --------

## create two objects with different rows and columns
key(x) <- "id"
y <- x[1:300, 1:6]
z <- x[201:400, c(1,2,4,5)]

## get 100 rows
m <- merge(y, z, by = c("id", "sex"))

lookat(m, 20)

## operator Examples --------

y <- x$length + x$height + 2.3
z <- x$length * x$height / 3

lk(y < z, 20)

## ------------------------------------------------------------------------
## Deal with NULL values

delete("null_data")
x <- as.db.data.frame(null.data, "null_data")

## OR if the table already exists, you can create the wrapper directly
## x <- db.data.frame("null_data")

dim(x)

names(x)
}
}

\keyword{package}
