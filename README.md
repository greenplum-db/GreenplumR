GreenplumR
==========

GreenplumR is a R client that designed for Greenplum Database (> 6.0). With GreenplumR installed in R environment, users can interact with data in Greenplum Database for analytics purpose. GreenplumR provides a rich interface to allow user access both tables and views with minimal data transfer. Users can easily access database without any knowledge of SQL. Moreover, GreenplumR can allow user to execute their own R code combine with data in Greenplum Database directly via built-in ``APPLY`` functions. GreenplumR can work with PL/Conatiner to provide a high preformance sandbox R runtime environment.

How to install
==============
Pre-Installation:

You need to install **ini, shiny, RPostgreSQL** packages before install GreenplumR.
```
install.packages(c("ini", "shiny", "RPostgreSQL"))
```


You can install GreenplumR through 2 ways:

 1. Install throught devtools and get the latest development version from github by running the following code (need R >= 3.6.0):

```
    ## install.packages("devtools")
    devtools::install_github("GreenplumR", "greenplum-db")
```
2. Or You can download the source tarball directly from [**here**](https://github.com/greenplum-db/GreenplumR/tarball/master), and then install the tarball in R:

```
    install.packages("greenplum-db-GreenplumR-xxxx.tar.gz", repos = NULL, type = "source")
```
Get started
===========

```R

library(GreenplumR)

## connect to a local database
db.connect(host = "localhost",port = 5432, dbname = "test")

## connect to database via DSN

db.list() # list connections

db.objects() # list all tables in connection 1

db.objects("tbl.tbl") # search table in connection 1

## wrapper of a table in connection 1
t <- db.data.frame("tbl.tbl")

t

## example of gpapply
fn.function_plus_one <- function(num)
{
    return (num[[1]] + 1)
}
# for example table "tbl.tbl" has one integer column, we need to define the signature of input data frame
.sig <- list("num" = "int")
x <- db.gpapply(t, output.name = NULL, FUN = fn.function_plus_one, output.signature = .sig, clear.existing = TRUE, case.sensitive = TRUE, language = "plcontainer")

# comparing with gpapply, gptapply allows you to add an extra argument "INDEX"
# if the table is indexed by column num, we can call gptapply in this way:
.index <- 'num'
y <- db.gptapply(t, INDEX = .index, output.name = NULL, FUN = fn.function_plus_one, output.signature = .sig, clear.existing = TRUE, case.sensitive = TRUE, language = "plcontainer")

```
For more information about gpapply and gptapply, please refer to detailed document:

[gpapply](./db.gpapply.md)

[gptapply](./db.gptapply.md)


Management Tools
===========

TBA