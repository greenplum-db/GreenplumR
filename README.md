GreenplumR
==========

GreenplumR is a R client that designed for Greenplum Database (> 6.0). With GreenplumR installed in R environment, users can interact with data in Greenplum Database for analytics purpose. GreenplumR provides a rich interface to allow user access both tables and views with minimal data transfer. Users can easily access database without any knowledge of SQL. Moreover, GreenplumR can allow user to execute their own R code combine with data in Greenplum Database directly via built-in ``APPLY`` functions. GreenplumR can work with PL/Conatiner to provide a high preformance sandbox R runtime environment.

How to install
==============

You can install GreenplumR through 2 ways:

 1. Install throught devtools and get the latest development version from github by running the following code (need R >= 3.0.2):

```
    ## install.packages("devtools") # 'devtools' package is only available for R >= 3.0.2
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
db.connect(port = 15432, dbname = "test")

## connect to database via DSN

db.list() # list connections

db.objects() # list all tables in connection 1

db.objects("tbl.tbl") # search table in connection 1

## wrapper of a table in connection 1
t <- db.data.frame("tbl.tbl")

t

## example of gpApply (TBA)

x <- gpApply()

## example of gpTApply (TBA)

y <- gpTApply()

```

Management Tools
===========

TBA