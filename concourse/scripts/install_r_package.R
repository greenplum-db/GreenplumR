#!/usr/bin/env Rscript
# called with package name as arg[1]
args = commandArgs(trailingOnly=TRUE)
cran = getOption("repos")
cran["CRAN"]="https://cran.r-project.org/"
options(repos = cran)
rm (cran)
install.packages(args[1])
