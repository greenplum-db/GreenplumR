#!/usr/bin/env Rscript
# called with package name as arg[1]
require(remotes)
args = commandArgs(trailingOnly=TRUE)
install_github(args[1])
