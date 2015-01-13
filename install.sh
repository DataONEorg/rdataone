#!/bin/sh
#
# A shell script that installs and executes integration tests
# on the D1 R Client code.
# 
# Prerequisites for this to work include:
# aptitude install r-recommended r-cran-rjava
# R CMD javareconf
# R> install.packages("rJava")
# ln -s /usr/local/lib/R/site-library/rJava/jri/libjri.so /usr/lib

# Install modules into user directory.
#     R_USER_LIBS=${R_LIBS_USER:-${HOME}/.Rlibrary}
#     mkdir -p ${R_LIBS_USER}
#     echo 'R_LIBS_USER="~/.Rlibrary"' >> ${HOME}/.Renviron

unset JAVA_HOME
#R --version
#R --no-save -e 'library(dataone); d1.javaversion();'
#R --silent CMD INSTALL dataone &&\
#  R --silent --no-save -e 'library(dataone); d1.test();'

#  R --silent --no-save -e 'library(dataone); source(system.file("testing.R"), package="dataone"); dataone:::d1.test();'
# R library package dependencies: httr, devtools, testthat, roxygen2, XML, rJava, dataonelibs, mbjones/PKI from github, stringr
# options(repos = c(CRAN = "http://cran.rstudio.com"))
# install_github("mbjones/PKI")
# install.packages(c("httr", "devtools", "testthat", "roxygen2", "XML", "rJava", "stringr"))
mkdir -p ./R/x86_64-pc-linux-gnu-library/3.1 && \
R --silent CMD INSTALL -l ./R/x86_64-pc-linux-gnu-library/3.1 dataonelibs && \
R --silent CMD INSTALL -l ./R/x86_64-pc-linux-gnu-library/3.1 dataone && \
R --file=./runTests.R

