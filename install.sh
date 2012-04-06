#!/bin/sh
#
# A shell script that installs and executes the D1 R Client code.
# 
# Prerequisites for this to work include:
# aptitude install r-recommended r-cran-rjava
# R CMD javareconf
# R> install.packages("rJava")
# ln -s /usr/local/lib/R/site-library/rJava/jri/libjri.so /usr/lib
cd ..
unset JAVA_HOME
R --version
R CMD INSTALL d1_client_r
R --no-save -e 'library(dataone); d1.test();'
#R --no-save -e 'library(dataone); d1.javaversion();'
