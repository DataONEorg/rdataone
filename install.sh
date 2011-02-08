#!/bin/sh
#
# A shell script that installs and executes the D1 R Client code.
#
# aptitude install r-recommended r-cran-rjava
# R CMD javareconf
# R> install.packages("rJava")
# ln -s /usr/local/lib/R/site-library/rJava/jri/libjri.so /usr/lib
umask 002
R --version
#source /etc/R/ldpaths
R CMD INSTALL d1_client_r
R --no-save -e 'library(dataone); d1.t1();'
