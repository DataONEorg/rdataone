#!/bin/sh
#
# A shell script that installs and executes the D1 R Client code.
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
R --silent CMD INSTALL dataone &&\
  R --silent --no-save -e 'library(dataone); d1.test();'

