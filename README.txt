DataONE R Client
================

An R_ package that provides read/write access to data and metadata from the DataONE_ network of `Member Node`_ data repositories. Member Nodes in DataONE are independent data repositories that have adopted the DataONE services for interoperability, making each of the repositories accessible to client tools such as the DataONE R Client using a standard interface.  The DataONE R Client can be used to access data files and to write new data and metadata files to nodes in the DataONE network.  

.. _R: http://www.r-project.org/
.. _DataONE: http://www.dataone.org/
.. _Member Node: http://www.dataone.org/member-nodes

Downloads
---------

The currently supported version requires two R packages, dataone and dataonelibs (which provides the Java libraries necessary for the dataone package to function):

- DataONE R Client (dataone_1.0.0.tar.gz_)
- DataONE Libraries (dataonelibs_1.0.0.tar.gz_)

Documentation for the DataONE R Client is provided as help files within the R framework, and is downloadable in PDF format:

- DataONE R Client Manual (dataone_1.0.0-manual.pdf_)

.. _dataone_1.0.0.tar.gz: https://releases.dataone.org/dist/dataone_1.0.0.tar.gz

.. _dataonelibs_1.0.0.tar.gz: https://releases.dataone.org/dist/dataonelibs_1.0.0.tar.gz

.. _dataone_1.0.0-manual.pdf: https://releases.dataone.org/dist/dataone_1.0.0-manual.pdf

Installation Notes
------------------

The easiest way to install the client is via the CRAN_ repository::


  > install.packages("dataonelibs")

  > install.packages("dataone")


.. _CRAN: http://cran.r-project.org

Alternatively, you can download and install the package manually from this web
site above and, after downloading the package, install it using::

  $ R CMD install dataonelibs_1.0.0.tar.gz

  $ R CMD install dataone_1.0.0.tar.gz

This package uses rJava to bind to the Java client libraries.  Be sure to install 
rJava for your particular version of R and Java.

.. > install.packages("rJava",,"http://rforge.net/",type="source")

Once installed, the package can be run in R using::

  $ R 
  > library(dataone)
  Loading required package: rJava
  Loading required package: XML
  Loading required package: dataonelibs

  > help(dataone)


License
-------

The DataONE R Client is licensed as open source software under the Apache 2.0 license.

Authors
-------

- Matthew Jones <jones@nceas.ucsb.edu>
- Rob Nahf <nahf@dataone.unm.edu>
