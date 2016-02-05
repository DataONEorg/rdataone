# Version 2.0.0

## New features and functions

* Complete rewrite of the package, eliminating all dependencies on Java
* Support for the DataONE v2 API, as well as the existing v1 API

# Version 1.1.0.9002 (2015-06-04)
## Bug fixes

* query() now returns correctly if no query results found and data.frame was requested

# Version 1.1.0.9001 (2015-05-29)

## New features and functions

* Updated query() to add possible return values of R list or data.frame

* added uploadDataObject that can upload a single DataObject to DataONE

* added uploadDataPackage that can upload all objects that have been added to a DataPackage

# Version 1.0.0

* Initial release, providing support for the DataONE REST API
* Utilizes rJava to access the DataONE libclient_java library
* Functions to read and write data from DataONE member repositories
* Functions to query the global DataONE metadata index