# *dataone* R Package Design

The intent of this document is to collect design ideas for the *dataone* R package.
Both new functionality and refactoring of current implementations can be included.
Once a consensus has been reached for a particular implementation, then more formal
documentation can be prepared, if desired, i.e. UML activity diagrams, sequence charts, etc.

## Design Goals

### Provide authentication for DataONE service calls

The current DataONE authentication for CNs and MNs can use either
authentication tokens or X.509 certificates. Authentication tokens
are only supported for DataONE nodes that support the V2 DataONE API.
Also, the DataONE service requests can be send to either production
or testing nodes, requiring different authentication for the production
versus testing environments.

### Current Implementation

The current implementation uses the DataONE API version number to determine
whether an authentication token can be used. The API version is obtained from
a DataONE node when the MNode or CNode instance for that node is created.
The node instance is passed along to the various auth_* methods so that
they can test for an retrieve the correct authentication for the node
that the request is being made to.

### Refactoring Ideas
