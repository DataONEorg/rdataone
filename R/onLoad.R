.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, jars='*', lib.loc = libname)
}
