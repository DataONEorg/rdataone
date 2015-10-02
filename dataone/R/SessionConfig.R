#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2015
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

#' SessionConfig provides methods to get, set, load, unload dataone configuration parameters.
#' @rdname SessionConfig-class
#' @slot DefaultConfigFilepath the default config file to load if no other filename is specified
#' @slot InitialConfigFilepath the initial config file if a config file has never been created
#' @export
setClass("SessionConfig", slots = c(
  DefaultConfigFilepath = "character", # configuration file to use if one is not specified to loadConfig()
  InitialConfigFilepath = "character") # Location to read / write configuration value
)

#' Initialize a SessionConfig object
setMethod("initialize", "SessionConfig", function(.Object) {
  # If a session is currently defined, then unload it
  if (is.element(".D1Config", base::search())) {
    detach(".D1Config", unload=TRUE)
  }
  
  .Object@DefaultConfigFilepath <- sprintf("%s/.dataone/sessionConfig.R", path.expand("~")) # configuration file to use if one is not specified to loadConfig()
  .Object@InitialConfigFilepath <- system.file(package="dataone", "extdata/configParams.R") # Location to read / write configuration values
  
  # Create an R environment that will be used to store config variables
  attach(NULL, name=".D1Config")
  # sessionConfigEnv <- as.environment(".D1Config")
  return(.Object)
})

#' Load configuration parameters
#' @description Load a configuration into the current session.
#' @details This routine has the side effect of creating an R variable for each
#' paramater contained in the specified configuration file. If a configuration file
#' is not specified, then the default file is used. If configuration is being run for the first
#' time, then a default file is loaded from the dataone package.
#' @param .Object a SessionConfig instance
#' @param file the file path to the configuration file, default is "~/.dataone/config.csv"
#' @export
setGeneric("loadConfig", function(.Object, ...) {
  standardGeneric("loadConfig")
})

#' @describeIn SessionConfig
setMethod("loadConfig", signature("SessionConfig"), function(.Object, file=as.character(NA)) {
  
  # No config file specified, so read default
  if(is.na(file)) {
    file <- .Object@DefaultConfigFilepath
    # If using default file location, make sure ~/.dataone directory exists first
    # First remove filename from file path, to get dir name
    defaultDir <- dirname(.Object@DefaultConfigFilepath)
    if(!file.exists(defaultDir)) {
      dir.create(defaultDir, showWarnings = TRUE, recursive = TRUE, mode = "0755")
    }
  }
  
  if(!file.exists(file)) {
    file.copy(.Object@InitialConfigFilepath, file)
    message(sprintf("An initial session configuration file for the R package \"dataone\" has been copied to \"%s\"", file))
    message("Please review the \"dataone\" package documentation section 'Configuring dataone'")
    message("and then edit the configuration file with values appropriate for your installation.")
  }
  # Check if the config environemnt exists
  if (! is.element(".D1Config", base::search())) {
    warning("Cannot load configuration parameters, a configuration session is not currently active.")
    return()
  }
  
  if(!file.exists(file)) {
    stop(sprintf("Unable to load configuration file: %s\n", file))
  }
  
  # Clear the config environment of all previously loaded or manually set variables
  rm(list=ls(".D1Config"), pos=".D1Config")
  
  # Load the configuration from the requested file
  source(file, as.environment(".D1Config"))
})


#' Unload configuration parameters
#' @description Unload a configuration
#' @details The R environment that contained the configuration parameters is destroyed
#' @param .Object a Configuration instance
#' @export
setGeneric("unloadConfig", function(.Object, ...) {
  standardGeneric("unloadConfig")
})

#' @describeIn SessionConfig
setMethod("unloadConfig", signature("SessionConfig"), function(.Object) {
  
  if (is.element(".D1Config", base::search())) {
    detach(".D1Config", unload=TRUE)
  }
})

#' Save configuration parameters to a file
#' @description Load a configuration into the current session.
#' @details This routine has the side effect of creating an R variable for each
#' paramater contained in the specified configuration file. If a configuration file
#' is not specified, then the default file is used. If configuration is being run for the first
#' time, then a default file is loaded from the dataone package.
#' @param .Object a SessionConfig instance
#' @param file the file path to the configuration file, default is "~/.dataone/config.csv"
#' @export
setGeneric("saveConfig", function(.Object, ...) {
  standardGeneric("saveConfig")
})

#' @describeIn SessionConfig
setMethod("saveConfig", signature("SessionConfig"), function(.Object, file=.Object@DefaultConfigFilepath) {
  
  if (! is.element(".D1Config", base::search())) {
    warning("Cannot save configuration parameters, a configuration session is not currently active.")
    return()
  }
  # Remove existing configuration to prevent corruption
  if(file.exists(file)) {
    if (file.info(file)[["isdir"]]) {
      # Is the file a directory?
      message(sprintf("Unable to save configuration file, specified file to save \"%s\" is a directory", file))
      return()
    }
    # Backup previous file
    file.rename(file, sprintf("%s.save", file))
  }
  # List all the config params that were read in or saved via saveConfig()
  params <- base::ls(".D1Config")
  if (length(params) == 0) {
    message("Unable to save configuration file, no parameters loaded")
    return()
  }
  D1ConfigEnv <- as.environment(".D1Config")
  # Write out each parameter and value to the config file as an R statement
  for (pName in params) {
    # Serialize an R object to a string using the dput function and store this in a data frame
    # that will be written to a config parameter file
    strBuf <- textConnection("paramBuf", "w")
    dump(pName, strBuf, control=NULL, envir=as.environment(".D1Config"))
    close(strBuf)
    #write(sprintf("# %s", comment), file, ncolumns=1, append=TRUE)
    write(paramBuf, file, ncolumns=2, sep=" ", append=TRUE)
  }
})

#' Load configuration parameters
#' @description Load a configuration into the current configuration session.
#' @details This routine has the side effect of creating an R variable for each
#' paramater contained in the specified configuration file. If a configuration file
#' is not specified, then the default file is used. If Configuration is being run for the first
#' time, then a default file is loaded from the dataone package.
#' @param .Object a SessionConfig instance
#' @param file the file path to the configuration file, default is "~/.dataone/config.csv"
#' @export
setGeneric("setConfig", function(.Object, name, value) {
  standardGeneric("setConfig")
})

#' @describeIn SessionConfig
setMethod("setConfig", signature("SessionConfig", "character", "ANY"), function(.Object, name=as.character(NA), value) {
  
  if (! is.element(".D1Config", base::search())) {
    warning("Cannot set configuration parameters, a Configuration session is not currently active.")
    return()
  }
  
  sessionConfigEnv <- as.environment(".D1Config")
  qualifiedParamName <- sprintf("sessionConfigEnv$%s", name)
  # Get the data type (class) of a value already read in via readConfig(). If this parameter
  # was already stored, then compare the class of the previously stored value with the class
  # of the passed in variable.
  paramClass <- "character"
  if(is.element(name, base::ls(".D1Config"))) {
    cmd <- sprintf("class(%s)", qualifiedParamName)
    paramClass <- eval(parse(text=cmd))
    if(paramClass != class(value)) {
      stop(sprintf("\nCannot set parameter value: the type \"%s\" for the loaded parameter \"%s\" is \nnot the same as for the input value: %s\n", paramClass, name, class(value)))
    }
  }
  
  strBuf <- textConnection("paramBuf", "w")
  dumpList <- "value"
  dump(dumpList, strBuf, control=NULL)
  close(strBuf)
  assignCmd <- sprintf("%s <- %s", qualifiedParamName, paramBuf)
  #cat(sprintf("cmd: %s\n", assignCmd))
  eval(parse(text=assignCmd))
})

#' Load configuration parameters
#' @description Load a configuration into the current configuration session.
#' @details This routine has the side effect of creating an R variable for each
#' paramater contained in the specified configuration file. If a configuration file
#' is not specified, then the default file is used. If configuration is being run for the first
#' time, then a default file is loaded from the dataone package.
#' @param .Object a SessionConfig instance
#' @param file the file path to the configuration file, default is "~/.dataone/config.csv"
#' @export
setGeneric("getConfig", function(.Object, ...) {
  standardGeneric("getConfig")
})

#' @describeIn SessionConfig
setMethod("getConfig", signature("SessionConfig"), function(.Object, name=as.character(NA)) {
  
  if (! is.element(".D1Config", base::search())) {
    stop("Cannot load configuration parameters, a configuration session is not currently active.")
  }
  
  if(is.element(name, base::ls(".D1Config"))) {
    sessionConfigEnv <- as.environment(".D1Config")
    qualifiedParamName <- sprintf("sessionConfigEnv$%s", name)
    cmd <- sprintf("%s", qualifiedParamName)
    value <- eval(parse(text=cmd))
    return(value)
  }
})

#' List all currently defined configuration parameters
#' @description All currently defined configuration parameter that were instatiated 
#' with loadConfig() are listed
#' @export
setGeneric("listConfig", function(.Object, ...) {
  standardGeneric("listConfig")
})

#' @describeIn SessionConfig
setMethod("listConfig", signature("SessionConfig"), function(.Object, name=as.character(NA)) {
  
  if (! is.element(".D1Config", base::search())) {
    stop("Cannot load configuration parameters, a configuration session is not currently active.")
  }
  
  params <- base::ls(".D1Config")
  for (p in params) {
    val <- getConfig(.Object, p)
    cat(sprintf("%s = ", p))
    print(val)
  }
})
