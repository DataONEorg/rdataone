\name{D1Client-methods}
\docType{methods}
\alias{D1Client-methods}
\alias{D1Client,ANY,ANY-method}
\alias{D1Client,character,ANY-method}
\alias{D1Client,character,character-method}
\title{ ~~ Methods for Function \code{D1Client} in Package \pkg{dataone} ~~}
\description{
 A D1Client object can be created in one of 3 ways, as described below.  The user
 has a choice of environments and member nodes within the chosen environment when
 building the D1Client object.
 
 Valid choices for env are: 'PROD' (the default), 'SANDBOX','STAGING', 'STAGING2' 
 and 'DEV'
}
\section{Methods}{
\describe{

\item{\code{signature(env = "ANY", mnNodeid = "ANY")}}{
%%  ~~describe this method here~~
Creates a D1Client pointing to the production DataONE environment.  It does not
set a member node.
}

\item{\code{signature(env = "character", mnNodeid = "ANY")}}{
%%  ~~describe this method here~~
Creates a D1Client pointing to the environment specified, also without setting
a default member node.
}

\item{\code{signature(env = "character", mnNodeid = "character")}}{
%%  ~~describe this method here~~
Creates a D1Client pointing to the environment specified, and setting the default
member node for all content submission.
}
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
