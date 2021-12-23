\name{repl.context}
\title{REPL Context}
\alias{repl.context}
\usage{
repl.context(what)
}
\description{
  \code{repl.context} retrieves or sets the current REPL context. The
  context is an opaque R object which can be used by the application
  to track evaluation contexts. It can be set either directly using
  the \code{repl.context} function or temporarily for the duration of
  an evaluation in \code{\link{repl.eval}}.
}
\arguments{
  \item{what}{if present, sets the context to the supplied value. If
  missing, the function returns the current context}
}
\value{
  returns the current context, \code{NULL} if no context has been set
}
\details{
  The context itself is opaque, but some applications use it to flag
  output with the context/location where that output has been created.
  For example, RCloud uses context to flag both console and graphical
  output with contexts to be able to associate output with the
  originating code.

  The \code{repl} package provides both R-level as well as C-level API
  to retrieve and set contexts so it can be used by other applications
  or packages.
}
\author{Simon Urbanek}
\keyword{interface}
