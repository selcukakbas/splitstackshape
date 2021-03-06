% Generated by roxygen2 (4.0.2): do not edit by hand
\name{merged.stack}
\alias{merged.stack}
\title{Take a List of Stacked data.tables and Merge Them}
\usage{
merged.stack(data, id.vars = NULL, var.stubs, sep, keep.all = TRUE, ...)
}
\arguments{
\item{data}{The input \code{data.frame}.}

\item{id.vars}{The columns to be used as "ID" variables. Defaults to \code{NULL}, at which point, all names which are not identified as variable groups are used as the identifiers.}

\item{var.stubs}{The prefixes of the variable groups.}

\item{sep}{The character that separates the "variable name" from the "times"
in the source \code{data.frame}. Alternatively, can be set to
\code{"var.stubs"} (in quotes) if you do not have a value for \code{sep}.}

\item{keep.all}{Logical. Should all the variables in the source
\code{data.frame} be kept (\code{keep.all = TRUE}) or only those which
comprise the \code{id.vars} and split data from the \code{var.stubs}
(\code{keep.all = FALSE}).}

\item{\dots}{Other arguments to be passed on to \code{\link{Stacked}} (for example, \code{keep.rownames} to retain the rownames of the input dataset, or \code{atStart}, in case \code{sep = "var.stubs"} is specified).}
}
\value{
A merged \code{data.table}.
}
\description{
A wrapper around the \code{\link{Stacked}} function to
\code{\link[data.table:merge]{merge}} the resulting \code{list} into a
single \code{data.table}.
}
\note{
The \code{keyed} argument to \code{\link{Stacked}} has been hard-
coded to \code{TRUE} to make \code{merge} work.
}
\examples{
set.seed(1)
mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"),
                   varA.1 = sample(letters, 6),
                   varA.2 = sample(letters, 6),
                   varA.3 = sample(letters, 6),
                   varB.2 = sample(10, 6),
                   varB.3 = sample(10, 6),
                   varC.3 = rnorm(6))
mydf
merged.stack(mydf, var.stubs = c("varA", "varB", "varC"), sep = ".")

\dontshow{rm(mydf)}
}
\author{
Ananda Mahto
}
\seealso{
\code{\link{Stacked}}, \code{\link{Reshape}}
}

