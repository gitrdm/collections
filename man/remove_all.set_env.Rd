% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_env.R
\name{remove_all.set_env}
\alias{remove_all.set_env}
\title{Title Remove All Objects from Set}
\usage{
remove_all.set_env(set)
}
\arguments{
\item{set}{The set_env from which to remove all objects.}
}
\value{
The set_env with all objects removed.
}
\description{
Title Remove All Objects from Set
}
\examples{
set <- set_env()
set <- add_many.set_env(set, c("apple", "banana"))
set <- remove_all.set_env(set)
}
