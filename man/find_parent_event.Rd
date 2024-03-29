% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_func.R
\name{find_parent_event}
\alias{find_parent_event}
\title{Find the Event parent Level
Used in func: createMoF}
\usage{
find_parent_event(level, df)
}
\arguments{
\item{level}{a string, either "Occ" or "Event"}

\item{df}{df a data.frame, output of createAggregatedDF()}
}
\value{
a string, the name of the Event parent level (e.g. "Event-2")
}
\description{
Find the Event parent Level
Used in func: createMoF
}
