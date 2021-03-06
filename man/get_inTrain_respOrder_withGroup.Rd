% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ulits4preProcess.R
\name{get_inTrain_respOrder_withGroup}
\alias{get_inTrain_respOrder_withGroup}
\title{get inTrain vector by ordered reponse value(within each group)}
\usage{
get_inTrain_respOrder_withGroup(input, group, step = 4, start = 2)
}
\arguments{
\item{input}{numeric vector(normally the response)}

\item{group}{str vector(ex: stage, year)}

\item{step}{1 out of step in Test}

\item{start}{one of 1:step where the first Test data is.}
}
\value{
index vector for inTrain
}
\description{
get inTrain vector by ordered reponse value(within each group)
}
