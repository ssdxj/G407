% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ulits4spc.R
\name{spc_ave}
\alias{spc_ave}
\title{Self use function. Average spc SampleID or PlotID or Treatment.}
\usage{
spc_ave(spc, by = "SampleID")
}
\arguments{
\item{spc:}{spc obj}

\item{by:}{SampleID/PlotID/Treatment/SampleDate
\enumerate{
  \item SampleID: group_by(spc_df_melt, FieldID, SampleDate, PlotID, SampleID, wl)
  \item PlotID: group_by(spc_df_melt, FieldID, SampleDate, PlotID, wl)
  \item Treatment: group_by(spc_df_melt, FieldID, SampleDate, Treatment, wl)
  \item SampleDate: group_by(spc_df_melt, FieldID, SampleDate, wl)
}}
}
\value{
spc obj
}
\description{
Self use function. Average spc SampleID or PlotID or Treatment.
}
