% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_reorder_pfas.R
\name{fct_reorder_PFAS}
\alias{fct_reorder_PFAS}
\title{fct_reorder_PFAS Reorder PFAS factor}
\usage{
fct_reorder_PFAS(data)
}
\arguments{
\item{data}{dataset with column PFAS}
}
\value{
data with reordered factor column
}
\description{
fct_reorder_PFAS Reorder PFAS factor
}
\examples{
levels(fct_reorder_PFAS(BMF_diet_PFAS_ng_gdw_compare)$PFAS)
}
