% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imp_na.R
\name{imp_na}
\alias{imp_na}
\title{Impute Missing Data using Linear Interpolation}
\usage{
imp_na(data, period = "open days")
}
\arguments{
\item{data}{Data frame. A data frame containing road traffic data.}

\item{period}{Character. The period to impute data from: "open days" for open days,
"public holidays" for public holidays, and "holidays" for holidays.}
}
\value{
A data frame with imputed data and a new column named "imputation" indicating
whether each data point was original (0), imputed (1), or NA (2).
}
\description{
This function uses a linear interpolation method to fill in missing data in the road traffic data.
The interpolation is done over the day if no more than 4 hours of data in a row are missing,
and then over the days (grouped by the day of the week) if no more than 4 consecutive weeks
are missing.
}
