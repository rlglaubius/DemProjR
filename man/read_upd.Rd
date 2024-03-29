% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upd-utils.R
\name{read_upd}
\alias{read_upd}
\title{Read demographic inputs from a Spectrum UPD file}
\usage{
read_upd(filename)
}
\value{
A list containing
\enumerate{
\item{location - Country name}
\item{base.pop - Base-year population by year, sex (1:male, 2:female) and age for 1970, 1975, 1980, and 1985}
\item{life.table - life table indicators by year, sex, and age. Includes life expectancy by age (ex), survival ratios (Sx), and numbers left alive (lx). UPD files store lx for ages 1 and 5 only.}
\item{tfr - Total fertility rate by year}
\item{srb - Sex ratio at birth by year}
\item{pasfrs - Proportionate age-specific fertility rates by year and age}
\item{migr - net number of migrants by year, sex, and age}
}
}
\description{
Read demographic inputs from a Spectrum UPD file
}
