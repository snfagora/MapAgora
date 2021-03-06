% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/org_classify_description_texts.R
\name{get_aws_url_from_server}
\alias{get_aws_url_from_server}
\title{Get the Amazon Web Server URL associated with a particular Employment Identification Numbers}
\usage{
get_aws_url_from_server(
  ein,
  year = 2019,
  form = NULL,
  move_global = TRUE,
  tax_period = 2019,
  multiple_tax_period = FALSE
)
}
\arguments{
\item{ein}{An Employment Identification Numbers (EIN)}

\item{year}{A year in which a form was filed. The default IRS year is 2019.}

\item{form}{An IRS document form. The default is NULL. There are three other options: "990", "990PF", "990EZ"}

\item{move_global}{Whether moving the XML file, which contains the 990 forms filed in a particular year, to the user's global environment. The default value is TRUE.}

\item{tax_period}{A tax period (year-month). The default tax period is 2019.}

\item{multiple_tax_period}{This is only applied to the case when an EIN is associated with multiple tax periods. If you want to extract the latest tax period's latest submission, set this argument to FALSE. If you want to extract the latest submission from the multiple tax periods, set this argument to TRUE. The default value is FALSE.}
}
\value{
If a single tax period was identified, the function returns the Amazon Web Server URL associated with a particular Employment Identification Numbers. If multiple tax periods were identified, the function returns either a vector that only includes the URL (multiple_tax_period= FALSE) or a dataframe that includes the URL as well as related tax periods (multiple_tax_period = TRUE).
}
\description{
Get the Amazon Web Server URL associated with a particular Employment Identification Numbers
}
