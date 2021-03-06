% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/org_classify_description_texts.R
\docType{data}
\name{irs_index}
\alias{irs_index}
\title{Index table of all available IRS filings}
\format{
A data frame with 3235913 rows and 14 variables:
\describe{
\item{EIN}{IRS Employer Identification Number}
\item{TaxPeriod}{Corresponds to YYYMM of the tax period end date}
\item{DLN}{Download Number}
\item{FormType}{Version of the 990 Form filed}
\item{URL}{location on the AWS server of the XML parsed filing}
\item{SubmittedOn}{Submission date}
\item{ObjectId}{id of the filing on the server}
\item{OrganizationName}{name of organization}
\item{LastUpdated}{date last updated}
\item{IRS_year}{Calendar year the IRS processed the filing. Corresponds to AWS index year.}
\item{TaxPeriodBeginDt}{Start of the filings tax period}
\item{TaxPeriodEndDt}{End of the filings tax period}
\item{Tax_Year}{Year of the tax filing}
\item{ReturnTs}{timestamp of the tax return}
}
}
\usage{
irs_index
}
\description{
A dataset containing the AWS server information for available IRS filings. This index
is refactored from the AWS index files. The AWS files are arranged by year the IRS
processed the file. These are arranged by tax year.
}
\keyword{datasets}
