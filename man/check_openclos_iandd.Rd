% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numbered_checks.R
\name{check_openclos_iandd}
\alias{check_openclos_iandd}
\title{Check 11
Check that the response to I&D is consistent with whether the fracture is open or closed 
in one set of form5.x are related}
\usage{
check_openclos_iandd(form, rep)
}
\arguments{
\item{form}{dataframe containing ptstatus and one set of form5.x}

\item{rep}{which set of form 5.x}
}
\value{
a dataframe containing problematic entries with relevant columns
}
\description{
Check 11
Check that the response to I&D is consistent with whether the fracture is open or closed 
in one set of form5.x are related
}
