% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slushy_log.R
\name{lst_to_df}
\alias{lst_to_df}
\title{Convert a List to a Data Frame}
\usage{
lst_to_df(lst)
}
\arguments{
\item{lst}{List. A structured list where each element contains package details such as `Package`, `Version`, `Source`, `Repository`, `Requirements`, and `Hash`.}
}
\value{
A `data.frame` with columns:
\itemize{
  \item `Package`: Package name (character).
  \item `Version`: Package version (character).
  \item `Source`: Package source (character).
  \item `Repository`: Repository information (character).
  \item `Requirements`: Collapsed requirements as a single string (character).
  \item `Hash`: Package hash (character).
}
}
\description{
Converts a structured list of package details into a tidy data frame for easier analysis and processing.
}
\details{
The function ensures missing fields in the input list are replaced with `NA_character_`. 
If `Requirements` is present, it collapses multiple entries into a single comma-separated string.
}
\examples{
\dontrun{
example_list <- list(
  list(Package = "dplyr", Version = "1.0.10", Source = "CRAN", Repository = "https://cran.r-project.org"),
  list(Package = "ggplot2", Version = "3.4.0", Source = "CRAN", Repository = "https://cran.r-project.org", Requirements = c("grid", "scales"))
)

lst_to_df(example_list)
}

}
\keyword{internal}
