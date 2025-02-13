% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{date_split}
\alias{date_split}
\title{Add columns that split date into constituent parts}
\usage{
date_split(df)
}
\arguments{
\item{df}{data frame output of import functions (add link)}
}
\value{
data frame with 8 more columns than before
}
\description{
Splits the date into day, month and year columns as well as the ISO
week and adds all four columns. Also add  ISO year and week in YYYY-WxX format,
because teden and leto are not enough tu unambiguously determine the correct iso week.
Then also adds the "Equivalent last year" or ELY week, which is the one exactly
52 weeks ago. WHich is the simplest way to do y-o-y stuff I believe.
For convenienve adds a column with the year-month in YYYY-MM format and the
"Equivalent last year" for easy y-o-y calculations
}
