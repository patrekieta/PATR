\name{calc_age}
\alias{calc_age}

\title{
calculate the duration between two dates.
}
\description{
A useful function to properly calculate the accurate time between two dates. Typically used to calculate an age.
}
\usage{
calc_age(date, age.day = today(), units = "years", floor = TRUE)
}

\arguments{
  \item{date}{
The calculation date or birthdate that will be used in the calculation. Can be an individual date or a vector of dates.
}
  \item{age.day}{
The target date. Typically today. This only accepts a single date which all calculation dates will be compared against to determine the total difference based on the unites argument.
}
  \item{units}{
This will determine the final output of the calculation. Accepted values are "seconds", "minutes", "days", and "years".
}
  \item{floor}{
Tells function whether to take the floor of the final output. Floor: takes a single numeric argument x and returns a numeric vector containing the largest integers not greater than the corresponding elements of x. See \link[base]{round} for more information. \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/Round}{round() Documentation}
}
}
\value{
Individual Date or vector of Dates
}
\references{
\href{https://stackoverflow.com/questions/27096485/change-a-column-from-birth-date-to-age-in-r}{Stack Overflow Example}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\section{Warning }{
This method relies on the calculations performed in the lubridate package to calculate durations. Values are not accurate beyond the minutes level and could be variable based on time zone.
}


\seealso{
\href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/Round}{round() Documentation}
\href{https://stackoverflow.com/questions/27096485/change-a-column-from-birth-date-to-age-in-r}{Stack Overflow Example}
}
\examples{
calc_age(date = as.Date("2020-01-01"), age.day = today(), units = "years", floor = TRUE)
}

