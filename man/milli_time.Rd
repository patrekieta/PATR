\name{milli_time}
\alias{milli_time}

\title{
calculate system run time in milliseconds for a given expression.
}
\description{
Provides a method of calculation the runtime for any given expression.
}
\usage{
milli_time(expr)
}

\arguments{
  \item{expr}{
The Expression that will be used while calculating the runtime of the provided expression.
}
}
\details{
Can provide any form of expression to this function. Outputs the amount of milliseconds that it took to run the provided expression. This function will still run the provided expression and the expression will still have an output. Works similar to \link[base]{system.time}. \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/system.time}{syste.time() Documentation}
}
\value{
numeric
}

\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\note{
It should be noted that calculating the starting and ending time of the expression adds to the overall runtime length compared to just running the expression without milli_time().
}

\seealso{
\href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/system.time}{syste.time() Documentation}
}
\examples{
milli_time(
  10000*2000
)
}
