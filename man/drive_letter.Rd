\name{drive_letter}
\alias{drive_letter}

\title{
Discover the system drive letter for a given drive name.
}
\description{
Function to discover which drive letter on your computer corresponds to name provided.
}
\usage{
drive_letter(name = "")
}

\arguments{
  \item{name}{
Provide the name of the computer drive that will be looked up to find the corresponding mapped drive letter.
}
}

\value{
Character
}

\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\note{
This method only works on windows systems.
}

\seealso{
\href{https://learn.microsoft.com/en-us/windows/win32/cimwin32prov/win32-logicaldisk}{Microsoft Documentation}
}
\examples{
drive_letter(name = "Test")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
