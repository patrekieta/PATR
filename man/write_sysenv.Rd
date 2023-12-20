\name{write_sysenv}
\alias{write_sysenv}

\title{
Create or edit the .Renviron file in your home directory.
}
\description{
This function provides a method of creating or editing an existing .Renviron file. Useful for storing system environment variables that are unique to the user's system.
}
\usage{
write_sysenv()
}

\details{
Opens a new tab in RStudio which allows the user to edit their .Renviron file. Simply save the .Renviron file in the user's home directory for easy access to the data stored in there. The Tab can be closed once the file is saved.
}


\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com},
}
\note{
It should be noted that the user will need to restart their R instance in order for the saved .Renviron file to take effect.
}

\examples{
write_sysenv()
}
