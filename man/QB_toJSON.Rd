\name{QB_toJSON}
\alias{QB_toJSON}

\title{
Convert a data.frame into proper JSON formatting for Quickbase
}
\description{
This function accepts a data.frame and converts it into a JSON format that is accepted by Quickbase. Column names should match up to Quickbase field names. Additionally, it requires the tableID for where the data will go. This function is also used in \link[DSTR]{POST_QB_Data}.
}
\usage{
QB_toJSON(x, tableId = "")
}

\arguments{
  \item{x}{
Data.frame which contains the data you want in Quickbase.
}
  \item{tableId}{
tableID for the Quickbase table where the data will be entered.
}
}
\value{
Returns a String that is formatted JSON.
}
\references{
\href{https://developer.quickbase.com/operation/upsert}{Quickbase Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\section{Warning}{
This function does not currently allow for Multi-Select Fields in Quickbase.
}


\examples{
df <- data.frame("Record ID#" = c(10),"Statement" = c("This is a test."))

QB_toJSON(x = df, tableId = "")
}

