\name{QB_DELETE}
\alias{QB_DELETE}

\title{
Delete Quickbase Records using record IDs or a query.
}
\description{
This function uses the Quickbase REST API to perform either bulk deletes from a single or numeric vector of Quickbase Record IDs. If you set recordID to FALSE, you can instead use the Quickbase Query language to indicate which records should be deleted. You can only perform these actions if the userToken you use has delete privelages in the requested table.

Deletes record(s) in a table based on a query. Alternatively, all records in the table can be deleted.

THIS FUNCTION SHOULD BE USED WITH GREAT CAUTION!!!
}
\usage{
QB_DELETE(x, tableId = "", userToken = "", hostname = "", recordID = TRUE, where = "")
}

\arguments{
  \item{x}{
A single or vector numeric. This should contain the record IDs if recordID is TRUE.
}
  \item{tableId}{
A string that corresponds to the Quickbase table that will be altered.
}
  \item{userToken}{
API Token which has access to the requested table. Required*
}
  \item{hostname}{
Hostname of the Quickbase instance where the table can be found. You can find this by looking in the URL. Required*
}
  \item{recordID}{
A logical to indicate whether you are using a list of Record IDs or a query.
}
  \item{where}{
A string that corresponds to the query you wish to use for deletion. This is only considered when recordID = FALSE. For more information on the Quickbase query language use \href{https://helpv2.quickbase.com/hc/en-us/articles/4418287644308-Components-of-a-Query}{Quickbase Query Syntax}.
}
}

\value{
If recordID = TRUE, this will return a dataframe with the provided Record IDs and a logical of if the corresponding record ID was deleted.
If recordID = FALSE, this will return the API response.
}
\references{
\href{https://developer.quickbase.com/operation/deleteRecords}{Quickbase Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\note{
When righting a query, you should use "'" to indicate a string. Using "" around your string will break the API and result in an error.
}


\examples{
recordid <- 2

QB_DELETE(x = recordid, tableId = "", userToken = Sys.getenv("QB_TOKEN"))
}

