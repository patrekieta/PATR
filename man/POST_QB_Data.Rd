\name{POST_QB_Data}
\alias{POST_QB_Data}

\title{
Insert or Update Quickbase Records.
}
\description{
This function uses the Quickbase REST API to insert or update records in a given Quickbase table. This accepts a data.frame where the column names match to the name of an existing field in the Quickbase table.

Insert and/or update record(s) in a table. In this single API call, inserts and updates can be submitted. Update can use the key field on the table, or any other supported unique field. Refer to \href{https://developer.quickbase.com/fieldInfo}{Quickbase Fields} for more information about how each field type should be formatted. This operation allows for incremental processing of successful records, even when some of the records fail.

This F
}
\usage{
POST_QB_Data(x, tableId = "", userToken = "", hostname = "")
}

\arguments{
  \item{x}{
The data.frame that will be inserted into Quickbase.
}
  \item{tableId}{
Quickbase TableId where the data will go. Can be found in the URL or the table settings while in Quickbase. Required*
}
  \item{userToken}{
API Token which has access to the requested table. Required*
}
  \item{hostname}{
Hostname of the Quickbase instance where the table can be found. You can find this by looking in the URL. Required*
}
}

\value{
Returns the API response to this request.
}
\references{
\href{https://developer.quickbase.com/operation/upsert}{Quickbase Documentation}
\href{https://developer.quickbase.com/fieldInfo}{Quickbase Fields},
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\note{
Multi-Select fields are not currently allowed in this process. They will break the JSON output from \link[DSTR]{QB_toJSON}.

This endpoint supports a maximum payload size of 10MB.
}



\seealso{
\link[DSTR]{QB_toJSON},
\link[DSTR]{get_QB_Fields}
}
\examples{
df <- data.frame("Record ID#" = c(10),"Statement" = c("This is a test."))

POST_QB_Data(x = df, tableId = "", userToken = Sys.getenv("QB_TOKEN"))
}

