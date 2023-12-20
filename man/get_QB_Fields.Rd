\name{get_QB_Fields}
\alias{get_QB_Fields}

\title{
Queries Quickbase for all fields in the specified table.
}
\description{
This function queries Quickbase through their REST API to get all fields in a specified table. This will also return all the metadata about those fields including things such as Field ID, Field Name, and Field Type. Also provides the option to get the permissions for each field as they relate to the user specified with the userToken attribute.

Gets the properties for all fields in a specific table. The properties for each field are the same as in Get field.
}
\usage{
get_QB_Fields(tableId = "", includeFieldPerms = FALSE, userToken = "", hostname = "")
}
\arguments{
  \item{tableId}{
Quickbase TableId where the data will go. Can be found in the URL or the table settings while in Quickbase. Required*
}
  \item{includeFieldPerms}{
Optional to include the field permissions as they relate to the user specified with the userToken.
}
  \item{userToken}{
API Token which has access to the requested table. Required*
}
  \item{hostname}{
Hostname of the Quickbase instance where the table can be found. You can find this by looking in the URL. Required*
}
}

\value{
Returns a data.frame which contains the fields for the table as well as their metadata.
}
\references{
\href{https://developer.quickbase.com/operation/getFields}{Quickbase Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}

\examples{
get_QB_Fields(tableId = "", includeFieldPerms = FALSE, userToken = Sys.getenv("QB_TOKEN"), hostname = "")
}

