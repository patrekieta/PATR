\name{get_QB_query}
\alias{get_QB_query}

\title{
Query for Quickbase data with the Quickbase REST API.
}
\description{
Prebuilt API Query for general Quickbase tables. Uses Quickbase API to download data and clean with proper column names.For additional information you can visit: \href{https://developer.quickbase.com/operation/runQuery}{Quickbase Documentation}
}
\usage{
get_QB_query(from = "", select = "",  where = "", userToken = "", large_query = FALSE, hostname = "")
}

\arguments{
  \item{from}{
The table identifier. This is the table Id where the data can be found. The table ID can be found in the table settings or in the url of the table. *Required
  }
  \item{select}{
An array of field ids for the fields that should be returned in the response. If empty, the default columns on the table will be returned. Field IDs should be surrounded by [] and IDs inside should be seperated by commas.
  }
  \item{where}{
The filter, using the Quickbase query language, which determines the records to return. If this parameter is omitted, the query will return all records. For more information on the Quickbase query language use \href{https://helpv2.quickbase.com/hc/en-us/articles/4418287644308-Components-of-a-Query}{Quickbase Query Syntax}.
  }
  \item{userToken}{
  Quickbase Authentication Token. Found under myPreferences in Quickbase. *Required
  }
  \item{large_query}{
If the report is expected to be too large, we can indicate this here. Using large_query will force the function to grab the report 5000 records at a time. Will cause the request to take longer.
  }
  \item{hostname}{
Necessary hostname for where the quickbase data is stored.
  }
}

\value{
Returns a complete dataframe which should correspond to the Quickbase Report.
}
\references{
\href{https://helpv2.quickbase.com/hc/en-us/articles/4418287644308-Components-of-a-Query}{Quickbase Query Syntax}
\href{https://developer.quickbase.com/operation/runQuery}{Quickbase Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}


\examples{
get_QB_query(from = "", select = "[1,3]",  where = "{3.OAF.01/01/1970}", userToken = Sys.getenv("QB_TOKEN"), large_query = FALSE)
}
