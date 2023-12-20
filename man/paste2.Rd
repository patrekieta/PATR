\name{paste2}
\alias{paste2}

\title{
paste() with option to ignore NA values.
}
\description{
an easy way to concatenate vectors while also having the option to remove NA values. An alternative to \link[base]{paste}.
}
\usage{
paste2(..., sep = " ", collapse = NULL, na.rm = F)
}

\arguments{
  \item{...}{
This is used as the input for all your variables in the paste. Accepts individuals lists, vectors, and data.frame columns.
}
  \item{sep}{
a character string to separate the terms. Not NA_character_.
}
  \item{collapse}{
an optional character string to separate the results. Not NA_character_.
}
  \item{na.rm}{
provides the option to ignore NA characters from your final paste string.
}
}
\details{
paste2 converts its arguments (via as.character) to character strings, and concatenates them (separating them by the string given by sep). If the arguments are vectors, they are concatenated term-by-term to give a character vector result.

If na.rm = FALSE, paste2() coerces NA_character_, the character missing value, to "NA" which may seem undesirable, e.g., when pasting two character vectors, or very desirable, e.g. in paste("the value of p is ", p).

If a value is specified for collapse, the values in the result are then concatenated into a single string, with the elements being separated by the value of collapse.

Shamelessly stolen from this thread \href{https://stackoverflow.com/questions/13673894/suppress-nas-in-paste}{Stack Overflow}
}
\value{
A character vector of the concatenated values. This will be of length zero if all the objects are, unless collapse is non-NULL, in which case it is "" (a single empty string).
}
\references{
\href{https://stackoverflow.com/questions/13673894/suppress-nas-in-paste}{Stack Overflow}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}

\seealso{
\href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/paste}{Paste Documentation}
}
\examples{
### When given a single vector
paste2(1:12)

### When working with data.frame
df <- data.frame("a" = c("a","b"), "b" = c("c","d"))
df$c <- paste2(df$a, df$b)

### When dealing with NA values
df <- data.frame("a" = c("a","b"), "b" = c("c","d"), "c" = c(NA, "e"))
df$d <- paste2(df$a, df$b, df$c, na.rm = TRUE)

}
