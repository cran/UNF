% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unf.R, R/unf3.R, R/unf4.R, R/unf5.R, R/unf6.R
\name{unf}
\alias{unf}
\alias{unf3}
\alias{unf4}
\alias{unf5}
\alias{unf6}
\title{Universal Numeric Fingerprint}
\usage{
unf(x, version = 6, ...)

unf3(
  x,
  digits = 7L,
  characters = 128L,
  factor_as_character = TRUE,
  nonfinites_as_missing = FALSE,
  empty_character_as_missing = FALSE,
  dvn_zero = FALSE,
  ...
)

unf4(
  x,
  digits = 7L,
  characters = 128L,
  truncation = 128L,
  version = 4,
  factor_as_character = TRUE,
  nonfinites_as_missing = FALSE,
  empty_character_as_missing = FALSE,
  dvn_zero = FALSE,
  ...
)

unf5(
  x,
  digits = 7L,
  characters = 128L,
  truncation = 128L,
  raw_as_character = TRUE,
  factor_as_character = TRUE,
  nonfinites_as_missing = FALSE,
  empty_character_as_missing = FALSE,
  dvn_zero = FALSE,
  timezone = "",
  date_format = "\%Y-\%m-\%d",
  decimal_seconds = 5,
  ...
)

unf6(
  x,
  digits = 7L,
  characters = 128L,
  truncation = 128L,
  raw_as_character = TRUE,
  factor_as_character = TRUE,
  complex_as_character = TRUE,
  nonfinites_as_missing = FALSE,
  timezone = "",
  date_format = "\%Y-\%m-\%d",
  decimal_seconds = 5,
  ...
)
}
\arguments{
\item{x}{For \code{unf}, a vector, matrix, dataframe, or list; for \code{unf3}, \code{unf4}, \code{unf5}, a vector. If \code{x} is a dataframe or list with one variable or one vector element, respectively, \code{unf} returns the UNF for the single vector (which is consistent with the Dataverse implementation but ambiguous in the UNF standard). For algorithm versions < 5, all non-numeric vectors are treated as character.}

\item{version}{Version of the UNF algorithm. Allowed values are 3, 4, 4.1, 5, and 6. Always use the same version of the algorithm to check a UNF. Default for \code{unf} is 6 and default for \code{unf4} is 4 (but can also be set to 4.1, which is identical except for using SHA256 instead of MD5).}

\item{digits}{The number of significant digits for rounding for numeric values. Default is 7L. Must be between 1 and 15.}

\item{characters}{The number of characters for truncation. Default is 128L. Must be greater than 1.}

\item{factor_as_character}{A logical indicating whether to treat an factors as character. If \code{FALSE}, factor variables are treated as integer (and thus handled as any numeric value).}

\item{nonfinites_as_missing}{A logical indicating whether to treat nonfinite values (\code{NaN}, \code{Inf}, \code{-Inf}) as \code{NA}. This is supplied to create compatibility with a Dataverse UNFv5 implementation.}

\item{empty_character_as_missing}{A logical indicating whether to treat an empty character string as a missing value. This is supplied to create compatibility with a Dataverse UNFv5 implementation.}

\item{dvn_zero}{A logical indicating whether to format a zero (0) numeric value as \code{+0.e-6} instead of the default \code{+0.e+}. This is supplied to create compatibility with a Dataverse UNFv5 implementation, backwards compatibility with v1.0 of the UNF package (for UNFv3, UNFv4, UNFv4.1).}

\item{truncation}{The number of bits to truncate the UNF signature to. Default is 128L. Must be one of: 128,192,196,256.}

\item{raw_as_character}{A logical indicating whether to format raw vectors as character.}

\item{timezone}{A character string containing a valid timezone. This is used for formatting \dQuote{Date} and \dQuote{POSIXt} class variables. Because of different implementations of datetime classes across computer applications, UNF signatures may vary due to the timezone in which they are calculated. This parameter allows for the comparison of UNFs calculated in different timezones.}

\item{date_format}{A character string containing a formatting pattern for \dQuote{Date} class variables. One of \code{'\%Y-\%m-\%d'} (the default), \code{'\%Y-\%m'}, \code{'\%Y'}, \code{'\%F'}.}

\item{decimal_seconds}{A number indicating the number of decimal places to round fractional seconds to. The UNF specification (and default) is 5.}

\item{complex_as_character}{A logical indicating whether to format raw vectors as character. If \code{TRUE}, UNF should match Dataverse UNFv5 implementation. If \code{FALSE}, complex numbers are formatted as \code{A,iB}.}

\item{\ldots}{Additional arguments passed to specific algorithm functions. Ignored.}
}
\value{
The \code{unf} function returns a list of class \code{UNF}, containing:
\itemize{
  \item \code{unf}: A character string containing the universal numeric fingerprint.
  \item \code{hash}: A raw vector expressing the unencoded universal numeric fingerprint. This can be converted to a UNF using \code{base64Encode}.
  \item \code{unflong}: For \code{unf5}, a character string containing the un-truncated universal numeric fingerprint.
  \item \code{formatted}: A character string containing the formatted UNF, including version number and header attributes.
}
The object additionally contains several attributes:
\itemize{
  \item \code{version}: A one-element numeric vector specifying which version of the UNF algorithm was used to generate the object.
  \item \code{digits}: A one-element numeric vector specifying how many significant digits were used in rounding numeric values.
  \item \code{characters}: A one-element numeric vector specifying how many characters were preserved during truncation of character values.
  \item \code{truncation}: A one-element numeric vector specifying how many bits the UNF hash was truncated to.
}

The default print method displays the UNF along with these attributes. For example:
\code{UNF:3:4,128:ZNQRI14053UZq389x0Bffg==}
This representation identifies the signature as UNF, using version 3 of the algorithm, computed to 4 significant digits for numbers and 128 for characters. The segment following the final colon is the actual fingerprint in base64-encoded format.
}
\description{
UNF is a cryptographic hash or signature that can be used to uniquely identify (a version of) a dataset, or a subset thereof.
}
\details{
The Dataverse Network implements a potentially incorrect version of the UNF algorithm with regard to the handling of zero values and logical \code{FALSE} values in data (though the specification is unclear). Setting the \code{dvn} argument to \code{TRUE} (the default), uses the Dataverse implementation (for comparison to files stored in that archive).
}
\examples{
# Version 6 #

### FORTHCOMING ###

# Version 5 #
## vectors

### just numerics
unf5(1:20) # UNF:5:/FIOZM/29oC3TK/IE52m2A==
unf5(-3:3, dvn_zero = TRUE) # UNF:5:pwzm1tdPaqypPWRWDeW6Jw==

### characters and factors
unf5(c('test','1','2','3')) # UNF:5:fH4NJMYkaAJ16OWMEE+zpQ==
unf5(as.factor(c('test','1','2','3'))) # UNF:5:fH4NJMYkaAJ16OWMEE+zpQ==

### logicals
unf5(c(TRUE,TRUE,FALSE), dvn_zero=TRUE)# UNF:5:DedhGlU7W6o2CBelrIZ3iw==

### missing values
unf5(c(1:5,NA)) # UNF:5:Msnz4m7QVvqBUWxxrE7kNQ==

## variable order and object structure is irrelevant
unf(data.frame(1:3,4:6,7:9)) # UNF:5:ukDZSJXck7fn4SlPJMPFTQ==
unf(data.frame(7:9,1:3,4:6))
unf(list(1:3,4:6,7:9))

# Version 4 #
# version 4
data(longley)
unf(longley, ver=4, digits=3) # PjAV6/R6Kdg0urKrDVDzfMPWJrsBn5FfOdZVr9W8Ybg=

# version 4.1
unf(longley, ver=4.1, digits=3) # 8nzEDWbNacXlv5Zypp+3YCQgMao/eNusOv/u5GmBj9I=

# Version 3 #
x1 <- 1:20
x2 <- x1 + .00001

unf3(x1) # HRSmPi9QZzlIA+KwmDNP8w==
unf3(x2) # OhFpUw1lrpTE+csF30Ut4Q==

# UNFs are identical at specified level of rounding
identical(unf3(x1), unf3(x2))
identical(unf3(x1, digits=5),unf3(x2, digits=5))

# dataframes, matrices, and lists are all treated identically:
unf(cbind.data.frame(x1,x2),ver=3) # E8+DS5SG4CSoM7j8KAkC9A==
unf(list(x1,x2), ver=3)
unf(cbind(x1,x2), ver=3)

}
\references{
\url{https://guides.dataverse.org/en/latest/developers/unf/index.html}

Altman, M., J. Gill and M. P. McDonald. 2003. \emph{Numerical Issues in Statistical Computing for the Social Scientist}. John Wiley \& Sons. [Describes version 3 of the algorithm]

Altman, M., \& G. King. 2007. A Proposed Standard for the Scholarly Citation of Quantitative Data. \emph{D-Lib} 13(3/4). \url{http://dlib.org/dlib/march07/altman/03altman.html} [Describes a citation standard using UNFs]

Altman, M. 2008. A Fingerprint Method for Scientific Data Verification. In T. Sobh, editor, Advances in Computer and Information Sciences and Engineering, chapter 57, pages 311--316. Springer Netherlands, Netherlands, 2008. \url{https://link.springer.com/chapter/10.1007/978-1-4020-8741-7_57} [Describes version 5 of the algorithm]

Data Citation Synthesis Group. 2013. Declaration of Data Citation Principles [DRAFT]. \url{https://force11.org/info/joint-declaration-of-data-citation-principles-final/}. [Describes general principles of data citation, of which UNF is likely to be a part]
}
\seealso{
\code{\link{\%unf\%}}
}
