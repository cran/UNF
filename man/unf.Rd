\name{unf}
\alias{unf}
\title{universal numeric fingerprint}
\description{
A universal numeric fingerprint is used to guarantee that a defined subset of data is substantively identical to a comparison subset. Two fingerprints will match if and only if the subset of data generating them are identical, when represented using a given number of significant digits.
}
\usage{
	unf (data, digits=6, version=3)
}

\arguments{
	\item{data}{A numeric or charactervector or data frame. Other types will be computed.}	
	\item{digits}{number of digits used in rounding (for numeric values) or truncation (for characters) prior to applying cryptographic hash}
	\item{version}{algorithmic version. Always use the same version of the algorithm to check a signature.}
}
\details{
A UNF is created by rounding data values (or truncating strings)  to a known number of 
digits (characters), representing those values in standard form (as 32bit unicode-formatted
strings), and applying a fingerprinting method (such as cryptographic
hashing function) to this representation.  
UNF's are computed from data values provided by the statistical package,
 so they directly reflect the internal representation of the data -- 
the data as the statistical package interprets it.

A UNF differs from an ordinary file checksum in several important ways:

1. \emph{UNF's are format independent.}  The UNF for the data will be the same regardless
of whether the data is saved as a R binary format, 
SAS formatted file, Stata formatted file, etc., but
file checksums will differ.

2. \emph{UNF's are robust to insignificant rounding error.} 
A UNF will also be the same if the data differs in non-significant digits, a file checksum not.

3.\emph{UNF's detect misinterpretation of the data by the statistical software.} 
If the statistical software misreads the file, the resulting UNF will not match the original, but the file checksums may match.

4.\emph{UNF's are strongly tamper resistant.} Any accidental or intentional changes to
the data values will change the resulting UNF. Most file checksums's and 
descriptive statistics detect only certain types of changes. 

UNF libraries are available for standalone use, for use in C++, and for use with other packages.
}

\value{
Returns a character string representing the UNF computed from the data.
For example:
	UNF:3:6:ZNQRI14053UZq389x0Bffg==

This representation identifies the signature as a fingerprint, using version 3,
 of the algorithm,  computed to 6 significant digits. The segment following the final colon is the actual fingerprint in base64 encoded format.

Note: to compare two UNF's, or sets of unfs, one often wants to compare only the base64 portion. Use \code{as.character} for this, which will extract the base64 portion.
Use \code{summary} to produce a single UNF from set of vectors.


}

\references{
Altman, M., J. Gill and M. P. McDonald.  2003.  \emph{Numerical Issues in Statistical
Computing for the Social Scientist}.  John Wiley \& Sons.
\url{http://www.hmdc.harvard.edu/numerical_issues/}
}


\examples{

# simple example
v=1:100/10 +.0111 
vr=signif(v,digits=2)

# print.unf shows in  standard format, including version and digits
print(unf(v))

# as.character will return base64 section only for comparisons
as.character(unf(v))

# this is false,  since computed  base64 values UNF's differ
as.character(unf(v))==as.character(unf(vr))

# this is true,  since computed UNF's base64 values are the same at 2 significant digits
as.character(unf(v, digits=2))==as.character(unf(vr))

# WARNING: this is false, since UNF's values are the same, but 
# number of calculated digits differ , probably not the comparison
# you intend

identical(unf(v,digits=2),unf(vr))

# compute a fingerprint of longley at 10 significant digits of accuracy
# this fingerprint can be stored and verified when reading the dataset
# later
data(longley)
mf10<-unf(longley,digits=10);

#printable representation, prints seven UNF's, one for each vector
print(mf10)

#  summarizes the base64 portion of the unf for each vector into a 
# single  base64 UNF representing entire dataset
summary(mf10)

}

\author{
Micah Altman
\email{Micah\_Altman@harvard.edu}

\url{http://www.hmdc.harvard.edu/micah_altman/}
}

\keyword{misc}
\keyword{debugging}
