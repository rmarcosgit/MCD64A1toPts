\name{burnedPixelDF}
\alias{burnedPixelDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{burnedPixelDF
%%  ~~function to do ... ~~
}
\description{
This function creates a data.frame object (burnedPixelsDF) from the MCD64A1v006 raster layers located in the target folder. The resulting object contains the core data (fireID-clump4-date) to retrieve ignition points and fire perimeters, as well as calculate fire recurrence.
}
\usage{
burnedPixelDF(x,clumpDist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a string value representing the path were subfolders Burn_Date/, Burn_Date_Unc/ and QA/ are located.}
  \item{clumpDist}{a numeric value with the distance threshold (in meters) afar from which a burned pixels is considered an individual fire event.}
  \item{nDays}{a numeric value with the temporal threshold (in days) from which a burned pixels is considered an individual fire event.}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
