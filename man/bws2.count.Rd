\name{bws2.count}
\alias{bws2.count}
\alias{barplot.bws2.count}
\alias{sum.bws2.count}


\title{
Calculating count-based best--worst scaling scores
}


\description{
This function calculates best, worst, best-minus-worst, and standardized 
best-minus-worst scores for each respondent.
}


\usage{
bws2.count(data, ...)

\method{barplot}{bws2.count}(height, score = c("bw", "b", "w"), 
  output = c("level", "attribute"), mfrow = NULL, ...)

\method{sum}{bws2.count}(x, output = c("level", "attribute"), ...)
}


\arguments{
  \item{data}{
A data frame containing the dataset generated from 
\code{bws2.dataset()} with argument \code{type = "marginal"}.
}
  \item{x, height}{
An object of the S3 class \sQuote{bws2.count}.
}
  \item{output}{
A character showing a type of BWS score calculated by this function: 
\code{"attribute"} is assigned to this argument when BWS scores for 
attributes are calculated or \code{"level"} is assigned when BWS scores 
for attribute levels are calculated.
}
  \item{score}{
A character showing a type of the output from this function: \code{"b"} is 
assigned to this argument when the output is based on best scores, \code{"w"} 
is assigned when it is based on worst scores, or \code{"bw"} is assigned 
when it is based on best-minus-worst scores.
}
  \item{mfrow}{
A two-element vector \code{c(nr, nc)}; bar plots will be drawn in an 
\code{nr}-by-\code{nc} array on the device by row.
}
  \item{\dots}{
Arguments passed to function(s) used internally.
}
}


\details{
The \code{bws2.count()} function calculates disaggregated best (B), 
worst (W), best-minus-worst (BW), and standardized BW scores. 
For details on these scores, refer to the Details section on 
the help page of this package.

Output from this function is the object of S3 class \sQuote{bws2.count}, 
which inherits from the S3 class \sQuote{data.frame}. The generic functions 
such as \code{barplot()} and \code{sum()} are available for the S3 class 
\sQuote{bws2.count}. The \code{barplot()} function draws the bar plots of 
B, W, or BW scores for each attribute when \code{output = "attribute"} or 
those for each attribute level when \code{output = "level"}. 
The \code{sum} function returns a data frame containing B, W, BW, and 
standardized BW scores for all respondents for each attribute 
when \code{output = "attribute"} or for each attribute level 
when \code{output = "level"}.
}

\value{
The output from \code{bws2.count()}, which is the object of the S3 class 
\sQuote{bws2.count}, is a data frame containing five types of variables: 
respondent's identification variable, B score variables, W score variables, 
BW score variables, and standardized BW score variables. These scores are 
calculated by each respondent. The names of these score variables are 
\code{b.<name of attribute or attribute level>}, 
\code{w.<name of attribute or attribute level>}, 
\code{bw.<name of attribute or attribute level>}, and 
\code{sbw.<name of attribute or attribute level>}. 
Part \code{<name of attribute or attribute level>} for each score variable 
is set according to the argument \code{attribute.levels} in 
\code{bws2.dataset()} used to generate a dataset for \code{bws2.count()}.

The output has the following attributes:
\item{nquestions}{
A vector showing the number of questions.
}
\item{nrespondents}{
A vector showing the number of respondents.
}
\item{freq.levels}{
A variable showing the frequency of each attribute level in the choice sets.
}
\item{attribute.levels}{
A list of attributes and their levels, which is the same as those assigned 
to argument \code{attribute.levels} in \code{bws2.dataset()} used to 
generate a dataset assigned to argument \code{data} of \code{bws2.count()}.
}
\item{vnames}{
A variable showing the names of each attribute level.
}
\item{b.names}{
A variable showing the names of B score by each attribute level.
}
\item{w.names}{
A variable showing the names of W score by each attribute level.
}
\item{bw.names}{
A variable showing the names of BW score by each attribute level.
}
\item{sbw.names}{
A variable showing the names of standardized BW score by each attribute level.
}
}


\author{
Hideo Aizaki
}


\seealso{
\code{\link{support.BWS2-package}}, \code{\link{bws2.dataset}}
}


\examples{
## See examples in bws2.dataset()
}


\keyword{multivariate}

