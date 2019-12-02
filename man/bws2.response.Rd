\name{bws2.response}
\alias{bws2.response}

\title{
Generating artificial responses to Case 2 best-worst scaling questions
}

\description{
The function synthesizes responses to Case 2 best-worst scaling (BWS) questions on the basis of a paired (maximum difference) model.
}

\usage{
bws2.response(design, attribute.levels, base.level = NULL,
 b, n, detail = FALSE, seed = NULL)
}

\arguments{
\item{design}{
A matrix or data frame containing an orthogonal main-effect design.
}
\item{attribute.levels}{
A list containing the names of the attributes and their levels.
}
\item{base.level}{
A list containing the base level for each attribute.
}
\item{b}{
A vector containing parameters of independent variables in the model. The vector is used to calculate utilities for alternatives.
}
\item{n}{
An integer value showing the number of respondents in the resultant dataset.
}
\item{detail}{
A logical variable: if \code{TRUE}, the dataset is returned in a detailed format; and if \code{FALSE} (default), the dataset is returned in a simple format.
}
\item{seed}{
Seed for a random number generator.
}
}

\details{
This function synthesizes responses to Case 2 BWS questions on the basis of a paired (maximum difference) model with attribute and/or level variables (see Model 1 and Model 2 in Aizaki and Fogarty (2019) for details). The model assumes that a profile has \eqn{m} attributes and each attribute has two or more levels. The profile is expressed as a combination of \eqn{m} levels. The number of possible pairs where level \eqn{i} is selected as the best and level \eqn{j} is selected as the worst (\eqn{i \neq j}) from \eqn{m} levels is given by \eqn{m \times (m - 1)}. The model also assumes that the respondents select level \eqn{i} as the best and level \eqn{j} as the worst because the difference in utility between levels \eqn{i} and \eqn{j} is the highest among all of the \eqn{m \times (m - 1)} differences in utility. The systematic component of the utility is assumed to be a linear additive function of the attribute and level variables (Model 2 has no attribute variables). If the error component of the utility is assumed to be an independently, identically distributed type I extreme value, the probability of selecting level \eqn{i} as the best and level \eqn{j} as the worst is expressed as a conditional logit model.

Given the parameter values assigned to the argument \code{b} and the choice sets assigned to the argument \code{design}, the function \code{bws2.response} calculates the utility for the levels. The parameter values assigned to the argument \code{b} are set as a numerical vector where the elements correspond to the parameters of attribute and/or level variables. The variables are set according to the model specification. Assume that a profile has four attributes A, B, C, and D with three levels each (e.g., levels A1, A2, and A3 for attribute A). For Model 1, dummy-coded attribute variables and effect-coded level variables are used, an arbitrary attribute is set as the base (reference) level, and an arbitrary level for each attribute is set as the base level. If the parameters of the dummy-coded attribute variables \eqn{D_{A}}, \eqn{D_{B}}, and \eqn{D_{C}} are \eqn{1.75}, \eqn{1.31}, and \eqn{0.84}, respectively (i.e., attribute D is the base attribute), and those of the effect-coded level variables \eqn{D_{A1}}, \eqn{D_{A2}}, \eqn{D_{B1}}, \eqn{D_{B2}}, \eqn{D_{C1}}, \eqn{D_{C2}}, \eqn{D_{D1}}, and \eqn{D_{D2}} are \eqn{-1.24}, \eqn{0.18}, \eqn{-1.11}, \eqn{0.10}, \eqn{-1.11}, \eqn{0.39}, \eqn{-0.25}, and \eqn{-0.37}, respectively (i.e., levels A3, B3, C3, and D3 are the base levels), a vector assigned to the argument \code{b} is given by \code{c(1.75, 1.31, 0.84, 0, -1.24, 0.18, -1.11, 0.10, -1.11, 0.39, -0.25, -0.37)}, where the fourth element corresponds to the base attribute (D), and thus has a value of 0. For Model 2, dummy-coded level variables are used and an arbitrary level is set as the base level. If the parameters of the dummy-coded level variables \eqn{D_{A1}}, \eqn{D_{A2}}, \eqn{D_{A3}}, \eqn{D_{B1}}, \eqn{D_{B2}}, \eqn{D_{B3}}, \eqn{D_{C1}}, \eqn{D_{C2}}, \eqn{D_{C3}}, \eqn{D_{D1}}, and \eqn{D_{D2}} are \eqn{-0.10}, \eqn{1.32}, \eqn{2.19}, \eqn{-0.42}, \eqn{0.79}, \eqn{1.69}, \eqn{-0.89}, \eqn{0.62}, \eqn{0.94}, \eqn{-0.87}, and \eqn{-0.99}, respectively (i.e., level D3 is the base level), a vector assigned to the argument \code{b} is given as \code{c(-0.10, 1.32, 2.19, -0.42, 0.79, 1.69, -0.89, 0.62, 0.94, -0.87, -0.99, 0)}, where the last element corresponds to the base level (D3), and thus has a value of 0. After calculating the utility values (by adding the calculated values of the systematic component of the utility and random numbers generated from a type I extreme value distribution), the function bws2.response finds the pair with the highest difference in utility from the \eqn{m \times (m - 1)} differences in utility.
}

\value{
The function \code{bws2.response} returns a data frame that contains synthesized responses to Case 2 BWS questions, in either a detailed or a simple format. The detailed format dataset contains the following variables, as well as independent variables according to the arguments \code{attribute.levels} and \code{base.level}.

\item{id}{
An identification number of artificial respondents.
}
\item{Q}{
A serial number of questions.
}
\item{PAIR}{
A serial number of possible pairs of the best and worst levels for each question.
}
\item{BEST}{
An alternative number treated as the best in the possible pairs of the best and worst levels.
}
\item{WORST}{
An alternative number treated as the worst in the possible pairs of the best and worst levels.
}
\item{BEST.AT}{
A character showing the attribute corresponding to the level treated as the best in the possible pairs of the best and worst levels for each question.
}
\item{WORST.AT}{
A character showing the attribute corresponding to the level treated as the worst in the possible pairs of the best and worst levels for each question.
}
\item{BEST.LV}{
A character showing the level treated as the best in the possible pairs of the best and worst levels for each question.
}
\item{WORST.LV}{
A character showing the level treated as the worst in the possible pairs of the best and worst levels for each question.
}
\item{RES}{
Responses to BWS questions, taking the value of 1 if a possible pair of the best and worst levels is selected by the synthesized respondents and 0 otherwise.
}
\item{STR}{
A stratification variable used to identify each combination of respondent and question.
}

The simple format dataset contains the following variables.
\item{id}{
An identification number of artificial respondents.
}
\item{Bi}{
A variable describing the row number of the level that is selected as the best in the \eqn{i}-th BWS question. The serial number of questions is appended to the tail of the variable name (e.g., \code{B1} for the first question, \code{B2} for the second question, and \code{B3} for the third question).
}
\item{Wi}{
A variable describing the row number of the level that is selected as the worst in the \eqn{i}-th BWS question. The serial number of questions is appended to the tail of the variable name (e.g., \code{W1} for the first question, \code{W2} for the second question, and \code{W3} for the third question).
}

The detailed format dataset includes a dependent variable and independent variables for the analysis, and thus is available for discrete choice analysis functions such as the function \code{clogit} in the \bold{survival} package. On the other hand, the simple format dataset only contains variables that correspond to responses to BWS questions, as well as \code{id} variable. It must be converted using the function \code{bws2.dataset} in the package for the analysis. For details, see the Examples section.
}

\references{
See the help page for \code{\link{support.BWS2-package}}.
}

\seealso{
\code{\link{support.BWS2-package}}, 
\code{\link[support.BWS2]{bws2.dataset}}, 
\code{\link[survival]{clogit}}
}

\examples{
# The following lines of code synthesize responses to Case 2 BWS questions,
# return them in detailed and simple format, and then fit the models using
# the function clogit in the survival package. The profiles are expressed
# by four attributes with three levels each. The parameters for the attribute
# and level variables are the same as those explained in the Details section.

\dontrun{
# Load packages
library(survival)
library(support.BWS2)

# Set design for BWS2 questions
dsgn <- cbind(
  c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  c(1, 3, 2, 3, 2, 1, 2, 1, 3),
  c(1, 2, 3, 3, 1, 2, 2, 3, 1))

# Synthesize responses to BWS2 questions (Model 1)
## attributes and their levels
attr.lev <- list(
  A = c("A1", "A2", "A3"), B = c("B1", "B2", "B3"),
  C = c("C1", "C2", "C3"), D = c("D1", "D2", "D3"))
## base levels
base.lev <- list(A = "A3", B = "B3", C = "C3", D = "D3")
## parameters
b1 <- c(1.75, 1.31, 0.84, 0,       # pars for A, B, C, and D  
        -1.24, 0.18, -1.11, 0.10,  # pars for A1, A2, B1, and B2
        -1.11, 0.39, -0.25, -0.37) # pars for C1, C2, D1, and D2
## dataset in detailed format
dat.detail1 <- bws2.response(
  design = dsgn,
  attribute.levels = attr.lev,
  base.level = base.lev,
  b = b1,
  n = 100,
  detail = TRUE,
  seed = 123)
str(dat.detail1)
## dataset in simple format 
dat.simple1 <- bws2.response(
  design = dsgn,
  attribute.levels = attr.lev,
  base.level = base.lev,
  b = b1,
  n = 100,
  detail = FALSE,
  seed = 123) 
str(dat.simple1)

# Convert dat.simple1 into dataset for the analysis
rsp.var1 <- colnames(dat.simple1)[-1]
dat.simple1.pr <- bws2.dataset(
  data = dat.simple1,
  id = "id",
  response = rsp.var1,  
  choice.sets = dsgn,        
  attribute.levels = attr.lev,
  base.level = base.lev,
  model = "paired")

# Fit conditional logit models
mf1 <- RES ~ A + B + C + A1 + A2 + B1 + B2 + C1 + C2 + 
             D1 + D2 + strata(STR)
out.detail1 <- clogit(formula = mf1, data = dat.detail1)
out.simple1 <- clogit(formula = mf1, data = dat.simple1.pr)
out.simple1
all.equal(coef(out.detail1), coef(out.simple1))


# Synthesize responses to BWS2 questions (Model 2)
## parameters
b2 <- c(-0.10, 1.32, 2.19, # pars for A1, A2, and A3
        -0.42, 0.79, 1.69, # pars for B1, B2, and B3
        -0.89, 0.62, 0.94, # pars for C1, C2, and C3
        -0.87, -0.99, 0)   # pars for D1, D2, and D3
## dataset in detailed format
dat.detail2 <- bws2.response(
  design = dsgn,
  attribute.levels = attr.lev,
  b = b2,
  n = 100,
  detail = TRUE,
  seed = 123)
str(dat.detail2)
## dataset in simple format 
dat.simple2 <- bws2.response(
  design = dsgn,
  attribute.levels = attr.lev,
  b = b2,
  n = 100,
  detail = FALSE,
  seed = 123) 
str(dat.simple2)

# Convert dat.simple2 into dataset for the analysis
rsp.var2 <- colnames(dat.simple2)[-1]
dat.simple2.pr <- bws2.dataset(
  data = dat.simple2,
  id = "id",
  response = rsp.var2,  
  choice.sets = dsgn,        
  attribute.levels = attr.lev,
  model = "paired")

# Fit conditional logit models
mf2 <- RES ~ A1 + A2 + A3 + B1 + B2 + B3 + C1 + C2 + C3 +
             D1 + D2 + strata(STR)
out.detail2 <- clogit(formula = mf2, data = dat.detail2)
out.simple2 <- clogit(formula = mf2, data = dat.simple2.pr)
out.simple2
all.equal(coef(out.detail2), coef(out.simple2))
}
}

\keyword{ datagen }

