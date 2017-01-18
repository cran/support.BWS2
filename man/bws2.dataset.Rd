\name{bws2.dataset}
\alias{bws2.dataset}
\alias{bws2.dataset.marginal}
\alias{bws2.dataset.paired}

\title{
Creating a dataset suitable for Case 2 best--worst scaling analysis using 
counting and modeling approaches
}


\description{
This function creates a dataset used for \code{bws2.count()} in 
\pkg{support.BWS2} and functions for discrete choice models such as 
\code{clogit()} in \pkg{survival}. 
}


\usage{
bws2.dataset(data, id, response, choice.sets, attribute.levels, 
  attribute.variables = c("reverse", "constant"), effect = NULL, 
  delete.best = FALSE, type = c("paired", "marginal"), ...)
}


\arguments{
  \item{data}{
A data frame containing a respondent dataset.
}
  \item{id}{
A character showing the name of the respondent identification number 
variable used in the respondent dataset.
}
  \item{response}{
A vector containing the names of response variables in the respondent 
dataset, showing the best and worst attribute levels selected in each 
Case 2 BWS question.
}
  \item{choice.sets}{
A data frame or matrix containing an orthogonal main-effect design.
}
  \item{attribute.levels}{
A list containing the names of the attributes and their levels.
}
  \item{attribute.variables}{
A character showing a type of attribute variables, denoted by 
\code{"reverse"} when the attribute variables take the value of 
\code{1} for a possible best, \code{-1} for a possible worst, and 
\code{0} otherwise, or \code{"constant"} when the attribute variables 
are created as attribute-specific constants.
}
  \item{effect}{
A list containing the base level in each attribute: the argument is used 
when attribute level variables are created as effect coded ones and 
while \code{NULL} is assigned to the argument when attribute level 
variables are created as dummy coded ones.
}
  \item{delete.best}{
A logical value denoted by \code{TRUE} when deleting an attribute level 
selected as the best in the worst choice set (that is, using a marginal 
sequential model) or \code{FALSE} when not doing so.
}
  \item{type}{
A character showing a type of dataset created by this function: 
\code{"paired"} for a paired model and \code{"marginal"} for a 
marginal (sequential) model.
}
  \item{\dots}{
Optional arguments; currently not in use.
}
}


\details{
The respondent dataset, in which each row corresponds to a respondent, 
must be organized by users and then assigned to the argument \code{data}. 
The dataset must include the respondent's identification number (id) 
variable in the first column and the response variables in the subsequent 
columns, each indicating which attribute levels are selected as 
the best and worst for each question. Other variables in the respondent 
dataset are treated as the respondents' characteristics such as gender 
and age. Respondents' characteristic variables are also stored in 
the resultant dataset created by the function \code{bws2.dataset()}. 
Although the names of the id and response variables are left to 
the discretion of the user, those of the id and response variables are 
assigned to the arguments \code{id} and \code{response}. 

The response variables must be constructed such that the best attribute 
levels alternate with the worst by question. For example, when there are 
nine BWS questions, the variables are B1, W1, B2, W2, ..., B9, and W9. 
Here, B\eqn{i} and W\eqn{i} show the attribute levels selected 
as the best and worst in the \eqn{i}-th question. 
The row numbers of the attribute levels 
selected as the best and worst are stored in the response variables. 
For example, suppose that a respondent was asked to answer the following 
BWS question, which is the same as that shown on the help page of 
this package, and then selected A1 (attribute level in the first row) as 
the best and C2 (attribute level in the third row) as the worst.

\tabular{c}{
Please select your best and worst attribute levels from the following four:}
\tabular{ccc}{
Best \tab Attribute \tab Worst \cr
[_] \tab A1 \tab [_] \cr
[_] \tab B3 \tab [_] \cr
[_] \tab C2 \tab [_] \cr
[_] \tab D3 \tab [_] \cr
}

The response variables B1 and W1, corresponding to the respondent's answer 
to this question, take the value of \code{1} (= the attribute level in 
the first row) and \code{3} (= the attribute level in the third row).

The arguments \code{choice.sets} and \code{attribute.levels} are 
the same as those in \code{bws2.questionnaire()}. 
The order of questions in the respondent dataset has to be 
the same as that in \code{choice.sets}.

The arguments \code{type}, \code{attribute.variables}, \code{effect}, and 
\code{delete.best} are set according to the model you will use: argument 
\code{type} is set as \code{"paired"} for the paired model, or 
\code{"marginal"} for the marginal (sequential) model; 
the argument \code{attribute.variables} is set as \code{"reverse"} 
for a model in which attribute variables take the value of \code{1} 
if the associated attribute-level variables are treated as 
the possible best, \code{-1} for the possible worst, and \code{0} 
otherwise (Flynn et al. 2007 and 2008), while it is set as 
\code{"constant"} for a model with attribute-specific 
constants in which the attribute variables takes the value of \code{1} 
if the associated attribute-levels are treated as the possible best or 
worst, and \code{0} otherwise (Hensher et al. 2015, Appendix 6B); 
the argument \code{effetct} is set as a list containing the base level 
in each attribute for a model with effect-coded attribute level 
variables (Flynn et al. 2007 and 2008), while it is set as 
\code{NULL} for a model with dummy-coded attribute level variables 
(Hensher et al. 2015, Appendix 6B); 
and the argument \code{delete.best} is set as \code{TRUE} for 
the marginal sequential model, or \code{FALSE} otherwise.
}


\value{
The function returns a dataset in data frame format for the paired model 
or one for the marginal (sequential) model. The dataset for the paired 
model contains the following variables and attribute and/or 
attribute-level variables explained above:
\item{id}{A respondent's identification number; the actual name and 
values of this variable is set according to the id variable in 
the respondent dataset.}
\item{Q}{A serial number of BWS questions.}
\item{PAIR}{A serial number for the possible pairs of the best and worst 
attribute levels for each question.}
\item{BEST}{An attribute-level number treated as the best in the possible 
pairs of the best and worst attribute levels for each question.}
\item{WORST}{An attribute-level number treated as the worst in the possible 
pairs of the best and worst attribute levels for each question.}
\item{BEST.AT}{A character showing the attribute corresponding to 
the attribute level treated as the best in the possible pairs of 
the best and worst attribute levels for each question.}
\item{WORST.AT}{A character showing the attribute corresponding to 
the attribute level treated as the worst in the possible pairs of 
the best and worst attribute levels for each question.}
\item{BEST.LV}{A character showing the attribute level treated as 
the best in the possible pairs of the best and worst attribute levels 
for each question.}
\item{WORST.LV}{A character showing the attribute level treated as 
the worst in the possible pairs of the best and worst attribute levels 
for each question.}
\item{RES.B}{A row number in the profile corresponding to 
the attribute level selected as the best by respondents.}
\item{RES.W}{A row number in the profile corresponding to 
the attribute level selected as the worst by respondents.}
\item{RES}{Responses to BWS questions that takes the value of \code{1} 
if a possible pair of the best and worst attribute levels is selected 
by respondents and \code{0} otherwise: this variable is used as 
a dependent variable in the model formula of the function for discrete 
choice analysis (e.g., \code{clogit()} in the package 
\pkg{survival}).}
\item{STR}{A stratification variable identifying each combination of 
respondent and question; the variable is also used in the model formula 
of \code{clogit()}.}

The dataset for the marginal (sequential) model contains the variables 
\code{id}, \code{Q}, \code{RES.B}, \code{RES.W}, and \code{STR} 
mentioned above and the following variables:
\item{ALT}{A serial number of alternatives (attribute levels) for 
each question.}
\item{BW}{A state variable that takes the value of \code{1} for 
the possible best attribute levels and \code{-1} for 
the possible worst attribute levels.}
\item{ATT.cha}{A character showing the attribute corresponding to 
the attribute level treated as the possible best or worst for each question.}
\item{ATT}{An attribute number showing the attribute corresponding to 
the attribute level treated as the possible best or worst for each question.}
\item{LEV.cha}{A character showing the attribute levels treated as 
the possible best or worst for each question.}
\item{LEV}{An attribute level number showing the attribute level 
treated as the possible best or worst for each question.}
\item{RES}{Responses to BWS questions that takes the value of \code{1} 
if the possible best or worst attribute level is selected 
by respondents and \code{0} otherwise.}

The output has its attributes that consist of arguments assigned to 
this function (i.e., \code{id}, \code{response}, \code{choice.sets}, 
\code{attribute.levels}, \code{attribute.variables}, \code{effect}, 
\code{delete.best}, and \code{type}) and the following:
\item{design.matrix}{Design matrix.}
\item{lev.var.wo.ref}{Names of attribute-level variables excluding 
base levels.}
\item{freq.levels}{Frequency of attribute levels shown in 
all the questions.}
\item{respondent.characteristics}{Names of variables corresponding to 
the respondents' characteristics: variables, except for the respondents' 
id and response variables, are considered the respondents' characteristics.}
Note that the attributes \code{delete.best} and \code{freq.levels} are valid 
for the marginal (sequential) model.
}


\references{
Flynn TN, Louviere JJ, Peters TJ, Coast J (2007) Best-worst scaling: 
What it can do for health care research and how to do it. 
\emph{Journal of Health Economics}, \bold{26}, 171--189.

Flynn TN, Louviere JJ, Peters TJ, Coast J (2008) Estimating preferences 
for a dermatology consultation using best-worst scaling: 
Comparison of various methods of analysis. 
\emph{BMC Medical Research Methodology}, \bold{8}(76). 
doi: 10.1186/1471-2288-8-76.

Hensher DA, Rose JM, Greene WH (2015) \emph{Applied Choice Analysis}. 
2nd edition. Cambridge University Press.
}


\author{
Hideo Aizaki
}



\seealso{
\code{\link{support.BWS2-package}}, 
\code{\link[DoE.base]{oa.design}}, 
\code{\link[survival]{clogit}}
}



\examples{
# Load package survival used for a conditional logit model analysis of
# the responses
require(survival)

# Set a three-level orthogonal main-effect design (OMED) with
# four columns
omed <- matrix(
c(
1,3,2,3,
3,1,2,2,
3,3,3,1,
2,3,1,2,
2,2,2,1,
1,1,1,1,
1,2,3,2,
3,2,1,3,
2,1,3,3),
nrow = 9, ncol = 4, byrow = TRUE)
omed
## The OMED is generated by executing the following lines of code:
## require(DoE.base)
## set.seed(123)
## omed <- data.matrix(oa.design(nl = c(3, 3, 3, 3)))

# Set the names of the attributes and attribute levels
attr.lev <- list(
  A = c("A1","A2","A3"), B = c("B1","B2","B3"),
  C = c("C1","C2","C3"), D = c("D1","D2","D3"))

# Convert the OMED into Case 2 BWS questions using three formats:
## Attribute column is located on the left-hand side
bws2.questionnaire(omed, attribute.levels = attr.lev,
  position = "left") 
## Attribute column is located in the center
bws2.questionnaire(omed, attribute.levels = attr.lev,
  position = "center")
## Attribute column is located on the right-hand side
bws2.questionnaire(omed, attribute.levels = attr.lev,
  position = "right") 

# Set respondent dataset containing 20 respondents who answered 
# nine BWS questions
resp.data <- data.frame(
id = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
B1 = c(2,2,2,1,2,4,2,2,2,2,1,2,2,4,2,3,2,3,2,2),
W1 = c(1,1,1,4,1,3,3,1,4,1,4,4,1,1,1,4,1,1,4,4),
B2 = c(1,1,2,1,1,3,1,1,1,1,2,1,1,2,1,3,1,3,1,1),
W2 = c(2,4,4,4,4,2,4,2,4,2,4,4,4,4,2,4,4,1,4,4),
B3 = c(1,1,2,1,2,1,1,1,1,2,1,1,1,2,1,1,1,1,3,1),
W3 = c(4,4,4,2,4,4,4,3,4,3,4,4,3,1,4,4,3,4,4,4),
B4 = c(1,2,2,1,2,1,2,2,2,1,2,4,2,2,2,4,2,2,1,2),
W4 = c(3,4,3,2,3,3,3,1,4,3,3,3,4,3,3,1,4,3,4,4),
B5 = c(1,2,2,1,2,1,2,1,3,1,1,1,3,1,1,1,3,1,1,1),
W5 = c(4,1,3,4,4,4,3,4,4,4,2,4,4,2,4,2,1,4,3,4),
B6 = c(2,4,2,1,2,1,4,3,1,1,1,1,3,2,1,2,3,4,1,4),
W6 = c(4,1,4,4,4,3,3,4,4,2,4,2,4,4,3,4,4,1,4,1),
B7 = c(3,3,2,3,4,1,2,3,3,3,2,1,3,2,1,2,3,1,3,2),
W7 = c(1,4,1,4,1,4,4,4,4,2,4,4,4,4,4,4,4,4,4,4),
B8 = c(1,1,2,1,2,2,1,1,1,2,1,2,1,1,1,3,1,1,1,1),
W8 = c(3,3,3,3,3,3,3,3,4,3,3,3,4,3,3,4,4,3,4,3),
B9 = c(3,3,3,1,3,1,1,3,1,1,1,1,3,1,1,1,3,1,1,1),
W9 = c(2,1,2,2,2,2,4,2,4,2,4,2,2,2,2,4,1,2,2,2))


# Create a dataset and conduct a conditional logit model analysis
## Set response variables
response.vars <- names(resp.data)[2:19]
## Set a base level in each attribute
base.lev <- list(
  A = c("A3"), B = c("B3"), C = c("C3"), D = c("D3"))
## Paired model with attribute and attribute-level variables
pr.data <- bws2.dataset(
  data = resp.data,
  id = "id",
  response = response.vars,  
  choice.sets = omed,        
  attribute.levels = attr.lev,
  attribute.variables = c("reverse"),
  effect = base.lev,
  type = "paired")
attributes(pr.data)$design.matrix
head(pr.data, 12)
### Attribute variable D is omitted from the model
pr <- clogit(RES ~ A + B + C + 
  A1 + A2 + B1 + B2 + C1 + C2 + D1 + D2 + strata(STR), 
  data = pr.data)
pr
### Calculate coefficients of base level variables
b.pr <- coef(pr)
-sum(b.pr[4:5]) # attribute level A3
-sum(b.pr[6:7]) # attribute level B3
-sum(b.pr[8:9]) # attribute level C3
-sum(b.pr[10:11]) # attribute level D3
## Marginal model with attribute and attribute-level variables
mr.data <- bws2.dataset(
  data = resp.data,
  id = "id",
  response = response.vars,
  choice.sets = omed,
  attribute.levels = attr.lev,
  attribute.variables = c("reverse"),
  effect = base.lev,
  delete.best = FALSE,
  type = "marginal")
attributes(mr.data)$design.matrix
head(mr.data, 8)
### Attribute variable D is omitted from the model
mr <- clogit(RES ~ A + B + C + 
  A1 + A2 + B1 + B2 + C1 + C2 + D1 + D2 + strata(STR), 
  data = mr.data)
mr
### Calculate coefficients of base level variables
b.mr <- coef(mr)
-sum(b.mr[4:5]) # attribute level A3
-sum(b.mr[6:7]) # attribute level B3
-sum(b.mr[8:9]) # attribute level C3
-sum(b.mr[10:11]) # attribute level D3

# Calculate BWS scores
bwscores <- bws2.count(mr.data)
sum(bwscores, "level")
barplot(bwscores, "bw", "level")
}


\keyword{manip}

