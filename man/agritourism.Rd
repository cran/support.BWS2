\name{agritourism}
\alias{agritourism}
\docType{data}
\title{
Potential tourists' valuation of agritourism
}
\description{
This dataset contains responses to Case 2 BWS questions. Respondents were asked to evaluate agritourism packages provided by dairy farms in Hokkaido, Japan.
}
\usage{
data(agritourism)
}
\format{
A data frame with 240 respondents on the following 21 variables.
  \describe{
    \item{\code{id}}{Identification number of respondents.}
    \item{\code{b1}}{Item selected as the best in question 1.}
    \item{\code{w1}}{Item selected as the worst in question 1.}
    \item{\code{b2}}{Item selected as the best in question 2.}
    \item{\code{w2}}{Item selected as the worst in question 2.}
    \item{\code{b3}}{Item selected as the best in question 3.}
    \item{\code{w3}}{Item selected as the worst in question 3.}
    \item{\code{b4}}{Item selected as the best in question 4.}
    \item{\code{w4}}{Item selected as the worst in question 4.}
    \item{\code{b5}}{Item selected as the best in question 5.}
    \item{\code{w5}}{Item selected as the worst in question 5.}
    \item{\code{b6}}{Item selected as the best in question 6.}
    \item{\code{w6}}{Item selected as the worst in question 6.}
    \item{\code{b7}}{Item selected as the best in question 7.}
    \item{\code{w7}}{Item selected as the worst in question 7.}
    \item{\code{b8}}{Item selected as the best in question 8.}
    \item{\code{w8}}{Item selected as the worst in question 8.}
    \item{\code{b9}}{Item selected as the best in question 9.}
    \item{\code{w9}}{Item selected as the worst in question 9.}
    \item{\code{gender}}{Respondents' gender: 1 = male; 2 = female.}
    \item{\code{age}}{Respondents' age: 2 = 20s; 3 = 30s; 4 = 40s; 5 = 50s}
  }
See the section Examples for details.
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link[support.BWS2]{support.BWS2-package}}, \code{\link[support.BWS2]{bws2.dataset}}, \code{\link[DoE.base]{oa.design}}
}
\examples{
# Agritourism refers to various activities offered by farms and ranches
# to visitors, such as hands-on farm work or outdoor recreation.
#
# In the Case 2 BWS questions, respondents were asked to evaluate 
# agritourism packages provided by dairy farms (ranches) in Hokkaido, Japan. 
# We assumed that the agritourism package consists of the following four
# types of activities, each with three activity items:
#  1. Hands-on ranch chores
#    (1) Milking a cow
#    (2) Feeding a cow
#    (3) Nursing a calf
#  2. Hands-on food processing
#    (1) Butter making
#    (2) Ice-cream making
#    (3) Creamy caramel making
#  3. Hands-on craft making
#    (1) Making a product from wool
#    (2) Making a product from wood
#    (3) Making a product from pressed flowers
#  4. Outdoor activities
#    (1) Horse riding
#    (2) Tractor riding
#    (3) Walking with cows
#
# As there are four activities and each activity has three items, 
# a total of nine BWS questions were created using a three-level OMED
# with four columns. Each BWS question asked respondents to select
# the most and least interesting of the four activities shown 
# in the question.
#
# In the following, we assume that the paired and marginal models with
# both attribute and attribute-level variables (Flynn et al. 2007; 2008)
# are fitted to the responses using the conditional logit model, 
# with clogit() in the survival package.

# Load the package needed for the example:
require(survival)

options(digits = 4)

# The following OMED is generated using oa.design() in the DoE.base package:
# require(DoE.base)
# des <- data.matrix(
#    oa.design(nl = c(3,3,3,3), randomize = FALSE))
des <- cbind(
  c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  c(1, 3, 2, 3, 2, 1, 2, 1, 3),
  c(1, 2, 3, 3, 1, 2, 2, 3, 1))

# The names of the attributes (activities) and attribute levels 
# (activity items) were stored in the list attr.lev:
attr.lev <- list(
   chore = c("milking", "feeding", "nursing"),
   food = c("butter", "ice", "caramel"),
   craft = c("wool", "wood", "flower"),
   outdoor =c("horse", "tractor", "cow"))

# A series of Case 2 BWS questions were converted from the OMED using 
# bws2.questionnaire():
bws2.questionnaire(choice.sets = des, attribute.levels = attr.lev,
  position = "left")

# The responses to the questions were stored in the dataset agritourism
# in the support.BWS2 package:
data(agritourism)
dim(agritourism)
colnames(agritourism)

# The names of the response variables used in the dataset agritourism
# were stored in the vector response.vars:
response.vars <- colnames(agritourism)[2:19]
response.vars

# The base level in each attribute was stored in the object base.lev
# in list format:
base.lev <- list(
  chore = c("nursing"),
  food = c("caramel"),
  craft = c("flower"),
  outdoor =c("cow"))

# The datasets for the paired model and the marginal model were created
# using bws2.dataset() and then stored in the objects pr.data1 and mr.data1,
# respectively:
pr.data1 <- bws2.dataset(
   data = agritourism,
   id = "id",
   response = response.vars,  
   choice.sets = des,        
   attribute.levels = attr.lev,
   attribute.variables = c("reverse"),
   effect = base.lev,
   type = "paired") 
mr.data1 <- bws2.dataset(
   data = agritourism,
   id = "id",
   response = response.vars,
   choice.sets = des,
   attribute.levels = attr.lev,
   attribute.variables = c("reverse"),
   effect = base.lev,
   delete.best = FALSE,
   type = "marginal")
dim(pr.data1)
names(pr.data1)
dim(mr.data1)
names(mr.data1)

# The BWS scores were calculated using bws2.count() with the dataset for
# the marginal model:
scores <- bws2.count(mr.data1)
dim(scores)
names(scores)

# The scores for each level were aggregated among all respondents using
# sum() and bar plots of the scores were drawn using barplot():
sum(scores, "level")
barplot(scores, "bw", "level")

# If we only need aggregated B and W scores, these can be calculated from
# the dataset for a paired model as follows:
apply(pr.data1[pr.data1$RES == 1, c("BEST.LV", "WORST.LV")], 2, table)

# BW scores can be calculated according to groups of respondents. 
# For example, the scores for male and those for female are given as follows:
sum(scores[agritourism$gender == 1, ], "level")
sum(scores[agritourism$gender == 2, ], "level")

# Bar plots for respondents in their 20s and those in their 50s can also be
# drawn using the following lines of code:
barplot(scores[agritourism$age == 2, ], "bw", "level")
barplot(scores[agritourism$age == 5, ], "bw", "level")

# We fitted the conditional logit model to the Case 2 BWS responses 
# on the basis of the paired and marginal models with both attribute
# and attribute-level variables. The systematic component of the utility
# function for the example is
#    v = b1 chore + b2 food + b3 outdoor + 
#        b4 milking + b5 feeding + b6 butter + b7 ice +
#        b8 wool + b9 wood + b10 horse + b11 tractor
# where chore, food, and outdoor are attribute variables (craft has been
# omitted); and milking, feeding, butter, ice, wool, wood, horse, and
# tractor are attribute-level variables (nursing has been omitted for chore,
# caramel has been omitted for food, flower has been omitted for craft,
# and cow has been omitted for outdoor); bs are coefficients to be estimated.
#
# The model formula for clogit(), corresponding to the systematic component
# mentioned above, is described as:
mf <- RES ~ chore + food + outdoor + 
            milking + feeding + butter + ice + 
            wool + wood + horse + tractor +
            strata(STR)

# We fitted the paired model using clogit() with the dataset pr.data1:
pr.out <- clogit(formula = mf, data = pr.data1)
pr.out

# The attribute-level variables are effect-coded ones, and thus the 
# coefficient of the base level in each attribute can be calculated using:
b <- coef(pr.out)
(nursing <- -sum(b[4:5]))
names(nursing) <- "nursing"
(caramel <- -sum(b[6:7]))
names(caramel) <- "caramel"
(flower <- -sum(b[8:9]))
names(flower) <- "flower"
(cow <- -sum(b[10:11]))
names(cow) <- "cow"
craft <- 0
names(craft) <- "craft"
paired.model <- c(b[1:2], craft, b[3], b[4:5], nursing, b[6:7],
   caramel, b[8:9], flower, b[10:11], cow)
barplot(paired.model)

# The following code is for the marginal model: 
mr.out <- clogit(formula = mf, data = mr.data1)
mr.out
b <- coef(mr.out)
(nursing <- -sum(b[4:5]))
names(nursing) <- "nursing"
(caramel <- -sum(b[6:7]))
names(caramel) <- "caramel"
(flower <- -sum(b[8:9]))
names(flower) <- "flower"
(cow <- -sum(b[10:11]))
names(cow) <- "cow"
marginal.model <- c(b[1:2], craft, b[3], b[4:5], nursing, b[6:7],
   caramel, b[8:9], flower, b[10:11], cow)
barplot(marginal.model)

# As mentioned in Flynn et al. (2008), the results from the paired model
# are similar to those from the marginal model: the correlation coefficient
# for the two results is calculated as follows:
cor(marginal.model, paired.model)
plot(marginal.model, paired.model, 
   xlim = c(-0.5, 1), ylim = c(-0.5, 1))
}
\keyword{datasets}
