\name{support.BWS2-package}
\alias{support.BWS2-package}
\alias{support.BWS2}

\docType{package}

\title{
Basic functions for supporting an implementation of Case 2 best--worst scaling
}

\description{
The package has three basic functions that support an implementation of 
Case 2 (profile case) best--worst scaling. The first is to convert 
an orthogonal main-effect design into questions, the second is to create 
a dataset suitable for analysis, and the third is to calculate 
count-based scores.
}

\details{
The package is under development and thus may be changed substantially in 
the future.

1) Outline of Case 2 best--worst scaling

Case 2 (profile case) best--worst scaling (BWS) is a question-based 
survey method to elicit preferences for attribute levels 
(See Flynn 2010, Flynn et al. 2007 and 2008, Louviere et al. 2015, and 
Marley et al. 2008 for details of the subsection). A profile (choice set) 
has three or more attributes and each attribute has two or more levels. 
The profile is expressed as a combination of attribute levels. 
Numerous profiles are constructed using experimental designs. 
Attributes shown in each profile are fixed in all the profiles and 
a combination of attribute levels in each profile is changed according 
to the profiles. A profile selected from all the constructed profiles 
is presented to respondents, who are then asked to choose the best and 
worst attribute levels in the profile. This question is repeated until 
all profiles are evaluated. Analyzing the responses enables us to 
elicit preferences for the attribute levels.

A basic approach to constructing profiles is using an orthogonal 
main-effect design (OMED). Assume that the profiles have \eqn{K} 
attributes and each attribute has \eqn{L_{k}} levels. If all the attributes 
have the same number of levels, \eqn{L}, a \eqn{L^{K}} OMED is used to 
construct the profiles. Columns of the OMED correspond to attributes, 
while the rows to profiles. For example, profiles have four attributes 
and they have three levels: attribute A with levels A1, A2, and A3; 
attribute B with levels B1, B2, and B3; attribute C with levels C1, C2, 
and C3; and attribute D with levels D1, D2, and D3. A \eqn{3^{4}} 
OMED corresponding to the assumptions is as follows (see the section 
Examples of the function \code{bws2.dataset()} for code to generate 
the OMED):

\tabular{cccc}{
1 \tab 3 \tab 2 \tab 3 \cr
3 \tab 1 \tab 2 \tab 2 \cr
3 \tab 3 \tab 3 \tab 1 \cr
2 \tab 3 \tab 1 \tab 2 \cr
2 \tab 2 \tab 2 \tab 1 \cr
1 \tab 1 \tab 1 \tab 1 \cr
1 \tab 2 \tab 3 \tab 2 \cr
3 \tab 2 \tab 1 \tab 3 \cr
2 \tab 1 \tab 3 \tab 3 \cr
}

Suppose that attributes A, B, C, and D are assigned to the first, second, 
third, and fourth column of the OMED, respectively, and the values 1, 2, 
and 3 used in the OMED correspond to the attribute-level values in each 
attribute: 1 = A1, 2 = A2, and 3 = A3 for attribute A; 1 = B1, 2 = B2, 
and 3 = B3 for attribute B; 1 = C1, 2 = C2, and 3 = C3 for attribute C; 
and 1 = D1, 2 = D2, and 3 = D3 for attribute D. Accordingly, 
the above-mentioned OMED can be transformed into the following:

\tabular{cccc}{
A1 \tab B3 \tab C2 \tab D3 \cr
A3 \tab B1 \tab C2 \tab D2 \cr
A3 \tab B3 \tab C3 \tab D1 \cr
A2 \tab B3 \tab C1 \tab D2 \cr
A2 \tab B2 \tab C2 \tab D1 \cr
A1 \tab B1 \tab C1 \tab D1 \cr
A1 \tab B2 \tab C3 \tab D2 \cr
A3 \tab B2 \tab C1 \tab D3 \cr
A2 \tab B1 \tab C3 \tab D3}

The resultant OMED consists of nine rows: nine profiles, that is, nine 
Case 2 BWS questions, are constructed. For example, a profile 
corresponding to the first row of the OMED comprises A1, B3, C2, and D3. 
This means that respondents who face the question created from 
the first row of the OMED are asked to select their best and worst 
attribute levels from attribute levels A1, B3, C2, and D3, as follows:

\tabular{c}{
Please select your best and worst attribute levels from the following four:}
\tabular{ccc}{
Best \tab Attribute \tab Worst \cr
[_] \tab A1 \tab [_] \cr
[_] \tab B3 \tab [_] \cr
[_] \tab C2 \tab [_] \cr
[_] \tab D3 \tab [_] \cr
}

There are two approaches for analyzing responses to Case 2 BWS questions: 
a counting approach and modeling approach. The counting approach 
calculates scores on the basis of the number of times  attribute level 
\eqn{i} is selected as the best (\eqn{B_{in}}: B score for attribute 
level \eqn{i}) and the worst (\eqn{W_{in}}: W score for attribute 
\eqn{i}) among all the questions for respondent \eqn{n}. 
A (disaggregated) best-minus-worst (BW) score and its standardized 
variant are defined as 
\deqn{
BW_{in} = B_{in} - W_{in}, 
}
\deqn{
std.BW_{in} = \frac{BW_{in}}{f_{i}},
} 
where \eqn{f_{i}} is the frequency with which attribute level \eqn{i} 
appears across all questions.

The modeling approach uses discrete choice models to analyze responses. 
When using the modeling approach, a model type must be selected 
according to the assumption for respondents' choice behavior in 
Case 2 BWS questions and then a dataset must be formatted as per 
the selected model. There are three standard models: paired, marginal, 
and marginal sequential models. Although the three models commonly 
assume that the respondents derive utility for each attribute level 
shown in the profile, the assumption for how they select 
the best and worst attribute levels from the set of attribute levels 
in the profile differs among the three models.

The number of possible pairs in which attribute level \eqn{i} is selected 
as the best and attribute level \eqn{j} is selected as 
the worst (\eqn{i \neq j}) from \eqn{K} attribute levels is 
\eqn{K \times (K - 1)}. The paired model assumes that respondents select 
attribute level \eqn{i} as the best and attribute level \eqn{j} as 
the worst because the difference in utility between \eqn{i} and 
\eqn{j} represents the greatest utility difference 
among \eqn{K \times (K - 1)} utility differences. 
Consider the example profile mentioned above. It contains four 
attribute levels: A1, B3, C2, and D3. The number of possible pairs 
is \eqn{12} \eqn{(= 4 \times (4 - 1))}. There are 12 possible pairs 
of the best and worst attribute levels (in each pair, 
the former is the best and the latter is the worst): (A1, B3), (A1, C2), 
(A1, D3), (B3, A1), (B3, C2), (B3, D3), (C2, A1), (C2, B3), (C2, D3), 
(D3, A1), (D3, B3), and (D3, C2). 
If a respondent selects A1 as the best attribute level and C2 as 
the worst, the paired model assumes that the respondent calculates 12 
utility differences as per the 12 above-mentioned pairs and that 
the difference in utility between A1 and C2 is the maximum among 
the 12 utility differences.

The marginal model assumes that there are \eqn{K} possible best 
attribute levels and \eqn{K} possible worst attribute levels 
in a profile, that attribute level \eqn{i} is selected as 
the best from \eqn{K} possible best attribute levels in the profile, 
and that attribute level \eqn{j} is selected as the worst 
from \eqn{K} possible worst attribute levels. This is because the 
utility for attribute level \eqn{i} is the maximum among 
the utilities for \eqn{K} attribute levels and that for attribute level 
\eqn{j} is the minimum. Following the above example, the marginal model 
assumes that there are four possible best attribute levels and four 
possible worst attribute levels in the profile and interprets 
the respondent's choice behavior as follows: utility for A1 is 
the maximum among the four utilities for A1, B3, C2, and D3 and 
that for C2 is the minimum among the four.

The assumption of the marginal model that the worst attribute level is 
selected from \eqn{K} attribute levels would not be appropriate 
because the best attribute level in a profile must differ from 
the worst one in the profile. Thus, the marginal sequential model 
assumes that respondents select attribute level \eqn{i} as the best 
from \eqn{K} attribute levels in the profile and then attribute level 
\eqn{j} as the worst from the remaining \eqn{K - 1} attribute levels. 
Following the above example, under the marginal sequential 
model assumption, there are four possible best attribute levels and 
three possible worst attribute levels in the profile. 
The model considers that the respondent selects A1 as the best from 
the four possible attribute levels because the utility for A1 is 
the highest among the utilities for A1, B3, C2, and D3, but selects C2 
as the worst from three possible worst levels, B3, C2, and D3, 
because the utility for C2 is the least among the three.
 
The three models generally assume that the utility for attribute level 
\eqn{i} selected as the worst is the negative of the one selected as 
the best. Under these assumptions, and given the assumption for 
the stochastic component of utility, the probability of selecting 
attribute level \eqn{i} as the best and attribute level \eqn{j} as 
the worst can be expressed as a conditional logit model.

2) Role of the package and other packages needed to complete implementing 
Case 2 BWS

The package \pkg{support.BWS2} provides functions to convert an OMED 
into a series of Case 2 BWS questions, create a dataset for the analysis 
from the OMED and the responses to the questions, and calculate BWS scores. 
Other packages are needed to complete implementing Case 2 BWS with R: 
a package to construct OMEDs and another to analyze the responses 
on the basis of the modeling approach. For example, 
the \code{oa.design()} function in \pkg{DoE.base} (Groemping 2015) can 
construct OMEDs, while the functions \code{clogit()} in \pkg{survival} 
(Therneau 2016), \code{mlogit()} in \pkg{mlogit} (Croissant 2013), 
and \code{gmnl()} in \pkg{gmnl} (Sarrias and Daziano 2015) can fit 
the conditional logit model. The latter two functions are also used 
to fit advanced discrete choice models 
such as a mixed (random parameters) logit model. Refer to the task views
 about experimental designs (Groemping 2016) and econometrics (Zeileis 2017) 
on CRAN for details on packages for experimental designs and 
discrete choice models in R.

\bold{Acknowledgments}

I would like to thank Professor Kazuo Sato for his kind support. 
This work was supported by JSPS KAKENHI Grant Numbers 25450341 and 16K07886.
}


\author{
Hideo Aizaki
}

\references{
Flynn TN (2010) Valuing citizen and patient preferences in health: 
recent developments in three types of best-worst scaling. 
\emph{Expert Review of Pharmacoeconomics & Outcomes Research}, 
\bold{10}(3), 259--267.

Flynn TN, Louviere JJ, Peters TJ, Coast J (2007) Best-Worst Scaling: 
What it can do for health care research and how to do it. 
\emph{Journal of Health Economics}, \bold{26}, 171--189.

Flynn TN, Louviere JJ, Peters TJ, Coast J (2008) Estimating 
preferences for a dermatology consultation using best-worst scaling: 
Comparison of various methods of analysis. 
\emph{BMC Medical Research Methodology}, \bold{8}(76). 
doi: 10.1186/1471-2288-8-76.

Croissant Y (2013) \emph{mlogit: multinomial logit model}. 
R package version 0.2-4. \url{https://CRAN.R-project.org/package=mlogit}.

Groemping U (2015) \emph{DoE.base: Full Factorials, Orthogonal Arrays and 
Base Utilities for DoE Packages}. R package version 0.27-1. 
\url{https://CRAN.R-project.org/package=DoE.base}.

Groemping U (2016) \emph{CRAN Task View: Design of Experiments (DoE) &
 Analysis of Experimental Data}. 
\url{https://CRAN.R-project.org/view=ExperimentalDesign}.

Hensher DA, Rose JM, Greene WH (2015) \emph{Applied Choice Analysis}. 
2nd edition. Cambridge University Press.

Louviere JJ, Flynn TN, Marley AAJ (2015) \emph{Best-Worst Scaling: 
Theory, Methods and Applications}. Cambridge University Press.

Marley AAJ, Flynn TN, Louviere JJ (2008) Probabilistic models of 
set-dependent and attribute-level best-worst choice. 
\emph{Journal of Mathematical Psychology}, \bold{52}, 281--296.

Sarrias M, and Daziano R (2015) \emph{gmnl: Multinomial Logit Models 
with Random Parameters}. R package version 1.1-1. 
\url{https://CRAN.R-project.org/package=gmnl}.

Therneau T (2015) \emph{A Package for Survival Analysis in S}. 
Version 2.38, \url{https://CRAN.R-project.org/package=survival}.

Zeileis A (2017) \emph{CRAN Task View: Econometrics}. 
\url{https://CRAN.R-project.org/view=Econometrics}.
}


\keyword{ package }

