% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_mfa.R
\name{tidy_mfa}
\alias{tidy_mfa}
\title{Tidy MFA individual results}
\usage{
tidy_mfa(mfa_res, type = c("all", "quanti", "quali", "category")[1])
}
\arguments{
\item{mfa_res}{object of class MFA}

\item{type}{which data class to tidy (default all)}
}
\value{
Tidy data frame for MFA results
}
\description{
Takes an MFA objects and tidy the data into a much easier working format for evaluation
}
\examples{
library(FactoMineR)

# Quanti variables only
data(wine)
mfa_res <- MFA(wine[,c(1, 2)], group=c(1,1), type=c(rep("n",2)),
               ncp=5, name.group=c("olf","vis"))

tidy_mfa(mfa_res)

# Quali variables only
mfa_res <- MFA(iris[,-5], group=c(2,2), type=c(rep("s",2)),
               ncp=5, name.group=c("sepal", "petal"))

tidy_mfa(mfa_res)

# Quanti & quali variables
data(wine)
mfa_res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
               ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
               num.group.sup=c(1,6))

tidy_mfa(mfa_res)

data (poison)
mfa_res <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
               name.group=c("desc","desc2","symptom","eat"),
               num.group.sup=1:2)

tidy_mfa(mfa_res)
}
