% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corrEnv.R
\name{corrEnv}
\alias{corrEnv}
\title{Generate random data with a given correlation structure}
\usage{
corrEnv(n, nvar = 10, ngrad = 3, mu = seq(0.01, 0.5, length.out = 10),
  rho = 0.9, rho.non.corr = 0, vnames = NULL)
}
\arguments{
\item{n}{Number of observations.}

\item{nvar}{Number of environmental variables.}

\item{ngrad}{Number of gradients, each variable will be randomly assigned to one gradient.}

\item{mu}{A vector of (different) means of length nvars or a single value that is used for all variables.}

\item{vnames}{Variable names. Character vector of length nvar. If no string is provided, the default names will be V#, where #
represents the column.}

\item{rho_corr}{Correlation coefficient for correlation between gradient and associated variables, -1 <= rho <= 1, but typically close to 1 or -1 if following the design idea of the function to set the number of gradients with ngrad.}

\item{rho_non_corr}{Correlation coefficient for correlation between variables associated with different gradients, -1 <= rho <= 1, but typically around 0 if the aim is to have independent gradients.}
}
\value{
A list with the components: 1) correlation matrix with nvar rows and columns 2) data set with n rows and nvar columns.
}
\description{
This function generates random data with a given correlation structure. In contrast to other
functions, the aim is to establish variables that are associated with different gradients as for
example environmental variables in ecological analyses.
}
\details{
The variance of all variables in the variance-covariance matrix has been set to 1. To simulate variables with different variances, the correlation matrix is returned and can be edited. The relation between standard deviation and mean can be set through the size of the mean.
}
\examples{
nvar <- 15
mu <- sample(seq(0.01, 0.5, length.out = 50), nvar, replace = TRUE)

data_sim <- corrEnv(1000, nvar, 5, mu, 0.9, 0)
cor(data_sim[[2]])
pairs(data_sim[[2]])

}
\author{
Ralf Schaefer
}
