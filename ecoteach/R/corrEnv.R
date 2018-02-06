#' Generate random data with a given correlation structure
#'
#' @description This function generates random data with a given correlation structure. In contrast to other
#' functions, the aim is to establish variables that are associated with different gradients as for
#' example environmental variables in ecological analyses.
#'
#' @author Ralf Schaefer
#'
#' @param n Number of observations.
#' @param nvar Number of environmental variables.
#' @param ngrad Number of gradients, each variable will be randomly assigned to one gradient.
#' @param rho_corr Correlation coefficient for correlation between gradient and associated variables, -1 <= rho <= 1, but typically close to 1 or -1 if following the design idea of the function to set the number of gradients with ngrad.
#' @param rho_non_corr Correlation coefficient for correlation between variables associated with different gradients, -1 <= rho <= 1, but typically around 0 if the aim is to have independent gradients.
#' @param mu A vector of (different) means of length nvars or a single value that is used for all variables.
#' @param vnames Variable names. Character vector of length nvar. If no string is provided, the default names will be V#, where #
#' represents the column.
#' @details The variance of all variables in the variance-covariance matrix has been set to 1. To simulate variables with different variances, the correlation matrix is returned and can be edited. The relation between standard deviation and mean can be set through the size of the mean.
#' @return A list with the components: 1) correlation matrix with nvar rows and columns 2) data set with n rows and nvar columns.
#' @examples
#' nvar <- 15
#' mu <- sample(seq(0.01, 0.5, length.out = 50), nvar, replace = TRUE)
#'
#' data_sim <- genGradData(1000, nvar, 5, mu, 0.9, 0)
#' cor(data_sim[[2]])
#' pairs(data_sim[[2]])
#'
#' @export


corrEnv <- function(n, nvar, ngrad, mu, rho, rho.non.corr, vnames = NULL) {

 	if (is.numeric(c(n, nvar, ngrad, mu, rho, rho.non.corr)) == FALSE ) {
      	stop("Non-numeric input provided to ")
      	}

 	if (nvar < ngrad) {
      	stop("ngrad must be equal or greater than nvar")
      	}

 	if (abs(rho) > 1) {
     	stop("Values > 1 and < -1 not meaningful for rho")
     	}

	if (abs(rho.non.corr) > 1) {
     	stop("Values > 1 and < -1 not meaningful for rho.non.corr")
     	}

	# create matrix
	cor_mat <- matrix(NA, nrow = nvar, ncol = nvar)

	# assign each var to a group
	ind_vec <- sample(1:ngrad, size = nvar, replace = TRUE)

	# assign correlation coefficients to
	for(i in 1:nvar) {

		for(j in 1:nvar)
 		if(ind_vec[i] == ind_vec[j])
 	 	cor_mat[i, j] <- rho
  		else cor_mat[i, j] <- rho.non.corr
 		}

	# set diagonal to 1
	diag(cor_mat) <- 1

	# draw random data
	data <- MASS::mvrnorm(n = n, mu = mu, Sigma = cor_mat)

	if (length(vnames)) {

   	 	if (length(vnames) != nvar) {
      	stop("Invalid number of variable names")
    	}

	nnames <- trimws(unlist(strsplit(vnames, split = ",")))
    setnames(data, nnames)
  	}

  	final <- list(cor_mat, data)
  	return(final)
 }
