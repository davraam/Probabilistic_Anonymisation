#' 
#' @title probAnon (Probabilistic Anonymisation)
#'
#' @description The function applies probabilistic anonymisation to an input dataset
#' 
#' @details The function applies a method that adds random noise to the variables of a given dataset.
#' The method has been developed to add noise to some or all variables in a released pseudonymised dataset
#' where the values of identifying variables for individuals of interest are also available to an external
#' ‘attacker’ who wishes to identify those individuals so that they can interrogate their records in the
#' dataset. To avoid such identification by an ‘attacker’ who wishes to use the linking of patterns based
#' on the values of such variables, enough noise needs to be generated and added to these identifying
#' variables. The noise can then be ‘removed’ at the analysis stage since its characteristics are known.
#' The function first splits the input dataframe to two sub-dataframes, one for the continuous and one for
#' the categorical variables. Then, random noise is added to continuous and categorical variables separately. 
#' The random noise added to continuous variables follows a normal distribution with zero mean and variance 
#' equal to a proportion of the variance of each true continuous variable. Each proportion is specified by
#' the user in the argument \code{weights}. The random noise added to categorical variables follows a normal
#' distribution with zero mean and variance equal to a value set in the argument \code{var.categorical}.
#' In that way, the categorical variables with the addition of noise are converted to continuous variables.
#' Thus, for 0-1 binary variables, we then truncate any negative values to 0 and any values greater than 1
#' to 1. Finally the function returns the dataframe of noisy data to the user.
#'
#' @param dataframe, is the dataframe that includes the variables of which random noise will be added.
#' The dataframe can include continuous and categorical variables. At the present version of the function,
#' the anonymization is applied to continuous or binary 0-1 variables. The user is therefore required to
#' convert any categorical variables to binary 0-1 data, as a convention, before he/she use the probAnon 
#' function. In a later version of the function, we will allow the addition of noise to multi-categorical
#' variables.
#'
#' @param seed, allows the user to set a certain random number generator. If this argument is not specified,
#' the function bases the seed parameter on the local time (as determined by the computer’s internal clock).
#'
#' @param weights, is a vector $(w_1,…,w_s)$ that specifies the proportion of the variance of original
#' variables that will be used as the variance of the added noise. The weights are used only for the 
#' continuous variables of the input dataframe so the user should be sure that the length of \code{weights}
#' vector is equal to $s$ which is the number of the continuous variables in the input dataframe.
#' If the user does not input any vector for the weights then each weight is fixed to 0.1 by default
#' for each continuous variable which means that the variance of the added noise is equal to the 10% of
#' the variance of the original variable.
#'
#' @param var.categorical, is the variance of noise added to the categorical data. If this argument is not 
#' specified, the function adds noise of variance 0.5 (by default) to any binary variables.  
#'
#' @return The function returns a dataframe with the anonymised data.
#' 
#' @author Avraam D.
#' @export
#'
probAnon <- function(dataframe, seed=NULL, weights=NULL, var.categorical=0.5){

# generate a set of random numbers. If the user does not specify the seed for random number
# generator, then the seed is fixed to the momentum system's clock time converted to a numerical value  
if (is.null(seed)){
  Seed <- as.numeric(Sys.time())
}else{
  Seed <- seed
}
set.seed(Seed)

# the number of individuals (number of rows in the dataframe)
n <- dim(dataframe)[1]

# the number of variables (number of columns in the dataframe)
k <- dim(dataframe)[2]

# find the class of each variable in the dataset
class <- c()
for (i in 1:k){
  class[i] <- class(dataframe[,i])
}

# find which columns of the dataframe include continuous (numerical) and which categorical (categorical) variables
indx.of.continuous <- which(class=="numeric")
indx.of.categorical <- c(which(class=="factor"), which(class=="integer"))

#################################################################################################################
#
#  Add noise to the continuous variables
#
#################################################################################################################

if(length(indx.of.continuous)>0){

  continuous.vars <- dataframe[c(indx.of.continuous)]

  # calculate the variances and standard deviations of the original continuous variables
  cont.variances <- c()
  cont.sd <- c()
  for (i in 1:length(indx.of.continuous)){
    cont.variances[i] <- var(continuous.vars[,i])
    cont.sd[i] <- sd(continuous.vars[,i])
  }

  # the variance of the noise is equal to a weighted variance of the true variable.
  # if the user does not specify the weight of the variance for each continuous
  # variable then the weight is fixed by default to 0.1 for all continuous variables.  
  if (is.null(weights)){
    weights.vector <- rep(0.1, length(indx.of.continuous))  
  }
  if (!(is.null(weights))){
    if (length(weights)==length(indx.of.continuous)){
      weights.vector <- weights
    }else{
      stop("The length of the vector for the weights is not the same as the number of continuous variables", call.=FALSE)
    }
  }

  # calculate the variance of the noise 
  noise.variance <- weights.vector * cont.variances

  # Add noise to the continuous variables
  continuous.with.noise <- replace(continuous.vars, values=0)
  for (i in 1:length(indx.of.continuous)){
    continuous.with.noise[,i] <- continuous.vars[,i] + rnorm(n, mean=0, sd=sqrt(noise.variance[i]))
  }

}

#################################################################################################################
#
#  Add noise to the categorical variables
#
#################################################################################################################

if(length(indx.of.categorical)>0){

  categorical.vars <- dataframe[c(indx.of.categorical)]

  # Add noise to the categorical variables
  categorical.with.noise <- replace(categorical.vars, values=0)
  for (i in 1:length(indx.of.categorical)){
    categorical.with.noise[,i] <- categorical.vars[,i] + rnorm(n, mean=0, sd=sqrt(var.categorical))
  }

  # Truncate any negative values to 0 and any values greater than 1 to 1
  categorical.with.noise[categorical.with.noise<0]=0
  categorical.with.noise[categorical.with.noise>1]=1

}

#################################################################################################################
#
#  Combine continuous and categorical variables 
#
#################################################################################################################

if(length(indx.of.continuous)>0 & length(indx.of.categorical)>0){
  true.data <- as.data.frame(cbind(continuous.vars, categorical.vars)) 
  setNames(true.data, c(colnames(continuous.vars), colnames(categorical.vars)))
  noisy.data <- as.data.frame(cbind(continuous.with.noise, categorical.with.noise)) 
  setNames(noisy.data, c(colnames(continuous.vars), colnames(categorical.vars)))
}

if(length(indx.of.continuous)==0 & length(indx.of.categorical)>0){
  true.data <- as.data.frame(categorical.vars) 
  setNames(true.data, colnames(categorical.vars))
  noisy.data <- as.data.frame(categorical.with.noise) 
  setNames(noisy.data, colnames(categorical.vars))
}

#################################################################################################################

# re-order the columns in the output dataframe to have the same order as the columns of the input dataframe
noisy.data <- noisy.data[c(names(dataframe))]

# Return the anonymised data
return(noisy.data=noisy.data)

}
