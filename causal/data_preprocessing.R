library(base)
library(extraDistr)
library(BBmisc)

#process covariates and outcomes for regularized working model
#normalization = TRUE applies standard normalization

data_preprocessing <- function(treatment_status, pretreatment_covariates, outcome, epsilon, normalization){
  stopifnot(is.logical(normalization))
  sample_size <- length(treatment_status)
  
  if (normalization){ 
    pretreatment_covariates <- normalize(pretreatment_covariates, method = "standardize", margin = 1)
    outcome <- normalize(outcome, method = "standardize", margin = 1)
  }
  
  sign <- rsign(sample_size)
  perturbed_treatment_status <- treatment_status + epsilon * rcauchy(sample_size, location = 0, scale = 1)
  covariates <- rbind(rep(1, sample_size), perturbed_treatment_status, pretreatment_covariates) %*% diag(sign)
  outcome <- diag(sign) %*% outcome
  
  rownames(covariates) <- NULL
  covariate_norms <- sqrt(diag(t(covariates) %*% covariates))

  return(list(covariates = t(t(covariates)/covariate_norms), outcome = outcome/covariate_norms))
}