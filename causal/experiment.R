rm(list=ls())

source('main_function.R')
source('data_preprocessing.R')
library(speff2trial)

data("ACTG175")
data <- ACTG175  

#data preprocessing for regularized working model
#data_arms_1: processed data of zidovudine and didanosine
#data_arms_2: processed data of zidovudine and zalcitabine 
#data_arms_3: processed data of didanosine

for (treatment in 1:3){
  control_index <- which(data$arms == 0)
  treatment_index <- which(data$arms == treatment)
  index <- c(control_index, treatment_index)
  treatment_status <- c(rep(0, length(control_index)), rep(1, length(treatment_index)))
  cd40 <- c(data$cd40[control_index], data$cd40[treatment_index])
  preanti <- c(data$preanti[control_index], data$preanti[treatment_index])
  pretreatment_covariates <- rbind(cd40, preanti)
  outcome <- c(data$cd420[control_index], data$cd420[treatment_index]) 
  processed_data <- data_preprocessing(treatment_status, pretreatment_covariates, outcome, 0.005, TRUE)
  save(processed_data, file = paste0("data_arms_", treatment))
  assign(paste0("data_arms_", treatment), processed_data)
}

#main experiment with Algorithm 2
iteration <- 100

for (treatment in 1:3){
  data <- get(paste0("data_arms_", treatment))
  covariates <- data$covariates
  outcome <- data$outcome
  NN_parameter <- ceiling(length(outcome)^(dim(covariates)[1] / (2 * dim(covariates)[1] - 1)))
  treatment_coefficient_samples <- rep(0, NN_parameter * iteration)
  for (iter in 1:iteration){
    coefficient_estimation <- approximate_block_coordinate_descent(covariates, outcome, 10, 1000, 20)
    treatment_coefficient_samples[(NN_parameter * (iter - 1) + 1) : (NN_parameter * iter)] <- coefficient_estimation[2, ]
    if (iter %% 10 == 0){
      print(iter)
      }
  }
  save(treatment_coefficient_samples, file = paste0("treatment_coefficient_samples_", treatment))
}
