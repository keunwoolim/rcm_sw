library(base)
library(MASS)

#generate unit vector samples with given dimension and sample size
unit_vector_sample_generation <- function(dimension, unit_sample_size){
  samples <- mvrnorm(unit_sample_size, mu = rep(0, dimension), Sigma = diag(dimension))
  samples <- samples / sqrt(diag(samples %*% t(samples)))
  return (t(samples))
}

#computation of projected NN samples from covariates and outcome
NN_projection <- function(covariates, outcome, unit_vector_samples){
  projected_NN_samples <- matrix(0, nrow = NN_parameter, ncol = unit_sample_size)
  for(iter in 1:unit_sample_size){
    unit_vector <- unit_vector_samples[, iter]
    projected_covariates <- c(t(covariates) %*% unit_vector)
    NN_covariates_index <- order(projected_covariates)[(sample_size - NN_parameter + 1): sample_size]
    projected_NN_samples[, iter] <- sort(diag(outcome[NN_covariates_index]) %*% projected_covariates[NN_covariates_index])
  }
  return(projected_NN_samples)
}

#main iteration of the algorithm
objective_vector <- function(descent_vector, projected_NN_samples, unit_vector_samples){
  return_vector <- matrix(rep(0, dimension * NN_parameter), nrow = dimension)
  for (unit_iter in 1:unit_sample_size){
    descent_sorted_index <- order(t(descent_vector) %*% unit_vector_samples[, unit_iter])
    for (return_iter in 1:NN_parameter){
      index <- descent_sorted_index[return_iter]
      return_vector[, index] <- return_vector[, index] + projected_NN_samples[return_iter, unit_iter] * unit_vector_samples[, unit_iter]
    }
  }
  return(return_vector)
}

#project vector to hypersphere with given radius
vector_projection <- function(descent_vector){
  for (iter in 1:NN_parameter){
    if (norm(descent_vector[,iter], type = "2") > norm_bound){
      descent_vector[, iter] <- descent_vector[, iter] / norm(descent_vector[, iter], type = "2") * norm_bound
    }
  }
  return(descent_vector)
}