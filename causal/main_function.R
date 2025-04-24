source('aux_function.R')

#main implementation of Algorithm 2
approximate_block_coordinate_descent <- function(covariates, outcome, norm_bound, unit_sample_size, iteration){
  unit_sample_size <<- unit_sample_size
  dimension <<- nrow(covariates)
  sample_size <<- length(outcome)
  NN_parameter <<- ceiling(sample_size^(dimension / (2 * dimension - 1)))
  norm_bound <<- norm_bound
  
  unit_vector_samples <- unit_vector_sample_generation(dimension, unit_sample_size)
  projected_NN_samples <- NN_projection(covariates, outcome, unit_vector_samples)
  
  descent_vector <- norm_bound * unit_vector_sample_generation(dimension, NN_parameter) %*% diag(runif(NN_parameter)^(1 / dimension))
  
  for (iter in 1:iteration){
    descent_vector <- objective_vector(descent_vector, projected_NN_samples, unit_vector_samples) * dimension / unit_sample_size
    descent_vector <- vector_projection(descent_vector)
  }
  
  return(descent_vector)  
}


