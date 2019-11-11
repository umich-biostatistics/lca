#' Simulate data
#' 
#' Generate a simulated data set
#'
#'
#' @param n is number of observations
#' @param k number of manifest variables
#'


data_sim = function(n = 100, k = 12) {
  
  n = 100
  k = 12
  
  theta = c(0.15, 0.30, 0.55)
  
  true_cat = sample(c(1, 2, 3), 100, prob = theta, replace = TRUE)
  
  true_manifest_p = matrix(data = c(0.80, 0.15, 0.025, 0.025,
                                    0.20, 0.40, 0.38, 0.02, 
                                    0.05, 0.05, 0.10, 0.80),
                           nrow = 3, ncol = 4, byrow = TRUE)
  
  manifest = matrix(data = NA, nrow = n, ncol = k)
  
  for(i in 1:n) {
    manifest[i,] = sample(c(1,2,3,4), k, prob = true_manifest_p[true_cat[i],], replace = TRUE)
  }
  
  return(manifest)
}