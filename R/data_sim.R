#' Simulate data
#' 
#' Generate a simulated data set
#'
#'
#' @param n is number of observations
#' @param k number of manifest variables
#'


data_sim = function(n = 50, k = 12) {
  
  n = 50
  k = 12
  
  theta = c(0.15, 0.30, 0.55) # which theta most likely in results
  
  true_cat = sample(c(1, 2, 3), n, prob = theta, replace = TRUE)
  print(true_cat)
  true_manifest_p = matrix(data = c(0.80, 0.15, 0.025, 0.025,
                                    0.20, 0.40, 0.38, 0.02, 
                                    0.05, 0.05, 0.10, 0.80),
                           nrow = 3, ncol = 4, byrow = TRUE)
  
  manifest = matrix(data = NA, nrow = n, ncol = k)
  colnames(manifest) = paste("Z", 1:k, sep = "")
  
  for(i in 1:n) {
    manifest[i,] = sample(c(1,2,3,4), k, prob = true_manifest_p[true_cat[i],], replace = TRUE)
  }
  
  x = matrix(NA, nrow = n, ncol = 1)
  for (i in 1:1) {
    x[,i] = runif(n, -0.8, 0.8)
  }
  
  xc = matrix(NA, nrow = n, ncol = 2)
  for (i in 1:2) {
    temp = (true_cat == i)*1
    xc[,i] = temp
  }
  
  colnames(x) = paste("x", 1, sep = "")
  beta = c(-2.9)
  alpha = c(0.4, -0.3)
  
  y = x %*% beta + xc %*% alpha + rnorm(nrow(x), 0, 0.2)
  colnames(y) = "y"
  data = cbind(y, manifest, x)
  #print(cbind(1:n, true_cat))
  return(data)
}
