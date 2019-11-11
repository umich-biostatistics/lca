
#' Bayesian Latent Class Analysis
#' 
#' Perform Bayesian LCA
#'
#' Given a set of categorical predictors, draw posterior distribution of 
#' probability of class membership for each observation.
#' 
#' @param formula
#' @param data
#' @param nclasses
#' @param init
#' @param ...
#' 
#' @return a named list of draws.
#'

bayes_lca = function(formula, data, nclasses, init, manifest, ...) {
  
  # checks on input
  
  if(is.null(formula)) {
    stop("A model formula is required.")
  }
  
  if(is.null(data)) {
    stop("A data set is required to fit the model.")
  }
  
  if(!is.data.frame(data)) {
    stop("A data.frame is required to fit the model.")
  }
  
  # construct a model frame (mf)
  
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"), names(mf))
  mf = mf[c(1L, m)]
  mf[[1L]] = quote(stats::model.frame)
  mf = eval(mf, parent.frame())
  
  # construct a model matrix
  
  mt = attr(mf, "terms")
  x = model.matrix(mt, mf)
  
  # construct R2WinBUGS input
  
  # call R bugs model constructor
  model = constr_bugs_model()
  
  # Fit Bayesian latent class model
  
  # Results
  # return bugs fit
  return(
    list(model.frame = mf,
         model.matrix = x)
  )
  
}



#' Contruct Bugs Model
#'
#' Construct bugs latent class model and return the result for use in the code
#' function bayes_lca
#'
#' @param x model matrix
#'
#' @return R function which contains Bugs model

constr_bugs_model = function() {
  
  # return the exact form we want to fit,
  
  bugs_model_func = function() {
   
    list( theta=c(0.33,0.33,0.34) )	
    
    model{
      for (i in 1:N){
        true[i]~dcat(theta[])
        
        Z1[i]~dcat(Z1prior[true[i],])
        Z2[i]~dcat(Z2prior[true[i],])
        Z3[i]~dcat(Z3prior[true[i],])
        Z4[i]~dcat(Z4prior[true[i],])
        Z5[i]~dcat(Z5prior[true[i],])
        Z6[i]~dcat(Z6prior[true[i],])
        Z7[i]~dcat(Z7prior[true[i],])
        Z8[i]~dcat(Z8prior[true[i],])
        Z9[i]~dcat(Z9prior[true[i],])
        Z10[i]~dcat(Z10prior[true[i],])
        Z11[i]~dcat(Z11prior[true[i],])
        Z12[i]~dcat(Z12prior[true[i],])
        
      }
      
      theta[1:3]~ddirch(prior[])
      
      for(j in 1:3) {
        Z1prior[j,1:4]~ddirch(prior4[])
        Z2prior[j,1:4]~ddirch(prior4[])
        Z3prior[j,1:4]~ddirch(prior4[])
        Z4prior[j,1:4]~ddirch(prior4[])
        Z5prior[j,1:4]~ddirch(prior4[])
        Z6prior[j,1:4]~ddirch(prior4[])
        Z7prior[j,1:4]~ddirch(prior4[])
        Z8prior[j,1:4]~ddirch(prior4[])
        Z9prior[j,1:4]~ddirch(prior4[])
        Z10prior[j,1:4]~ddirch(prior4[])
        Z11prior[j,1:4]~ddirch(prior4[])
        Z12prior[j,1:4]~ddirch(prior4[])
      }
      
    }
    
    
  }
  
  return(bugs_model_func)
}




