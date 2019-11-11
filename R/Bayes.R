
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

constr_bugs_model = function(x) {
  
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
        
        #C1[i]<-step(-true[i]+1)
        #C3[i]<-step(true[i]-3)
        #C2[i]<-step(true[i]-2)-C3[i] # need to re-write
        
        #yhat[i] <- 
        #  alpha[1]+
        #  alpha[2]*C2[i]+
        #  alpha[3]*C3[i]
        
        #y[i]~dnorm(yhat[i],tau)
        
      }
      
      theta[1:3]~ddirch(prior[])
      
      #alpha[1]~dnorm(0,0.1)
      #alpha[2]~dnorm(0,0.1)
      #alpha[3]~dnorm(0,0.1)
      
      #tau~dgamma(0.1,0.1)
      
      Z1prior[1,1:5]~ddirch(prior5[])
      Z1prior[2,1:5]~ddirch(prior5[])
      Z1prior[3,1:5]~ddirch(prior5[])
      Z2prior[1,1:5]~ddirch(prior5[])
      Z2prior[2,1:5]~ddirch(prior5[])
      Z2prior[3,1:5]~ddirch(prior5[])
      Z3prior[1,1:5]~ddirch(prior5[])
      Z3prior[2,1:5]~ddirch(prior5[])
      Z3prior[3,1:5]~ddirch(prior5[])
      Z4prior[1,1:5]~ddirch(prior5[])
      Z4prior[2,1:5]~ddirch(prior5[])
      Z4prior[3,1:5]~ddirch(prior5[])
      Z5prior[1,1:5]~ddirch(prior5[])
      Z5prior[2,1:5]~ddirch(prior5[])
      Z5prior[3,1:5]~ddirch(prior5[])
      Z6prior[1,1:5]~ddirch(prior5[])
      Z6prior[2,1:5]~ddirch(prior5[])
      Z6prior[3,1:5]~ddirch(prior5[])
      Z7prior[1,1:5]~ddirch(prior5[])
      Z7prior[2,1:5]~ddirch(prior5[])
      Z7prior[3,1:5]~ddirch(prior5[])
      Z8prior[1,1:6]~ddirch(prior6[])
      Z8prior[2,1:6]~ddirch(prior6[])
      Z8prior[3,1:6]~ddirch(prior6[])
      Z9prior[1,1:6]~ddirch(prior6[])
      Z9prior[2,1:6]~ddirch(prior6[])
      Z9prior[3,1:6]~ddirch(prior6[])
      Z10prior[1,1:6]~ddirch(prior6[])
      Z10prior[2,1:6]~ddirch(prior6[])
      Z10prior[3,1:6]~ddirch(prior6[])
      Z11prior[1,1:6]~ddirch(prior6[])
      Z11prior[2,1:6]~ddirch(prior6[])
      Z11prior[3,1:6]~ddirch(prior6[])
      Z12prior[1,1:5]~ddirch(prior5[])
      Z12prior[2,1:5]~ddirch(prior5[])
      Z12prior[3,1:5]~ddirch(prior5[])
      
    }
    
    
  }
  
  return(bugs_model_func)
}




