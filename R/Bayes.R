
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
#' Construct bugs latent class model in the form of a function for use in the code
#' function bayes_lca
#'
#' @param x model matrix
#'
#' @return R function which contains Bugs model

constr_bugs_model = function(N, n_manifest, nclasses) {
  
  constructor = function() {
    
    bugs_model_enque = 
      quo({
        
        bugs_model_func = function() {
          
          for (i in 1:!!N){
            
            true[i]~dcat(theta[])
            
            for(j in 1:!!n_manifest){
              Z[i,j]~dcat(Zprior[true[i],j,])
            }
          }
          
          theta[1:!!nclasses]~ddirch(prior[])
          
          for(c in 1:!!nclasses) {
            for(j in 1:!!n_manifest) {
              Zprior[c,j,1:4]~ddirch(prior4[])
            }
          }
          
        }
        
      })
    
    return(bugs_model_enque)
  }
 
  text_fun = as.character(quo_get_expr(constructor()))[2]
  return(eval(parse(text = text_fun)))

  
}

