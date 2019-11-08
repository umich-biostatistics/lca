
#' Bayesian Latent Class Analysis
#' 
#' Perform Bayesian LCA
#'
#' Given a set of categorical predictors, draw posterior distribution of 
#' probability of class membership for each observation.
#' 
#' @param formula
#' @param data
#' @param init
#' @param ...
#' 
#' @return a named list of draws.
#'

bayes_lca = function(formula, data, init, ...) {
  
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
  
  # Fit Bayesian latent class model
  
  # Results
  
  return(
    list(model.frame = mf,
         model.matrix = x)
  )
  
}
