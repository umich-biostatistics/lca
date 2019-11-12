
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
#' @param manifest character vector containing the names of each manifest variable,
#' e.g., manifest = c("Z1", "med_3", "X5")
#' @param inits list of initial values for WinBUGS. Defaults will be set if nothing
#' is specified.
#' @param return.bugs = FALSE. If TRUE, return the bugs model as a character string.
#' This can be further edited to suit the researchers needs, e.g., by adding hyper-prior
#' distributions.
#' @param dir Specify full path to the directory where you want
#' to store the WinBUGS output files and BUGS model file.
#' @param ...
#' 
#' @return a named list of draws.
#'

bayes_lca = function(formula, data, nclasses, manifest, inits, return.bugs, dir, 
                     n.chains, n.iter, parameters.to.save, ...) {
  
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
  
  N = nrow(data)
  n_manifest = length(manifest)
  
  # construct a model frame (mf)
  
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"), names(mf))
  mf = mf[c(1L, m)]
  mf[[1L]] = quote(stats::model.frame)
  mf = eval(mf, parent.frame())
  
  # construct a model matrix
  
  mt = attr(mf, "terms")
  x = model.matrix(mt, mf)
  
  # select manifest variables from model matrix
  if(any(!(manifest %in% colnames(mf)))) {
    stop("At least one manifest variable name is not in the names of variables
         in the model frame.")
  }
  
  Z = mf[,manifest]
  
  pclass_prior = round(rep(1/nclasses, nclasses), digits = 3)
  if(sum(pclass_prior) != 1){
    pclass_prior[length(pclass_prior)] = pclass_prior[length(pclass_prior)] + 
      (1 - sum(pclass_prior))
  }
  
  dat_list = list(
    Z = structure(
      .Data=as.vector(Z),
      .Dim=c(N,n_manifest)
    ),
    prior = pclass_prior,
    prior4 = c(0.25, 0.25, 0.25, 0.25)
  )
  
  # construct R2WinBUGS input
  
  # call R bugs model constructor
  model = constr_bugs_model(N = N, n_manifest = n_manifest,
                            nclasses = nclasses)
  # write model
  filename <- file.path(dir, "model.bug")
  write.model(constr_bugs_model(N = N, n_manifest = n_manifest, nclasses = nclasses), filename)
  
  # Fit Bayesian latent class model
  samp_lca = bugs(data = dat_list, inits = inits,
                  model.file = filename, n.chains = n.chains, 
                  n.iter = n.iter, parameters.to.save = parameters.to.save, 
                  debug = TRUE)
  
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

