
#' Bayesian Latent Class Analysis
#' 
#' Perform Bayesian LCA
#'
#' Given a set of categorical predictors, draw posterior distribution of 
#' probability of class membership for each observation.
#' 
#' @param formula If formula = NULL, LCA without regression model is fitted. If
#' a regression model is to be fitted, specify a formula using R standard syntax,
#' e.g., Y ~ age + sex + trt. Do not include manifest variables in the regression
#' model specification. These will be appended internally as latent classes.
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
  
  # check for valid formula?
  
  if(is.null(data)) {
    stop("A data set is required to fit the model.")
  }
  
  if(!is.data.frame(data)) {
    stop("A data.frame is required to fit the model.")
  }
  
  N = nrow(data)
  n_manifest = length(manifest)
  
  if(is.null(formula)) {regression = FALSE}
  else {regression = TRUE}
  
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
  
  unique.manifest.levels = unique(apply(Z, 2, function(x) {length(unique(x))}))
  
  pclass_prior = round(rep(1/nclasses, nclasses), digits = 3)
  if(sum(pclass_prior) != 1){
    pclass_prior[length(pclass_prior)] = pclass_prior[length(pclass_prior)] + 
      (1 - sum(pclass_prior))
  }
  
  dat_list = vector(mode = "list", length = length(unique.manifest.levels) + 2)
  name = vector(mode = "numeric", length = length(unique.manifest.levels))
  for(j in 1:length(unique.manifest.levels)) {
    name[j] = paste("prior", unique.manifest.levels[j], sep = "")
    prior = round(rep(1/unique.manifest.levels[j], unique.manifest.levels[j]), digits = 3)
    if(sum(prior) != 1){
      prior[length(prior)] = prior[length(prior)] + 
        (1 - sum(prior))
    }
    dat_list[[j]] = prior
  }
  names(dat_list)
  
  names(dat_list) = c(name, "prior", "Z")
  
  dat_list["prior"] =  pclass_prior
  dat_list["Z"] = structure(
    .Data=as.vector(Z),
    .Dim=c(N,n_manifest)
  )
  
  # construct R2WinBUGS input
  
  # call R bugs model constructor
  model = constr_bugs_model(N = N, n_manifest = n_manifest,
                            nclasses = nclasses, regression = regression)
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
         model.matrix = x,
         bugs.object = samp_lca)
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

constr_bugs_model = function(N, n_manifest, n_beta, nclasses, regression) {
  
  constructor = function() {
    
    bugs_model_enque = 
      quo({
        
        bugs_model_func = function() {
          
          for (i in 1:!!N){
            true[i]~dcat(theta[])
            
            for(j in 1:!!n_manifest){
              Z[i,j]~dcat(Zprior[true[i],j,])
            }
            
            # vectorize regression expression
            # vectorized multiplication?
            yhat[i] <- x[i]*beta + C[i]*alpha
            y[i]~dnorm(yhat[i],tau)
          }
          
          theta[1:!!nclasses]~ddirch(prior[])
          
          for(c in 1:!!nclasses) {
            for(j in 1:!!n_manifest) {
              Zprior[c,j,1:4]~ddirch(prior4[])
            }
          }
          
          for(k in 1:!!n_beta) {
            beta[k]~dnorm(0,0.1)
          }
          
          for(k in 1:!!nclasses) {
            alpha[k]~dnorm(0,0.1)
          }
          
          tau~dgamma(0.1,0.1)
          
        }
        
      })
    
    return(bugs_model_enque)
  }
 
  text_fun = as.character(quo_get_expr(constructor()))[2]
  return(eval(parse(text = text_fun)))
  
}

