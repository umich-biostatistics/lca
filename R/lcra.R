
#' Bayesian Latent Class Regression Analysis
#' 
#' Perform Bayesian LCRA
#'
#' Given a set of categorical predictors, draw posterior distribution of 
#' probability of class membership for each observation.
#' 
#' @param formula If formula = NULL, LCA without regression model is fitted. If
#' a regression model is to be fitted, specify a formula using R standard syntax,
#' e.g., Y ~ age + sex + trt. Do not include manifest variables in the regression
#' model specification. These will be appended internally as latent classes.
#' @param family a description of the error distribution to 
#' be used in the model. Currently the options are c("gaussian") with identity 
#' link and c("binomial") which uses a logit link.
#' @param data
#' @param nclasses
#' @param manifest character vector containing the names of each manifest variable,
#' e.g., manifest = c("Z1", "med_3", "X5")
#' @param inits list of initial values for R2WinBUGS. Defaults will be set if nothing
#' is specified. Inits must be a list with n.chains elements; each element of the list
#' is itself a list of starting values for the WinBUGS model. 
#' 
#' @param dir Specify full path to the directory where you want
#' to store the WinBUGS output files and BUGS model file.
#' @param n.chains number of Markov chains.
#' @param n.iter number of total iterations per chain including burn-in.
#' @param n.burnin length of burn-in, i.e., number of iterations to discard
#' at the beginning. Default is n.iter/2.
#' @param n.thin thinning rate. Must be a positive integer. Set n.thin > 1 to save
#' memory and computing time if n.iter is large. 
#' @param useWINE logical, attempt to use the Wine emulator to run WinBUGS, defaults
#' to FALSE on Windows and TRUE otherwise.
#' @param WINE character, path to WINE binary file. If not provided, the program will
#' attempt to find the WINE installation on your machine.
#' @param ...
#' 
#' @details 
#' Details on running lcra on different operating systems:
#' 
#' * Microsoft Windows: no problems or additional set-up required
#' 
#' * Linux, Mac OS X, Unix: possible with the Wine emulator via useWine = TRUE.
#' Wine is a standalone program needed to emulate a Windows system on non-Windows
#' machines.
#' 
#' @return a named list of draws.
#'

lcra = function(formula, family, data, nclasses, manifest, inits = NULL, dir, 
                     n.chains, n.iter, parameters.to.save, ...) {
  
  # checks on input
  
  # check for valid formula?
  
  if(missing(data)) {
    stop("A data set is required to fit the model.")
  }
  
  if(!is.data.frame(data)) {
    stop("A data.frame is required to fit the model.")
  }
  
  if(missing(family)) {
    stop("Family must be specified. Currently the options are 'gaussian' (identity link) and 'binomial' which uses a logit link.")
  }
  
  if(missing(dir)) {
    dir = tempdir()
  }
  
  if(missing(formula)) {
    stop("Specify an R formula for the regression model to be fitted. 
         If you only want the latent class analysis, set formula = NULL.")
  }
  
  N = nrow(data)
  n_manifest = length(manifest)
  
  if(is.null(formula)) {do_regression = FALSE}
  else {do_regression = TRUE}
  
  # Convert all manifest variables to numeric 1,... nlevels?
  
  # construct a model frame (mf)
  
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"), names(mf))
  mf = mf[c(1L, m)]
  mf[[1L]] = quote(stats::model.frame)
  mf = eval(mf, parent.frame())
  
  # construct a model matrix
  
  mt = attr(mf, "terms")
  x = model.matrix(mt, mf)
  
  y = mf$y
  
  # select manifest variables from model matrix
  if(any(!(manifest %in% colnames(data)))) {
    stop("At least one manifest variable name is not in the names of variables
         in the data set.")
  }
  
  Z = data[,manifest]
  
  manifest.levels = apply(Z, 2, function(x) {length(unique(x))})
  unique.manifest.levels = unique(manifest.levels)
  
  p.length = length(unique.manifest.levels)
  
  pclass_prior = round(rep(1/nclasses, nclasses), digits = 3)
  if(sum(pclass_prior) != 1){
    pclass_prior[length(pclass_prior)] = pclass_prior[length(pclass_prior)] + 
      (1 - sum(pclass_prior))
  }
  
  dat_list = vector(mode = "list", length = 6)
  #name = vector(mode = "numeric", length = length(unique.manifest.levels))
  prior_mat = matrix(NA, nrow = ncol(Z), 
                     ncol = max(unique.manifest.levels))
  for(j in 1:length(manifest.levels)) {
    #name[j] = paste("prior", unique.manifest.levels[j], sep = "")
    prior = round(rep(1/manifest.levels[j], manifest.levels[j]), digits = 3)
    if(sum(prior) != 1){
      prior[length(prior)] = prior[length(prior)] + 
        (1 - sum(prior))
    }
    if(length(prior) < length(prior_mat[j,])) {
      fill = rep(NA, length = (length(prior_mat[j,]) - length(prior)))
      prior = c(prior, fill)
      prior_mat[j,] = prior
    } else {
      prior_mat[j,] = prior
    }
  }
  
  names(dat_list) = c("prior_mat", "prior", "Z", "y", "x", "nlevels")
  
  dat_list[["prior_mat"]] = structure(
    .Data=as.vector(prior_mat),
    .Dim=c(length(manifest.levels), max(unique.manifest.levels))
  )
  
  dat_list[["prior"]] =  pclass_prior
  
  dat_list[["Z"]] = structure(
    .Data=as.vector(as.matrix(Z)),
    .Dim=c(N,n_manifest)
  )
  
  # dat_list[["C"]] = structure(
  #   .Data=rep(0, (nclasses-1) * N),
  #   .Dim=c(N,nclasses-1)
  # )
  
  dat_list[["y"]] = y
  
  dat_list[["x"]] = structure(
    .Data=x,
    .Dim=c(N,ncol(x))
  )
  
  nlevels = apply(Z, 2, function(x) {length(unique(x))})
  names(nlevels) = NULL
  dat_list[["nlevels"]] = nlevels
  
  # construct R2WinBUGS input
  n_beta = ncol(x)
  
  regression = c()
  response = c()
  
  if(family == "gaussian") {
    response = expr(y[i] ~ dnorm(yhat[i], tau))
    regression = expr(yhat[i] <- inprod(x[i,], beta[]) + inprod(C[i,], alpha[]))
  } else if(family == "binomial") {
    response = expr(Y[i]~dbern(p[i]))
    regression = expr(logit(p[i]) <- inprod(x[i,], beta[]) + inprod(C[i,], alpha[]))
  }
  
  # call R bugs model constructor
  model = constr_bugs_model(N = N, n_manifest = n_manifest, n_beta = n_beta,
                            nclasses = nclasses, npriors = unique.manifest.levels, 
                            regression = regression, response = response)
  # write model
  filename <- file.path(dir, "model.bug")
  write.model(model, filename)
  
  # Fit Bayesian latent class model
  samp_lrca = bugs(data = dat_list, inits = inits,
                  model.file = filename, n.chains = n.chains, 
                  n.iter = n.iter, parameters.to.save = parameters.to.save, 
                  debug = TRUE)
  
  # Results
  # return bugs fit
  
  result = 
    list(model.frame = mf,
         model.matrix = x,
         bugs.object = samp_lcra,
         model = model)
  
  attr(result, "class") = "lcra"
  
  return(result)
  
}



#' Contruct Bugs Model
#'
#' Construct bugs latent class model in the form of a function for use in the code
#' function bayes_lca
#'
#' @param x model matrix
#' @param regression Expression which contains code for the response distribution,
#' e.g. expr(stuff)
#'
#' @return R function which contains Bugs model

constr_bugs_model = function(N, n_manifest, n_beta, nclasses, npriors,
                             regression, 
                             response) {
  
  constructor = function() {
    
    bugs_model_enque = 
      quo({
        
        bugs_model_func = function() {
          
          for (i in 1:!!N){
            true[i]~dcat(theta[])
            
            for(j in 1:!!n_manifest){
              Z[i,j]~dcat(Zprior[true[i],j,1:nlevels[j]])
            }
            
            for(k in 2:(!!nclasses)) {
              C[i,k-1] <- step(-true[i]+k) - step(-true[i]+k-1)
            }
            
            !!response
            !!regression
            
          }
          
          theta[1:!!nclasses]~ddirch(prior[])
          
          # need to generalize to all prior""[], make a series of arrays
          for(c in 1:!!nclasses) {
            for(j in 1:!!n_manifest) {
              Zprior[c,j,1:nlevels[j]]~ddirch(prior_mat[j,1:nlevels[j]])
            }
          } # need one of these double loops for each prior length
          
          for(k in 1:!!n_beta) {
            beta[k]~dnorm(0,0.1)
          }
          
          for(k in 2:!!nclasses) {
            alpha[k-1]~dnorm(0,0.1)
          }
          
          tau~dgamma(0.1,0.1)
          
        }
      })
    
    return(bugs_model_enque)
  }
 
  text_fun = as.character(quo_get_expr(constructor()))[2]
  return(eval(parse(text = text_fun)))
  
}


#' Get the Bugs model
#'
#' Sometimes the user may want more flexibility in the model fit than 
#' our program provides. In this case, the user can fit a close model and 
#' use this function to retrieve the model as an R function. 
#'
#' @param fit an lcra fit object
#'
#' @return R function which contains Bugs model

get_bugs_model = function(fit) {
  if(class(fit) != "lcra") {
    stop("Must be a lcra object to extract the Bugs model.")
  }
  return(fit$model)
}


#' Printing an lcra object
#' 
#' Print the lcra model output using print.bugs.
#' 
#' @param x an object of class 'lcra', see lcra() for details
#' @param ... further arguments to print

print.lcra = function(x, ...) {
  if(class(x) != "lcra") {
    stop("Must be a lcra object to extract the Bugs model.")
  }
  
  return(print.bugs(x, ...))
}


#' Plotting an lcra object
#' 
#' Plot the lcra model output using plot.bugs.
#' 
#' @param x an object of class 'lcra', see lcra() for details
#' @param ... further arguments to plot

plot.lcra = function(x, ...) {
  if(class(x) != "lcra") {
    stop("Must be a lcra object to extract the Bugs model.")
  }
  
  return(plot.bugs(x, ...))
}








