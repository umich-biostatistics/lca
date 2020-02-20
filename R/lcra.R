
#' Joint Bayesian Latent Class and Regression Analysis
#' 
#' Given a set of categorical manifest outcomes, identify unmeasured class membership
#' among subjects, and use latent class membership to predict regression outcome
#' y jointly with a set of regressors.
#' 
#' @param formula If formula = NULL, LCA without regression model is fitted. If
#' a regression model is to be fitted, specify a formula using R standard syntax,
#' e.g., Y ~ age + sex + trt. Do not include manifest variables in the regression
#' model specification. These will be appended internally as latent classes.
#' @param family a description of the error distribution to 
#' be used in the model. Currently the options are c("gaussian") with identity 
#' link and c("binomial") which uses a logit link.
#' @param data data.frame with the column names specified in the regression formula
#' and the manifest argument. The columns used in the regression formula can be of
#' any type and will be dealt with using normal R behaviour. The manifest variable
#' columns, however, must be coded as numeric using positive integers. For example,
#' if one of the manifest outcomes takes on values 'Dislike', 'Neutral',
#' and 'like', then code them as 1, 2, and 3. 
#' @param nclasses numeric, number of latent classes
#' @param manifest character vector containing the names of each manifest variable,
#' e.g., manifest = c("Z1", "med_3", "X5"). The values of the manifest columns must
#' be numerically coded with levels 1 through n_levels, where n_levels is the number
#' of levels for the ith manifest variable. The function will throw an error message
#' if they are not coded properly.
#' @param inits list of initial values for R2WinBUGS. Defaults will be set if nothing
#' is specified. Inits must be a list with n.chains elements; each element of the list
#' is itself a list of starting values for the WinBUGS model. 
#' @param dir Specify full path to the directory where you want
#' to store the WinBUGS output files and BUGS model file.
#' @param n.chains number of Markov chains.
#' @param n.iter number of total iterations per chain including burn-in.
#' @param parameters.to.save character vector of names of all parameters to be saved.
#' If unspecified, all will be saved.
#' @param n.burnin length of burn-in, i.e., number of iterations to discard
#' at the beginning. Default is n.iter/2.
#' @param n.thin thinning rate. Must be a positive integer. Set n.thin > 1 to save
#' memory and computing time if n.iter is large. 
#' @param useWINE logical, attempt to use the Wine emulator to run WinBUGS, defaults
#' to FALSE on Windows and TRUE otherwise.
#' @param WINE character, path to WINE binary file. If not provided, the program will
#' attempt to find the WINE installation on your machine.
#' @param debug logical, keep WinBUGS open debug, inspect chains and summary.
#' @param ... other arguments to bugs(). Run ?bugs to see list of possible
#' arguments to pass into bugs.
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
#' The manifest variable columns in **data** must be coded as numeric with positive
#' numbers. For example, if one of the manifest outcomes takes on values 'Dislike', 'Neutral',
#' and 'like', then code them as 1, 2, and 3. 
#' 
#' @references "Methods to account for uncertainty in latent class assignments 
#' when using latent classes as predictors in regression models, with application 
#' to acculturation strategy measures" (2020) In press at Epidemiology. 
#' doi:10.1097/EDE.0000000000001139
#' 
#' @examples 
#' \dontrun{
#' # Data sets 1 and 2
#' data('paper_sim')
#' data('paper_sim_binary')
#' 
#' # Set initial values
#' inits =
#'   list(
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 2),
#'       alpha = rep(0, length = 3),
#'       tau = 0.5
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 2),
#'       alpha = rep(0, length = 3),
#'       tau = 0.5
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 2),
#'       alpha = rep(0, length = 3),
#'       tau = 0.5
#'     )
#'   )
#' 
#' inits_binary =
#'   list(
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 2),
#'       alpha = rep(0, length = 3)
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 2),
#'       alpha = rep(0, length = 3)
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 2),
#'       alpha = rep(0, length = 3)
#'     )
#'   )
#' 
#' # Fit model 1
#' fit.gaus_paper =
#'   lcra(
#'     formula = Y ~ X1 + X2,
#'     family = "gaussian",
#'     data = paper_sim,
#'     nclasses = 3,
#'     manifest = paste0("Z", 1:10),
#'     inits = inits,
#'     dir = tempdir(),
#'     n.chains = 3,
#'     n.iter = 5000,
#'     parameters.to.save = c("theta", "beta", "true", "alpha")
#'   )
#' 
#' # Model 1 results
#' print(fit.gaus_paper, digits = 3)
#' plot(fit.gaus_paper)
#' 
#' # Extract results
#' fit.gaus_paper$median$true
#' fit.gaus_paper$median$beta
#' fit.gaus_paper$median$alpha
#' 
#' # Fit model 2
#' fit.binom_paper = 
#'   lcra(
#'     formula = Y ~ X1 + X2,
#'     family = "binomial",
#'     data = paper_sim_binary,
#'     nclasses = 3,
#'     manifest = paste0("Z", 1:10),
#'     inits = inits_binary,
#'     dir = tempdir(),
#'     n.chains = 3,
#'     n.iter = 5000,
#'     parameters.to.save = c("theta", "beta", "true", "alpha")
#'   )
#' 
#' # Model 2 results
#' print(fit.binom_paper, digits = 3)
#' plot(fit.binom_paper)
#' 
#' # Extract results
#' fit.binom_paper$median$true
#' fit.binom_paper$median$beta
#' fit.binom_paper$median$alpha
#' 
#' 
#' # Data sets 3 and 4
#' data('latent3')
#' data('latent3_binary')
#' 
#' # Set initial values
#' inits =
#'   list(
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 4),
#'       alpha = rep(0, length = 3),
#'       tau = 0.5
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 4),
#'       alpha = rep(0, length = 3),
#'       tau = 0.5
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 4),
#'       alpha = rep(0, length = 3),
#'       tau = 0.5
#'     )
#'   )
#' 
#' inits_binary =
#'   list(
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 4),
#'       alpha = rep(0, length = 3)
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 4),
#'       alpha = rep(0, length = 3)
#'     ),
#'     list(
#'       theta = c(0.33, 0.33, 0.34),
#'       beta = rep(0, length = 4),
#'       alpha = rep(0, length = 3)
#'     )
#'   )
#' 
#' # Fit model 3
#' fit.gaus_latent3 =
#'   lcra(
#'     formula = y ~ x1 + x2 + x3 + x4,
#'     family = "gaussian",
#'     data = latent3,
#'     nclasses = 3,
#'     manifest = paste0("Z", 1:12),
#'     inits = inits,
#'     dir = tempdir(),
#'     n.chains = 3,
#'     n.iter = 5000,
#'     parameters.to.save = c("theta", "beta", "true", "alpha")
#'   )
#' 
#' # Model 3 results
#' print(fit.gaus_latent3, digits = 3)
#' plot(fit.gaus_latent3)
#' 
#' # Extract results
#' fit.gaus_latent3$median$true
#' fit.gaus_latent3$median$beta
#' fit.gaus_latent3$median$alpha
#' 
#' # Fit model 4
#' fit.binom_latent3 = 
#'   lcra(
#'     formula = y ~ x1 + x2 + x3 + x4,
#'     family = "binomial",
#'     data = latent3_binary,
#'     nclasses = 3,
#'     manifest = paste0("Z", 1:12),
#'     inits = inits_binary,
#'     dir = tempdir(),
#'     n.chains = 3,
#'     n.iter = 5000,
#'     parameters.to.save = c("theta", "beta", "true", "alpha")
#'   )
#' 
#' # Model 4 results
#' print(fit.binom_latent3, digits = 3)
#' plot(fit.binom_latent3)
#' 
#' # Extract results
#' fit.binom_latent3$median$true
#' fit.binom_latent3$median$beta
#' fit.binom_latent3$median$alpha
#' }
#' 
#' @return 
#' A list containing the following items:
#' * **sims.array**: 3-dimensional array of simulation output, with dimensions n, 
#' n.chains, and length of combined parameter vector.
#' * **sims.list**: list of simulated parameters: for each scalar parameter, 
#' a vector of length n.sims for each vector parameter; a 2-way array of simulations, 
#' for each matrix parameter.
#' * **sims.matrix**: matrix of simulation output, with n.chains x n rows and 
#' one column for each element of each saved parameter.
#' * **summary**: summary statistics and convergence information for each saved parameter.
#' * **mean**: a list of the estimated parameter means.
#' * **sd**: a list of the estimated parameter standard deviations.
#' * **median**: a list of the estimated parameter medians.
#' * **model.frame**: the model frame.
#' * **model.matrix**: the model matrix.
#' * **bugs.object**: the complete bugs object.
#' * **model**: the BUGS model as a function.
#' 

lcra = function(formula, family, data, nclasses, manifest, inits = NULL, dir, 
                n.chains = 3, n.iter = 2000, parameters.to.save, n.burnin = n.iter/2, 
                n.thin = 1, useWINE = FALSE, WINE, debug = FALSE, ...) {
  
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
  
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { 
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  
  OS = get_os()
  
  if(OS == 'windows') useWINE = FALSE
  else if(OS == 'osx') useWINE = TRUE
  else if(OS == 'linux') useWINE = TRUE
  
  N = nrow(data)
  n_manifest = length(manifest)
  
  if(is.null(formula)) {do_regression = FALSE}
  else {do_regression = TRUE}
  
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"), names(mf))
  mf = mf[c(1L, m)]
  mf[[1L]] = quote(stats::model.frame)
  mf = eval(mf, parent.frame())
  
  mt = attr(mf, "terms")
  x = model.matrix(mt, mf)
  x = x[,!colnames(x) %in% c('(Intercept)'), drop = FALSE]
  
  y = mf$y
  
  # select manifest variables from model matrix
  if(any(!(manifest %in% colnames(data)))) {
    stop("At least one manifest variable name is not in the names of variables
         in the data set.")
  }
  
  Z = data[,manifest, drop = FALSE]
  
  lapply(Z, function(x) {
    if(!is.numeric(x)) {
      stop('All manifest variables must be numeric. At least one of the manifest
           variables you specified is not numeric.')
    }
    if(any(x < 0)) {
      stop('At least one of the manifest variables you specified contains negative values. 
           Code levels of manifest variables to take on values 1 through number of levels.')
    }
  })
  
  manifest.levels = apply(Z, 2, function(x) {length(unique(x))})
  
  unique.manifest.levels = unique(manifest.levels)
  
  p.length = length(unique.manifest.levels)
  
  pclass_prior = round(rep(1/nclasses, nclasses), digits = 3)
  if(sum(pclass_prior) != 1){
    pclass_prior[length(pclass_prior)] = pclass_prior[length(pclass_prior)] + 
      (1 - sum(pclass_prior))
  }
  
  dat_list = vector(mode = "list", length = 6)
  
  prior_mat = matrix(NA, nrow = ncol(Z), ncol = max(unique.manifest.levels))
  
  for(j in 1:length(manifest.levels)) {
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
  
  dat_list[["y"]] = y
  
  dat_list[["x"]] = structure(
    .Data=x,
    .Dim=c(N,ncol(x))
  )
  
  nlevels = apply(Z, 2, function(x) {length(unique(x))})
  names(nlevels) = NULL
  dat_list[["nlevels"]] = nlevels
  
  n_beta = ncol(x)
  
  regression = c()
  response = c()
  tau = c()
  
  if(family == "gaussian") {
    response = expr(y[i] ~ dnorm(yhat[i], tau))
    regression = expr(yhat[i] <- inprod(x[i,], beta[]) + inprod(C[i,], alpha[]))
    tau = expr(tau~dgamma(0.1,0.1))
  } else if(family == "binomial") {
    response = expr(y[i] ~ dbern(p[i]))
    regression = expr(logit(p[i]) <- inprod(x[i,], beta[]) + inprod(C[i,], alpha[]))
    tau = NULL
  }
  
  # call R bugs model constructor
  model = constr_bugs_model(N = N, n_manifest = n_manifest, n_beta = n_beta,
                            nclasses = nclasses, npriors = unique.manifest.levels, 
                            regression = regression, response = response, tau = tau)
  
  # write model
  filename <- file.path(dir, "model.bug")
  write.model(model, filename)
  
  # Fit Bayesian latent class model using BUGS
  suppressWarnings({
    samp_lcra = bugs(data = dat_list, 
                   inits = inits,
                   model.file = filename, 
                   n.chains = n.chains, 
                   n.iter = n.iter, 
                   parameters.to.save = parameters.to.save, 
                   debug = debug, 
                   n.burnin = n.burnin, 
                   n.thin = n.thin, 
                   useWINE = useWINE, 
                   WINE = WINE, ...)
  })
  
  
  # Results
  sims.array = samp_lcra$sims.array
  sims.list = samp_lcra$sims.list
  sims.matrix = samp_lcra$sims.matrix
  summary = samp_lcra$summary
  mean = samp_lcra$mean
  sd = samp_lcra$sd
  median = samp_lcra$median
  
  
  result = 
    list(sims.array = sims.array,
         sims.list = sims.list,
         sims.matrix = sims.matrix,
         summary = summary,
         mean = mean,
         sd = sd,
         median = median,
         model.frame = mf,
         model.matrix = x,
         bugs.object = samp_lcra,
         model = model)
  
  attr(result, "class") = "lcra"
  
  return(result)
  
}


constr_bugs_model = function(N, n_manifest, n_beta, nclasses, npriors,
                             regression, response, tau) {
  
  constructor = function() {
    
    bugs_model_enque = 
      quo({
        
        bugs_model_func = function() {
          
          for (i in 1:!!N){
            true[i]~dcat(theta[])
            
            for(j in 1:!!n_manifest){
              Z[i,j]~dcat(Zprior[true[i],j,1:nlevels[j]])
            }
            
            for(k in 1:(!!nclasses)) {
              C[i,k] <- step(-true[i]+k) - step(-true[i]+k-1)
            }
            
            !!response
            !!regression
            
          }
          
          theta[1:!!nclasses]~ddirch(prior[])
          
          for(c in 1:!!nclasses) {
            for(j in 1:!!n_manifest) {
              Zprior[c,j,1:nlevels[j]]~ddirch(prior_mat[j,1:nlevels[j]])
            }
          }
          
          for(k in 1:!!n_beta) {
            beta[k]~dnorm(0,0.1)
          }
          
          for(k in 1:!!nclasses) {
            alpha[k]~dnorm(0,0.1)
          }
          
          !!tau
          
        }
      })
    
    return(bugs_model_enque)
  }
 
  text_fun = as.character(quo_get_expr(constructor()))[2]
  return(eval(parse(text = text_fun)))
  
}


#' Printing an lcra object
#' 
#' Print the lcra model output using print.bugs.
#' 
#' @param x an object of class 'lcra', see lcra() for details
#' @param ... further arguments to print

print.lcra = function(x, ...) {
  if (class(x) != "lcra") {
    stop("Must be a lcra object to extract the Bugs model.")
  }
  return(R2WinBUGS:::print.bugs(x$bugs.object, ...))
}


#' Plotting an lcra object
#' 
#' Plot the lcra model output using plot.bugs.
#' 
#' @param x an object of class 'lcra', see lcra() for details
#' @param ... further arguments to plot

plot.lcra = function(x, ...) {
  if (class(x) != "lcra") {
    stop("Must be a lcra object to extract the Bugs model.")
  }
  return(R2WinBUGS:::plot.bugs(x$bugs.object, ...))
}
