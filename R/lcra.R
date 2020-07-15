
#' Joint Bayesian Latent Class and Regression Analysis
#' 
#' Given a set of categorical manifest outcomes, identify unmeasured class membership
#' among subjects, and use latent class membership to predict regression outcome
#' jointly with a set of regressors.
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
#' @param inits list of initial values. Defaults will be set if nothing
#' is specified. Inits must be a list with n.chains elements; each element of the list
#' is itself a list of starting values for the model. 
#' @param dir Specify full path to the directory where you want
#' to store the model file.
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
#' @param debug logical, keep WinBUGS open debug, inspect chains and summary.
#' @param ... other arguments to bugs(). Run ?bugs to see list of possible
#' arguments to pass into bugs.
#' @param sampler which MCMC sampler to use? lcra relies on Gibbs sampling,
#' where the options are "WinBUGS" or "JAGS". sampler = "JAGS" is the default,
#' and is recommended
#' @param n.adapt number of adaptive samples to take when using JAGS. See the
#' JAGS documentation for more information.
#' 
#' @details 
#' lcra allows for two different Gibbs samplers to be used. The options are
#' WinBUGS or JAGS. If you are not on a Windows system, WinBUGS can be 
#' very difficult to get working. For this reason, JAGS is the default.
#' 
#' For further instructions on using WinBUGS, read this:
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
#' **Model Definition**
#' 
#' The LCRA model is as follows:
#' 
#' \figure{model1.png}
#' \figure{model2.png}
#' 
#' The following priors are the default and cannot be altered by the user:
#' 
#' \figure{model3.png}
#' \figure{model4.png}
#' 
#' Please note also that the reference category for latent classes in the outcome 
#' model output is always the Jth latent class in the output, and the bugs
#' output is defined by the Latin equivalent of the model parameters
#' (beta, alpha, tau, pi, theta). Also, the bugs output includes the variable true,
#' which corresponds to the MCMC draws of C_i, i = 1,...,n, as well as the MCMC 
#' draws of the deviance (DIC) statistic. Finally the bugs output for pi
#' is stored in a three dimensional array corresponding to (class, variable, category),
#' where category is indexed by 1 through maximum K_l; for variables where the number of 
#' categories is less than maximum K_l, these cells will be set to NA. The parameters
#' outputted by the lcra function currently are not user definable.
#' 
#' @references "Methods to account for uncertainty in latent class assignments 
#' when using latent classes as predictors in regression models, with application 
#' to acculturation strategy measures" (2020) In press at Epidemiology. 
#' doi:10.1097/EDE.0000000000001139
#' 
#' @examples 
#' if(requireNamespace("rjags")){
#' 
#' # quick example
#' 
#' inits = list(list(theta = c(0.33, 0.33, 0.34), beta = rep(0, length = 3), 
#'                   alpha = rep(0, length = 2), tau = 0.5, true = rep(1, length = nrow(express))))
#' 
#' fit = lcra(formula = y ~ x1 + x2, family = "gaussian", data = express,
#'            nclasses = 3, inits = inits, manifest = paste0("Z", 1:5),
#'            n.chains = 1, n.iter = 50)
#' \donttest{
#' data('paper_sim')
#' 
#' # Set initial values
#' inits =
#'   list(
#'     list(theta = c(0.33, 0.33, 0.34), beta = rep(0, length = 3),
#'          alpha = rep(0, length = 2), tau = 0.5, true = rep(1, length = 100)),
#'     list(theta = c(0.33, 0.33, 0.34), beta = rep(0, length = 3),
#'          alpha = rep(0, length = 2), tau = 0.5, true = rep(1, length = 100)),
#'     list(theta = c(0.33, 0.33, 0.34), beta = rep(0, length = 3),
#'          alpha = rep(0, length = 2), tau = 0.5, true = rep(1, length = 100))
#'   )
#' 
#' # Fit model 1
#' fit.gaus_paper = lcra(formula = Y ~ X1 + X2, family = "gaussian",
#'                       data = paper_sim, nclasses = 3, manifest = paste0("Z", 1:10),
#'                       inits = inits, n.chains = 3, n.iter = 5000)
#' 
#' # Model 1 results
#' library(coda)
#' 
#' summary(fit.gaus_paper)
#' plot(fit.gaus_paper)
#' 
#' 
#' # simulated examples
#' 
#' library(gtools) # for Dirichel distribution
#' 
#' # with binary response 
#' 
#' n <- 500
#' 
#' X1 <- runif(n, 2, 8)
#' X2 <- rbinom(n, 1, .5)
#' Cstar <- rnorm(n, .25 * X1 - .75 * X2, 1)
#' C <- 1 * (Cstar <= .8) + 2 * ((Cstar > .8) & (Cstar <= 1.6)) + 3 * (Cstar > 1.6)
#' 
#' pi1 <- rdirichlet(10, c(5, 4, 3, 2, 1))
#' pi2 <- rdirichlet(10, c(1, 3, 5, 3, 1))
#' pi3 <- rdirichlet(10, c(1, 2, 3, 4, 5))
#' 
#' Z1<-(C==1)*t(rmultinom(n,1,pi1[1,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[1,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[1,]))%*%c(1:5)
#' Z2<-(C==1)*t(rmultinom(n,1,pi1[2,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[2,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[2,]))%*%c(1:5)
#' Z3<-(C==1)*t(rmultinom(n,1,pi1[3,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[3,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[3,]))%*%c(1:5)
#' Z4<-(C==1)*t(rmultinom(n,1,pi1[4,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[4,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[4,]))%*%c(1:5)
#' Z5<-(C==1)*t(rmultinom(n,1,pi1[5,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[5,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[5,]))%*%c(1:5)
#' Z6<-(C==1)*t(rmultinom(n,1,pi1[6,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[6,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[6,]))%*%c(1:5)
#' Z7<-(C==1)*t(rmultinom(n,1,pi1[7,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[7,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[7,]))%*%c(1:5)
#' Z8<-(C==1)*t(rmultinom(n,1,pi1[8,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[8,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[8,]))%*%c(1:5)
#' Z9<-(C==1)*t(rmultinom(n,1,pi1[9,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[9,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[9,]))%*%c(1:5)
#' Z10<-(C==1)*t(rmultinom(n,1,pi1[10,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[10,]))%*%c(1:5)+(C==3)*t(rmultinom(n,1,pi3[10,]))%*%c(1:5)
#' 
#' Z <- cbind(Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10)
#' 
#' Y <- rbinom(n, 1, exp(-1 - .1*X1 + X2 + 2*(C == 1) + 1*(C == 2)) / 
#'               (1 + exp(1 - .1*X1 + X2 + 2*(C == 1) + 1*(C == 2))))
#' 
#' mydata = data.frame(Y, X1, X2, Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10)
#' 
#' inits = list(list(theta = c(0.33, 0.33, 0.34), beta = rep(0, length = 3),
#'                   alpha = rep(0, length = 2), true = rep(1, length = nrow(mydata))))
#' 
#' fit = lcra(formula = Y ~ X1 + X2, family = "binomial", data = mydata,
#'            nclasses = 3, inits = inits, manifest = paste0("Z", 1:10),
#'            n.chains = 1, n.iter = 1000)
#' 
#' summary(fit)
#' plot(fit)
#' 
#' # with continuous response
#' 
#' n <- 500
#' 
#' X1 <- runif(n, 2, 8)
#' X2 <- rbinom(n, 1, .5)
#' Cstar <- rnorm(n, .25*X1 - .75*X2, 1)
#' C <- 1 * (Cstar <= .8) + 2*((Cstar > .8) & (Cstar <= 1.6)) + 3*(Cstar > 1.6)
#' 
#' pi1 <- rdirichlet(10, c(5, 4, 3, 2, 1))
#' pi2 <- rdirichlet(10, c(1, 3, 5, 3, 1))
#' pi3 <- rdirichlet(10, c(1, 2, 3, 4, 5))
#' pi4 <- rdirichlet(10, c(1, 1, 1, 1, 1))
#' 
#' Z1<-(C==1)*t(rmultinom(n,1,pi1[1,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[1,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[1,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[1,]))%*%c(1:5)
#' Z2<-(C==1)*t(rmultinom(n,1,pi1[2,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[2,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[2,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[2,]))%*%c(1:5)
#' Z3<-(C==1)*t(rmultinom(n,1,pi1[3,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[3,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[3,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[3,]))%*%c(1:5)
#' Z4<-(C==1)*t(rmultinom(n,1,pi1[4,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[4,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[4,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[4,]))%*%c(1:5)
#' Z5<-(C==1)*t(rmultinom(n,1,pi1[5,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[5,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[5,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[5,]))%*%c(1:5)
#' Z6<-(C==1)*t(rmultinom(n,1,pi1[6,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[6,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[6,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[6,]))%*%c(1:5)
#' Z7<-(C==1)*t(rmultinom(n,1,pi1[7,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[7,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[7,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[7,]))%*%c(1:5)
#' Z8<-(C==1)*t(rmultinom(n,1,pi1[8,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[8,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[8,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[8,]))%*%c(1:5)
#' Z9<-(C==1)*t(rmultinom(n,1,pi1[9,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[9,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[9,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[9,]))%*%c(1:5)
#' Z10<-(C==1)*t(rmultinom(n,1,pi1[10,]))%*%c(1:5)+(C==2)*
#'   t(rmultinom(n,1,pi2[10,]))%*%c(1:5)+(C==3)*
#'   t(rmultinom(n,1,pi3[10,]))%*%c(1:5)+(C==4)*t(rmultinom(n,1,pi4[10,]))%*%c(1:5)
#' 
#' Z <- cbind(Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10)
#' 
#' Y <- rnorm(n, 10 - .5*X1 + 2*X2 + 2*(C == 1) + 1*(C == 2), 1)
#' 
#' mydata = data.frame(Y, X1, X2, Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10)
#' 
#' inits = list(list(theta = c(0.33, 0.33, 0.34), beta = rep(0, length = 3),
#'                   alpha = rep(0, length = 2), true = rep(1, length = nrow(mydata)), 
#'                   tau = 0.5))
#' 
#' fit = lcra(formula = Y ~ X1 + X2, family = "gaussian", data = mydata,
#'            nclasses = 3, inits = inits, manifest = paste0("Z", 1:10),
#'            n.chains = 1, n.iter = 1000)
#' 
#' summary(fit)
#' plot(fit)
#' 
#' }
#' }
#' @return 
#' Return type depends on the sampler chosen. 
#' 
#' If sampler = "WinBUGS", then the return object is:
#' WinBUGS object and lists of draws and summaries. Use fit$ to browse options.
#' 
#' If sampler = "JAGS", then the return object is:
#' An MCMC list of class **mcmc.list**, which can be analyzed with the coda package.
#' Each column is a parameter and each row is a draw. You can extract a parameter 
#' by name, e.g., fit[,"beta\[1\]"]. For a list of all parameter names from the fit, 
#' call colnames(as.matrix(fit)), which returns a character vector with the names.
#' 

lcra = function(formula, data, family, nclasses, manifest, sampler = "JAGS", 
                inits = NULL, dir, n.chains = 3, n.iter = 2000, n.burnin = n.iter/2, 
                n.thin = 1, n.adapt = 1000, useWINE = FALSE, WINE, debug = FALSE, ...) {
  
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
  
  y = mf[[setdiff(names(mf), colnames(x))]]
  
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
  
  i <- NULL
  inprod <- NULL 
  alpha <- NULL
  `logit<-` <- NULL
  
  if(family == "gaussian") {
    response = expr(y[i] ~ dnorm(yhat[i], tau))
    regression = expr(yhat[i] <- inprod(x[i,], beta[]) + inprod(C[i,], alpha[]))
    tau = expr(tau~dgamma(0.1,0.1))
    parameters.to.save = c("beta", "true", "alpha", "pi", "theta", "tau")
  } else if(family == "binomial") {
    response = expr(y[i] ~ dbern(p[i]))
    regression = expr(logit(p[i]) <- inprod(x[i,], beta[]) + inprod(C[i,], alpha[]))
    tau = NULL
    parameters.to.save = c("beta", "true", "alpha", "pi", "theta")
  }
  
  # call R bugs model constructor
  model = constr_bugs_model(N = N, n_manifest = n_manifest, n_beta = n_beta,
                            nclasses = nclasses, npriors = unique.manifest.levels, 
                            regression = regression, response = response, tau = tau)
  
  # write model
  filename <- file.path(dir, "model.text")
  write_model(model, filename)
  
  if(sampler == "JAGS") {
    
      model_jags = jags.model(
        file = filename,
        data = dat_list,
        inits = inits,
        n.chains = n.chains,
        n.adapt = n.adapt,
        quiet = FALSE)
      
      samp_lcra = window(
        coda.samples(
          model = model_jags,
          variable.names = parameters.to.save,
          n.iter = n.iter,
          thin = 1
        ), start = n.burnin)
    
  } else if(sampler == "WinBUGS") {
    
      samp_lcra = as.mcmc.list(
        R2WinBUGS::bugs(data = dat_list, 
             inits = inits,
             model.file = filename, 
             n.chains = n.chains, 
             n.iter = n.iter, 
             parameters.to.save = parameters.to.save, 
             debug = debug, 
             n.burnin = n.burnin, 
             n.thin = n.thin, 
             useWINE = useWINE, 
             WINE = WINE, ...))
    
  } else {
    stop('Sampler name is not one of the options, which are JAGS and WinBUGS.')
  }
  
  return(samp_lcra)
}


constr_bugs_model = function(N, n_manifest, n_beta, nclasses, npriors,
                             regression, response, tau) {
  
  true <- NULL
  
  constructor = function() {
    
    bugs_model_enque = 
      quo({
        
        bugs_model_func = function() {
          
          for (i in 1:!!N){
            true[i]~dcat(theta[])
            
            for(j in 1:!!n_manifest){
              Z[i,j]~dcat(pi[true[i],j,1:nlevels[j]])
            }
            
            for(k in 1:(!!(nclasses-1))){
              C[i,k] <- step(-true[i]+k) - step(-true[i]+k-1)
            }
            
            !!response
            !!regression
            
          }
          
          theta[1:!!nclasses]~ddirch(prior[])
          
          for(c in 1:!!nclasses){
            for(j in 1:!!n_manifest){
              pi[c,j,1:nlevels[j]]~ddirch(prior_mat[j,1:nlevels[j]])
            }
          }
          
          for(k in 1:!!n_beta){
            beta[k]~dnorm(0,0.1)
          }
          
          for(k in 1:!!(nclasses-1)){
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
