
#' An S4 class to represent hyperparameter optimization results
#'
#' @slot res (matrix) of classification accuracy results, rows for iterations and columns for parameter combinations
#' @slot namegrid (list) parameter combinations
#' @slot baseheur (list) metaheur objects
#' @slot walltime (integer) execution wall-clock time in minutes

setClass("metaheurhyper", representation(res="matrix", namegrid="list", baseheurs="list", walltime="integer"))


#' search for best hyperparameters for metaheuristic optimization
#'
#' @param gridclassobject (GridClass) created by setgrid function in preprocomb package, defaults to examplegrid
#' @param iterations (integer) number of iterations done for a restart
#' @param cores (integer) number of cores used in computation of classification accuracies holdout rounds
#' @param searchtype (character) grid or random search
#' @param nrohyperparams (integer) number of hyperparameters used in random search, between 1 and 5, default to 3
#' @param nholdout (integer) number of holdout rounds in computing classification accuracies
#' @param trials (integer) number of trials
#' @param model (character) caret name of predictive model, defaults to "rpart"
#' @examples ## result <- metaheurhyper(cores=2, trials=2, iterations=30)
#' @export


metaheurhyper <- function(gridclassobject=examplegrid,  searchtype="grid", nrohyperparams=3, iterations=10, cores=1, nholdout=2, trials=3, model="rpart"){

  ## CREATING HYPERPARAMETER SPACE

  starttime <- Sys.time()

  doParallel::registerDoParallel(cores=cores)

  ## CREATING HYPERPARAMETER GRIDS

  # classical grid

  if (searchtype=="grid"){
  initialtemperature=c(0.85,1)
  tempconst=0.95
  reheat=0.05
  taboolistlength=c(1,3)
  late=c(0,2)
  }

  # random grid

  if (searchtype=="random"){
  initialtemperature <- runif(nrohyperparams, 0.01, 0.99)
  tempconst <- runif(nrohyperparams, 0.85, 1)
  reheat <- runif(nrohyperparams, 0.01, 0.2)
  taboolistlength <- sample(1:5, nrohyperparams)
  late=sample(1:5, nrohyperparams)
  }

  newgrid <- expand.grid(initialtemperature=initialtemperature, tempconst=tempconst, reheat=reheat, taboolistlength=taboolistlength, late=late)

  namegrid <- list()
  for (i in 1:nrow(newgrid)){
    namegrid[[i]] <- paste(colnames(newgrid), as.character(newgrid[i,]), collapse=";")
  }

  runs <- nrow(newgrid)
  baseheurs <- list()


  ## EVALUATING HYPERPARAMETER SPACE, THAT IS HYPERPARAMETER GRID

  print(newgrid)

  trials <- foreach::foreach(j=1:trials, .packages=c('preprocomb')) %dopar% {

      for (u in 1:runs) # looping all rows in the hyperparameter grid
      {

        a <- metaheur(gridclassobject, iterations=iterations, initialtemperature=newgrid$initialtemperature[u],
                      tempconst=newgrid$tempconst[u], taboolistlength=newgrid$taboolistlength[u], reheat=newgrid$reheat[u],
                      late=newgrid$late[u], stopcond=1, stopvalue=0.99, cores=cores, nholdout=nholdout, model=model)

        baseheurs[[u]] <- a

      }

  ## FINALIZING RESULTS

  # Fill NA to the end of set iterations if stopping condition rearched

  histofrun <- lapply(baseheurs, function(x) getiterationhistory(x))
  res <- fillna(histofrun)

  endtime <- Sys.time()
  walltime <- as.integer(difftime(endtime, starttime, units="mins"))

  new("metaheurhyper", res=res, namegrid=namegrid, baseheurs=baseheurs, walltime=walltime)

  } # END OF FOREARC


} # END OF FUNCTION


fillna <- function(temp){
  max.length <- max(sapply(temp, length))
  l <- lapply(temp, function(v) { c(v, rep(NA, max.length-length(v)))})
  res <- t(do.call(rbind, l))
  row.names(res) <- seq(1,nrow(res),1)
  return(res)
}





