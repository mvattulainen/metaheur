#' An S4 class to represent metaheuristic optimization results
#'
#' @slot solutionlist (list) of best solutions, i.e. preprocessing combinations for each start
#' @slot iterationhistory (list) of iteration classification accuracies for each start
#' @slot logs (list) event log for each start
#' @slot walltime (integer) execution wall-clock time in minutes

setClass("metaheur", representation(solutionlist="list", iterationhistory="list", logs="list", walltime="integer"))

#' metaheuristic optimization of preprocessing combinations
#'
#' @param gridclassobject (GridClass) created by setgrid function in preprocomb package
#' @param startgrid (integer) 0 random restart (default), 1 grid restart
#' @param startnum (integer) number of restarts
#' @param iterations (integer) number of iterations done for a restart, defaults to 10
#' @param taboolistlength (integer) number of previous solution that can not be revisited, must be 1 or more
#' @param initialtemperature (numeric) initial propability for acccepting an inferior candidate, between 0 and 1
#' @param tempconst (numeric) multiplier for decreasing temperature on each iteration
#' @param reheat (numeric) propability of increasing temperature on each iteration
#' @param nholdout (integer) number of holdout rounds, defaults to 2
#' @param late (integer) location of previous best solution a candidate is compared to, defaults to 0 for last
#' @param stopcond (integer) type of stopping condition in addition to iterations, default to 1 for threshold, 2 for convergence
#' @param stopvalue (numeric) threshold for stopping, defaults to 0.99
#' @param deltafive (numeric) convergence criteria for last five iterations, defaults to 0.05
#' @param model (character) caret name of predictive model, defaults to "rpart"
#' @param cores (integer) number of cores used in computation of classification accuracy holdout rounds, defaults to 1
#' @examples ## result <- metaheur(examplegrid, startnum=2, nholdout=2, cores=2)
#' ## getbestheur(result)
#' @export

metaheur <- function(gridclassobject, startgrid=0, startnum=1, iterations=10, taboolistlength=1,
                                    initialtemperature=0.01, tempconst=0.01, reheat=0.01, nholdout=2, late=0,
                                    stopcond=1, stopvalue=0.99, deltafive=0.05, model="rpart", cores=1){

# TECHNICAL INITIALIZATION

  starttime <- Sys.time()

  doParallel::registerDoParallel(cores=cores)

  # initializations
  predictors <- model
  grid <- gridclassobject
  reslist <- list()
  reshistory <- list()
  r <- numeric()
  s2 <- numeric()
  logs <- list()

  # unique preprocesors in a phase
  uniquepreprocessors <- lapply(grid@grid, getuniquepreprocessors)

  if (startgrid==0) {gridsequence <- sample(1:nrow(grid@grid),startnum)} else
    {gridsequence <- seq(1, nrow(grid@grid), by=ceiling(nrow(grid@grid)/startnum))}



# RESTART STRUCTURE

counterforrestarts <- 1

for (k  in gridsequence)
{

  metaheurlog <- character(iterations*5)
  logcounter <- 1
  if (startgrid==0){metaheurlog[logcounter] <- "Start type: random restarts."}
  if (startgrid==1){metaheurlog[logcounter] <- "Start type: grid restarts."}
  logcounter <- logcounter + 1
  metaheurlog[logcounter] <- paste("Number of restarts:", startnum)

  taboo <- list()
  history <- numeric()
  temperature <- initialtemperature

  numberofcurrentbest <- sample(1:nrow(grid@grid),1) # random start

  logcounter <- logcounter + 1
  metaheurlog[logcounter] <- paste("Start combination:", numberofcurrentbest)

  currentbest_value <- getfirstassessment(numberofcurrentbest, grid, predictors, nholdout)

# ITERATION STRUCTURE WITHIN A RESTART

  for (j in 1:iterations)

  {

    # SOLUTION INITIALIZATION PROCEDURE

    copyofcurrentbest <- grid@grid[numberofcurrentbest,]

    logcounter <- logcounter + 1
    metaheurlog[logcounter] <- paste("Iteration:", j, "Current best:", paste(unlist(copyofcurrentbest),collapse=" "), round(currentbest_value,2))

    # setting taboo and history for first iteration
    if (j==1) {
      taboo <- append(taboo, list(copyofcurrentbest))
      history <- c(history, currentbest_value)
    }

    # MODIFICATION PROCEDURE

    candidate_new <- getnewcandidate(grid, taboo, taboolistlength, uniquepreprocessors, copyofcurrentbest)

    taboo <- append(taboo, list(candidate_new))

    # ASSESSMENT PROCEDURE

    candidatereturn <- getcandidatedata(grid, candidate_new)

    candidatedata <- candidatereturn[[1]]
    candidatenumber <- candidatereturn[[2]]

    candidate_value <- getconsequentassessment(candidatedata, predictors, nholdout)

    logcounter <- logcounter + 1
    metaheurlog[logcounter] <- paste("Iteration:", j, "Candidate:", paste(unlist(candidate_new), collapse=" "), round(candidate_value,2))

    # SELECTION PROCEDURE

    # Decreasing temperature
    temperature <- temperature*tempconst
    if (temperature < 0.00001) {temperature <- 0.00001}

    # Reheating
    reheatprobability <- sample(0:1, 1, prob=c(1-reheat, reheat))
    if (reheatprobability==1) {

      temperatureincrease <- (1-temperature)*stats::runif(1,0,1)

      logcounter <- logcounter + 1
      metaheurlog[logcounter] <- paste("Reheating temperature increase:", round(temperatureincrease,2))

      temperature <- temperature + temperatureincrease}

    logcounter <- logcounter + 1
    metaheurlog[logcounter] <- paste("Temperature:", round(temperature,2))

    # Late acceptance Hill-Climbing

    historycondition <- (length(history)-late) <= 0
    if (historycondition==TRUE){historycomparison <- history[1]} else { # first iteration
      historycomparison <- history[length(history)-late] # consequent iterations
    }

    logcounter <- logcounter + 1
    metaheurlog[logcounter] <- paste("Comparison value for late acceptance:", round(historycomparison,2))

    if (candidate_value > historycomparison) {
      currentbest_value <- candidate_value
      numberofcurrentbest <- candidatenumber}

    # Simulated annealing
    else {
      acceptinginferiorcandidate <- sample(0:1, 1, prob=c(1-temperature, temperature))
          if (acceptinginferiorcandidate==1)
          {currentbest_value <- candidate_value
          numberofcurrentbest <- candidatenumber

          logcounter <- logcounter + 1
          metaheurlog[logcounter] <- paste("SA: A weaker solution was accepted.")
        }
    }

    # Storing of

    history <- c(history, currentbest_value)
    historydelta <- mean(utils::tail(diff(history),5))

    logcounter <- logcounter + 1
    metaheurlog[logcounter] <- paste("History delta, last five:", round(historydelta,2))

    ## TERMINATION PROCEDURE

    if (stopcond==1){
      if (currentbest_value > stopvalue){

        logcounter <- logcounter + 1
        metaheurlog[logcounter] <- paste("Iteration stop condition reseached.")
        break}
    }
    if (stopcond==2){
      if (historydelta < deltafive ) {

        logcounter <- logcounter + 1
        metaheurlog[logcounter] <- paste("Iteration convergence condition rearched.")
        break}
    }


  } # ITERATION STRUCTURE EXIT

  reslist[[counterforrestarts]] <- list(combination=grid@grid[numberofcurrentbest,], accuracy=currentbest_value)
  reshistory[[counterforrestarts]] <- history

  logs[[counterforrestarts]] <- metaheurlog


  counterforrestarts <- counterforrestarts + 1



} # RESTART STRUCTURE EXIT

doParallel::stopImplicitCluster()

endtime <- Sys.time()
walltime <- as.integer(difftime(endtime, starttime, units="mins"))

new("metaheur", solutionlist=reslist, iterationhistory=reshistory, logs=logs, walltime=walltime)

} # FUNCTION EXIT

#' get the best preprocessing combination
#'
#' get the best combination and its classification accuracy
#'
#' @param x (metaheur class object) output of function metaheur()
#' @return (list) best combination, classification accuracy of the best combination
#' @examples
#' ##result <- metaheur(examplegrid)
#' ##getbestheur(result)
#' @export

getbestheur <- function(x){

  if(class(x)!="metaheur"){stop("Argument 'x' must be a metaheur class object.")}

  maxvalue <- max(unlist(lapply(x@iterationhistory, nonNAtail)))
  maxpos <- which.max(unlist(lapply(x@iterationhistory, nonNAtail)))
  maxsol <- x@solutionlist[[maxpos]][[1]]

  list(maxsol, maxvalue)
  }

getbestofruns <- function(x){
  max(unlist(lapply(x@iterationhistory, function(x) x)))
}

getiterationhistory <- function(x){
  unlist(x@iterationhistory)
}

#' get the logs of a metaheur search
#'
#' @param x (metaheur class object) output of function metaheur()
#' @param n (integer) number of log entries show, default to all entries
#' @examples
#' ##getlogs(examplesearch)
#' @export

getlogs <- function(x, n=NULL){

  if(class(x)!="metaheur"){stop("Argument 'x' must be a metaheur class object.")}

  logs <- x@logs[[1]]

  if(is.null(n)){
  logs <- head(logs,length(logs))
  }

  if(!is.null(n)){
    if ( n > length(logs)) {stop("Argument 'n' must be smaller than the length of a log.")}

  logs <- head(logs, n)
  }

  logs

  }

#' get the execution wall-clock time
#'
#' @param x (metaheur class object) output of function metaheur()
#' @examples
#' ##getwalltime(examplesearch)
#' @export

getwalltime <- function(x){

  if(class(x)!="metaheur"){stop("Argument 'x' must be a metaheur class object.")}

  x@walltime

}
