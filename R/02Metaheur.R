
#' metaheur
#'
#' Metaheuristic optimization of preprocessing combinations
#'
#' @param gridclassobject (GridClass) created by setgrid function in preprocomb package
#' @param startgrid (integer) 0 random restart (default), 1 grid restart
#' @param startnum (integer) number of restarts
#' @param iterations (integer) number of iterations done for a restart
#' @param taboolistlength (integer) number of previous solution that can not be revisited, > 1
#' @param initialtemperature (numeric) initial propability for acccepting an inferior candidate, between 0 and 1
#' @param tempconst (numeric) multiplier for decreasing temperature on each iteration
#' @param reheat (numeric) propability of increasing temperature on each iteration
#' @param nholdout (integer) number of holdout rounds
#' @param late (integer) location of previous best solution a candidate is compared to, default to 0 for last
#' @param stopcond (integer) type of stopping condition, default to 1 for threshold, 2 for convergence
#' @param stopvalue (numeric) threshold for stopping, defaults to 0.95
#' @param deltafive (numeric) convergence criteria for last five iteration, defaults to 0.05
#' @param verbose (boolean)
#' @examples ## result <- metaheur(examplegrid,verbose=TRUE)
#' @return (list) best combination, history
#' @export

metaheur <- function(gridclassobject, startgrid=0, startnum=1, iterations=10, taboolistlength=1,
                                    initialtemperature=0.01, tempconst=0.01, reheat=0.01, nholdout=1, late=0,
                                    stopcond=1, stopvalue=0.95, deltafive=0.05, verbose=FALSE){

  if (verbose==TRUE){
  if (startgrid==0){print("Start type: random restarts.")}
  if (startgrid==1){print("Start type: grid restarts.")}
  }

  if (verbose==TRUE) {cat("Number of restarts:", startnum, "\n")}

  # initializations
  predictors <- "knn"
  grid <- gridclassobject
  fitControl <- trainControl(method="boot", number=2, savePredictions=TRUE)
  a <- function(x) {factor(unlist(unique(x)))}
  b <- lapply(grid@grid, a)
  reslist <- list()
  reshistory <- list()
  r <- numeric()
  s2 <- numeric()

  if (startgrid==0) {gridsequence <- sample(1:nrow(grid@grid),startnum)} else
    {gridsequence <- seq(1, nrow(grid@grid), by=ceiling(nrow(grid@grid)/startnum))}



  # INITIALIZATION PROCEDURE

counter2 <- 1

for (k  in gridsequence) # random restart loop
{
  taboo <- list()
  history <- numeric()
  temperature <- initialtemperature

  if (verbose==TRUE) {cat("Start combination:", k, "\n")}

  s <- sample(1:nrow(grid@grid),1) # random start
  dat <- grid@data[[s]]
  dat1 <- data.frame(y=dat@y, x=dat@x)
  for (l in 1:nholdout){
  s2 <- preprocomb::getprogrammaticprediction(dat1, predictors, fitControl)[1]
  }
  s2 <- mean(s2)
  counter <- 1


  for (j in 1:iterations)

  {

    if (verbose==TRUE) {cat("Iteration:", j, "Current best:", unlist(grid@grid[s,]), s2, "\n")}

    scopy <- grid@grid[s,]

    if (counter==1) {
      taboo <- append(taboo, list(scopy))
      history <- c(history, s2)
    }

    # MODIFICATION PROCEDURE

    # select randomly one phase and one preprocessor

    set.seed(as.numeric(Sys.time()))

    repeat{


      candidate_phase <- sample(1:ncol(grid@grid),1)
      candidate_preprocessor <- sample(1:length(b[[candidate_phase]]), 1)

      candidate_preprocessor <- unlist(grid@grid[candidate_preprocessor, candidate_phase])
      candidate_new <- scopy
      candidate_new[,candidate_phase] <- candidate_preprocessor

      # FIX HERE: ADD LENGTH OF TABOO LIST: HOW MANY LAST ARE CONSIDERED:

      condition1 <- lapply(tail(taboo,taboolistlength), function(x) identical(unname(unlist(candidate_new)),unname(unlist(x))))
      condition2 <- all(unlist(condition1)==FALSE)

      #print(condition2)



      if(condition2==TRUE) {
        break}

    }



    taboo <- append(taboo, list(candidate_new)) # adding valid candidate to taboo list

    # finding the dataset that corresponds to candidate_new

    res <- logical()
    for (i in 1:nrow(grid@grid))
         {
         a1 <- unname(unlist(grid@grid[i,]))
         b1 <- as.character(unlist(candidate_new[1,]))
         res[i] <- (identical(a1,b1))
         }

    temp <- which(res==TRUE)

    dat <- grid@data[[temp]]
    dat1 <- data.frame(y=dat@y, x=dat@x)

    # ASSESSMENT PROCEDURE
    for (l in 1:nholdout){
    r[l] <- preprocomb::getprogrammaticprediction(dat1, predictors, fitControl)[1]
    }
    r <- mean(r)

    if (verbose==TRUE) {cat("Iteration:", j, "Candidate:", unlist(grid@grid[temp,]), r, "\n")}

    # SELECTION COMPONENT

    temperature <- temperature*tempconst

    # REHEATING CLASS
    reheatprop <- sample(0:1, prob=c(1-reheat, reheat))
    if (reheatprop==1) {
      if (verbose==TRUE) {cat("Reheating occured.", "\n")}
      temperatureincrease <- (1-temperature)*runif(1,0,1)
      temperature <- temperature + temperatureincrease}

    if (verbose==TRUE){cat("Temperature:", temperature, "\n")}

    ## Hill-Climbing/ Late acceptance Hill-Climbing

    # establish comparison point
    historycondition <- (length(history)-late) <= 0
    if (historycondition==TRUE){historycomparison <- history[1]} else {
      historycomparison <- history[length(history)-late]
    }
    if (verbose==TRUE) {cat("Comparison value for late acceptance:", historycomparison, "\n")}

    if (r > historycomparison) {
      s2 <- r
      s <- temp}

    # Simulated annealing
    else {
      prop <- sample(0:1, prob=c(1-temperature, temperature))
      if (prop==1)
      {s2 <- r
       s <- temp
       if (verbose==TRUE) {cat("SA: A weaker solution was accepted.","\n")}
        }
    }

    history <- c(history, s2)
    counter <- counter + 1

    historydelta <- mean(tail(diff(history),3))
    if (verbose==TRUE) {cat("History delta, last three:", historydelta, "\n")}

    ## STOP CLASS

    if (stopcond==1){
      if (s2 > stopvalue)
        {cat("Iteration stop condition reseached.", "\n")
        break}
    }
    if (stopcond==2){
      if (historydelta < deltafive ) {cat("Iteration convergence condition reseached.", "\n")
        break}
    }


  } # loop exit

  reslist[[counter2]] <- list(combination=grid@grid[s,], accuracy=s2)
  reshistory[[counter2]] <- history

  counter2 <- counter2 + 1

}

best <- which.max(unlist(lapply(reslist, function(x) x$accuracy)))
best <- data.frame(data.frame(unlist(reslist[[best]]$combination)), accuracy=reslist[[best]]$accuracy)
return(list(best, reshistory, reslist))

} # function exit





#' metaheurcompare
#'
#' Compare parameter settings
#'
#' @param gridclassobject (GridClass) created by setgrid function in preprocomb package, defaults to examplegrid
#' @param runs (integer) number of runs, defaults to two
#' @param iterations (integer) number of iterations done for a restart
#' @param taboolistlength (integer vector) number of previous solution that can not be revisited, > 1
#' @param initialtemperature (numeric vector) initial propability for acccepting an inferior candidate, between 0 and 1
#' @param tempconst (numeric vector) multiplier for decreasing temperature on each iteration
#' @param reheat (numeric vector) propability of increasing temperature on each iteration
#' @param late (integer vector) location of previous best solution a candidate is compared to, default to 0 for last
#' @examples ## result <- metaheur(examplegrid,verbose=TRUE)
#' @export


metaheurcompare <- function(gridclassobject=examplegrid, runs=2, iterations=10, initialtemperature=c(0.01,1), tempconst=c(1, 0.85),
                            reheat=c(0.01, 0.1), taboolistlength=c(1,1), late=c(0,1)){

output <- list()
bestofrun <- numeric()
histofrun <- list()

for (u in 1:runs)
{
a <- metaheur(gridclassobject, iterations=iterations, initialtemperature=initialtemperature[u],
                             tempconst=tempconst[u], taboolistlength=taboolistlength[u], reheat=reheat[u], late=late[u], stopvalue=0.99, verbose=FALSE)
b <- a[[3]]
d <- max(unlist(lapply(b, function(x) x$accuracy)))
bestofrun[u] <- d
histofrun[[u]] <- unlist(a[[2]])
}
res <- t(plyr::ldply(histofrun, rbind))
}

#test <- metaheurcompare(taboolistlength = c(1,3))

#' plotsearchpath
#'
#' plot the searh path
#'
#' @param x output of metaheurcompare function
#' @export

plotsearchpath <- function(x){
  data <- reshape2::melt(x)
  g1 <- ggplot(data, aes(x=Var1, y=value)) + geom_line() + facet_grid(Var2 ~ .) + theme_bw()
  g1
}

#' plotdensity
#'
#' plot the density of classification accuracies
#'
#' @param x output of metaheurcompare function
#' @export

plotdensity <- function(x){
  data <- reshape2::melt(x)
  g2 <- ggplot(data, aes(x=value)) + geom_density() + facet_grid(Var2 ~ .) + theme_bw()
  g2
}
















