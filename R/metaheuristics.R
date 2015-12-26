Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

getprogrammaticprediction <- function(preprocesseddataset, predictors, fitControl){

  tryCatch({

    training <- caret::createDataPartition(preprocesseddataset$y, times=1, list=FALSE, p=0.66)[,1]

    intrain <- preprocesseddataset[training,]
    intest <- preprocesseddataset[-training,]

    model_list <- caretEnsemble::caretList(y ~., data=intrain, methodList=predictors, trControl=fitControl)
    prediction <- as.data.frame(predict(model_list, newdata=intest))
    prediction$vote <- apply(prediction, 1, Mode)
    output <- as.numeric(lapply(prediction, function(x) mean(as.character(x)==as.character(intest$y))))

  }, error= function(e) return(NA) )

}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#modifiediris <- droplevels(iris[-c(1:60),])
#grid <- setgrid(phases=c("scaling", "smoothing", "outliers", "selection", "sampling"), data=modifiediris)


metaheuristicevaluation <- function(gridclassobject, startgrid=0, startnum=1, iterations=320, taboolistlength=1,
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
  fitControl <- caret::trainControl(method="boot", number=2, savePredictions=TRUE)
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
  s2 <- getprogrammaticprediction(dat1, predictors, fitControl)[1]
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
    r[l] <- getprogrammaticprediction(dat1, predictors, fitControl)[1]
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

a <- metaheuristicevaluation(grid_t, startgrid=0, startnum=1, iterations=5, verbose=FALSE)

a_i <- metaheuristicevaluation(grid_i, iterations=320)
a_m <- metaheuristicevaluation(grid_m, iterations=36)
a_t <- metaheuristicevaluation(grid_t, iterations=110)
a_s <- metaheuristicevaluation(grid_s, iterations=11)
a_p <- metaheuristicevaluation(grid_p, iterations=65)
a_l <- metaheuristicevaluation(grid_l, iterations=65)

## PLOTTER

b <- unlist(a[[2]])
plot(density(b))
boxplot(y=b, x=seq(1,31,1))
plot(y=b, x=seq(1,31,1), type="l")
plot(y=diff(b), x=seq(1,30,1), type="l")


## RUNNER

TODO: ADDITIONAL RUN LOOP, NOW ONE RUN PER CONFIGURATION

metaheurcompare <- function(gridclassobject, runs, iterations, initialtemperature, tempconst, taboolistlength){

gridclassobject <- grid_m
runs <- 2
iterations <- 30
initialtemperature <- c(0.01,1)
tempconst <- c(1, 0.85)
reheat <- c(0.01, 0.1)
taboolistlength <- c(1,1)


output <- list()
bestofrun <- numeric()
histofrun <- list()

for (u in 1:runs)
{
a <- metaheuristicevaluation(gridclassobject, startgrid=0, startnum=1, iterations=iterations, initialtemperature=initialtemperature[u],
                             tempconst=tempconst[u], taboolistlength=taboolistlength[u], reheat=reheat[u], verbose=FALSE)
b <- a[[3]]
d <- max(unlist(lapply(b, function(x) x$accuracy)))
bestofrun[u] <- d
histofrun[[u]] <- unlist(a[[2]])
}
histofrun <- data.frame(histofrun)
colnames(histofrun) <- seq(1,runs,1)
histofrun$index <- seq(1,iterations+1,1)
}

test <- metaheurcompare(grid_m, 2, 30, initialtemperature = c(0.1,1), tempconst = c(1, 0.85), taboolistlength = c(1,3))



## SAME SETTINGS, MULTIPLE RUNS

library(doSNOW)
library(foreach)
cl <- makeCluster(2, type="SOCK")
registerDoSNOW(cl)
pak <- c('kernlab')


  runs <- 4

  output <- list()
  bestofrun <- numeric()
  histofrun <- list()

  matriisi<-foreach(i=1:runs,.packages=pak, .multicombine=TRUE, .export=ls(envir=globalenv())) %dopar%
  {
    a <- metaheuristicevaluation(grid_s, startgrid=0, startnum=1, iterations=10, nholdout=2, verbose=TRUE)
    b <- a[[3]]
    d <- max(unlist(lapply(b, function(x) x$accuracy)))
    bestofrun <- d
    histofrun <- unlist(a[[2]])
    list(bestofrun, histofrun)
  }

  res <- unlist(lapply(matriisi, function(x) x[[2]]))
  histofrun <- data.frame(histofrun)
  colnames(histofrun) <- seq(1,runs,1)
  histofrun$index <- seq(1,iterations+1,1)





library(reshape2)
library(ggplot2)

metaheurplot <- function(x){
  data <- melt(x, id="index")
  g1 <- ggplot(data, aes(x=index, y=value)) + geom_line() + facet_grid(variable ~ .) + theme_bw()
  g2 <- ggplot(data, aes(x=value)) + geom_density() + facet_grid(variable ~ .) + theme_bw()
  multiplot(g1, g2, cols=2)
}
a <- metaheurplot(histofrun)



