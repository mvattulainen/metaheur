getbestparameters <- function(x){

  ## FIND THE BEST OF THE MEANS OF RUNS

  resulthistory <- lapply(x, function(x) x@res)
  #firstresult <- lapply(resulthistory, function(z) apply(z, 2, function(z) head(z,1)))
  lastresult <- lapply(resulthistory, function(z) apply(z, 2, nonNAtail))
  #lasttofirst <- mapply("/", lastresult, firstresult, SIMPLIFY = FALSE)

  walltime <- lapply(x, function(x) x@walltime)
  walltime <- sum(unlist(walltime))

  e <- t(data.frame(lastresult)) # data frame of last result rows, rows represent trials
  bestmeanofruns <- apply(e, 2, mean)
  bestmeans <- which.max(bestmeanofruns)
  bestmeanvalue <- round(bestmeanofruns[bestmeans],2)

  y <- lapply(resulthistory, function(x) x[,bestmeans])
  lastresult2 <- unlist(lapply(y, nonNAtail))

  bestscenario <- which.max(lastresult2)
  worstscenario <- which.min(lastresult2)
  medianscenario <-  which.min(abs(lastresult2 - median(lastresult2)))

  c(bestmeans, bestscenario, worstscenario,medianscenario, bestmeanvalue, walltime)

}

# GET THE LAST NO-NA VALUE

nonNAtail <- function(x){
  nonnas <- which(!is.na(x))
  res <- tail(x[nonnas],1)
}

getbestparametername <- function(x){
  name <- x[[1]]@namegrid[[getbestparameters(x)[1]]]
}


# PREPARE DATA FOR PLOTTING

prepareplotdata <- function(x){

  if (class(x)=="list") {

    bestmean <- getbestparameters(x)[1]
    bestscenario <- getbestparameters(x)[2]
    worstscenario <- getbestparameters(x)[3]
    medianscenario <- getbestparameters(x)[4]
    accuracy <- getbestparameters(x)[5]
    walltime <- getbestparameters(x)[6]

    bestmeans <- data.frame(lapply(x, function(x) x@res[,bestmean]))
    colnames(bestmeans) <- seq(1,length(x))
    colnames(bestmeans)[bestscenario] <- "Best run"
    colnames(bestmeans)[worstscenario] <- "Worst run"
    colnames(bestmeans)[medianscenario] <- "Median run"

    data <- bestmeans[,c(bestscenario, worstscenario, medianscenario)]

    data$Iteration <- seq(1,nrow(x[[1]]@res))

    plotdata <- reshape2::melt(data, id.vars=c("Iteration"))

    name <- paste("Accuracy:",accuracy,"(",walltime,"mins)", getbestparametername(x))

  }

  if (class(x)=="metaheur") {

    tempdata <- data.frame(fillna(x@iterationhistory))
    colnames(tempdata) <- paste("start", seq(1,ncol(tempdata)))
    tempdata$Iteration <- seq(1,nrow(tempdata))
    plotdata <- reshape2::melt(tempdata, id.vars="Iteration")

    name <- getbestheur(x)[[1]]
    accuracy <- round(getbestheur(x)[[2]],2)
    walltime <- x@walltime
    solution <- paste(names(name), unname(unlist(name[1,])), sep=":",collapse=";")
    name <- paste("Accuracy:",accuracy," (",walltime,"mins) ", sep="")

  }

  list(plotdata, name)
}

#' plot search path
#'
#' Plots the search path, i.e. classification accuracy by iteration.
#' metaheur class plots (output of metaheur function) can include restarts.
#' List of metaheurhyper class objects (output of metaheurhyper function) plot
#' includes best, median and worst scenarious.
#' @param x (metaheur object or list of metaheurgrid class objects) object to be plotted
#' @examples
#' plotsearchpath(examplesearch)
#' plotsearchpath(examplehyperparam)
#' @export

plotsearchpath <- function(x){

  params <- prepareplotdata(x)
  plotdata <- params[[1]]
  paramname <- params[[2]]

  g1 <- ggplot(plotdata, aes(x=Iteration, y=value)) + geom_line() + facet_wrap(~ variable) + theme_bw() + ggtitle(paramname)
  g1 <- g1 + theme(plot.title = element_text(size = 10))
  g1
}











