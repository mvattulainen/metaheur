
#' @import ggplot2
NULL

#' @import foreach
NULL

#' @importFrom methods new
NULL

#' @importFrom stats median runif
NULL

#' @importFrom utils tail head
NULL

#' preprocessing combinations example
#'
#' An example grid object made of modified Iris-data with package preprocomb. \cr
#' Contains 540 preprocessing combinations and corresponding preprocessed data sets. \cr
#' Can be used with metaheur() or metaheurhyper() functions as an example grid.
#'
#' modifiediris <- droplevels(iris[-c(1:60),]) \cr
#' examplegrid <- preprocomb::setgrid(phases=c("scaling", "smoothing", "outliers", "selection", "sampling"), data=modifiediris) \cr
#' @format A GridClass object
"examplegrid"

#' metaheuristic optimization example
#'
#' An example of metaheuristic search with examplegrid. \cr
#' 50 iterations and 10 times repeated holdout validation.
#'
#' examplesearch <- metaheur(examplegrid, iterations=50, nholdout=10, cores=2) \cr
#' @format A metaheur class object
"examplesearch"


#' hyperparameter optimization example
#'
#' An example of metaheuristic hyperparameter optimization. Can be plotted
#' with plotsearchpath(examplehyperparam).
#'
#' examplehyperparam <- metaheurgrid(cores=2, trials=10, iterations=50, nholdout=4) \cr
#' NOTE: the above uses examplegrid as default grid. Computation time 3,5 hours with \cr
#' Intel Celeron 1.4 Ghz, 2 cores \cr
#' @format list
"examplehyperparam"

## SUPPORTING FUNCTIONS

getuniquepreprocessors <- function(x) {factor(unlist(unique(x)))}

## GLOBAL VARIABLES

globalVariables(c("value","examplegrid", "Iteration"))


