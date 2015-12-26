
#' @import ggplot2
NULL

#' @importFrom utils tail
NULL

modifiediris <- droplevels(iris[-c(1:60),])
examplegrid <- preprocomb::setgrid(phases=c("scaling", "smoothing", "outliers", "selection", "sampling"), data=modifiediris)

globalVariables(c("value","Var1", "trainControl"))

