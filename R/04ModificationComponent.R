
## GET NEW CANDIDATE SOLUTION THAT MEETS TABOOLIST CRITERIA

getnewcandidate <- function(grid, taboo, taboolistlength, uniquepreprocessors, copyofcurrentbest){

  repeatcounter <- 0

set.seed(as.numeric(Sys.time()))
repeat{

  # sample a random phase (phase number) and a random preproccessor (preprocessor number)

  candidate_phase <- sample(1:ncol(grid@grid),1)
  #candidate_preprocessor <- sample(1:length(uniquepreprocessors[[candidate_phase]]), 1)

  ## Experimental

  currentpreprocessorinphase <- unlist(copyofcurrentbest[,candidate_phase])
  allpreprocessorsinphase <- as.character(unlist(uniquepreprocessors[[candidate_phase]]))
  leftpreprocessorsinphase <- allpreprocessorsinphase[-match(currentpreprocessorinphase, allpreprocessorsinphase)]
  candidate_preprocessor <- sample(leftpreprocessorsinphase,1)

  #candidate_preprocessor <- unlist(grid@grid[candidate_preprocessor, candidate_phase])

  candidate_new <- copyofcurrentbest

  # place new random preprocessing technique to the random phase of the current best solution

  candidate_new[,candidate_phase] <- candidate_preprocessor

  # test that the new candidate is NOT in the taboolist
  # at minimum it can not be the current best solution, corresponding to taboo list length 1

  condition1 <- lapply(utils::tail(taboo,taboolistlength), function(x) identical(unname(unlist(candidate_new)),unname(unlist(x))))
  condition2 <- all(unlist(condition1)==FALSE)

  repeatcounter <- repeatcounter +1

  if (repeatcounter > 100 ) {stop("The system was not able to find a solution candidate that is not in the taboo list")}

  if(condition2==TRUE) {
    break}
}

  return(candidate_new)
}





