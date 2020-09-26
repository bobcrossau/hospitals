best <- function(state, outcome) {
  setwd("D:/Documents/Academic/R/healthdata")
  possoutcomes <- c("heart attack", "heart failure", "pneumonia")
  checkoutcome <- match(outcome, possoutcomes, nomatch = 0)
  if(checkoutcome == 0) {  # check for valid outcome
    print("Invalid outcome")
    return() }
  if (outcome == "heart attack") { # assign column to outcome
    col <- 11 } 
  if (outcome == "heart failure") {
    col <- 17 }
  if (outcome == "pneumonia") {
    col <- 23 }
  # get data from "outcome-of-care-measures.csv"
  outcometable  <- read.csv("outcome-of-care-measures.csv") 
  statevector  <- lapply(outcometable[, 7], as.character) #state list
  checkstate <- match(state, statevector, nomatch = 0)
  if(checkstate == 0) { #check for valid state
    print("Invalid state")
    return() }
  # separate out state of interest
  section <- subset(outcometable, outcometable[, 7]== state)
  hospital <- as.vector(section[, 2])
  mortality <- as.numeric(section[, col])
  statetable <- data.frame(hospital, mortality) # build result
  sortedx <- statetable[order(mortality, hospital),] #sort result
  sorted <- na.omit(sortedx)        # remove NAs
  print(sorted[1, 1])     #print hospital at top of sorted list
}
