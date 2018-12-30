

best <- function(state, outcome) {
  #read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                   na.strings = "Not Available")
  data[,11] <- as.numeric(data[,11])
  data[,17] <- as.numeric(data[,17])
  data[,23] <- as.numeric(data[,23])
  
  #check validity of parameters
  if(!(state %in% data[,7])) {
    stop("invalid state")
  }
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  #return hospital name in that state with lowest 30-day death rate
  hospitals <- split(data, data$State)[[state]]
  
  if(outcome == "heart attack") {
    hospitals <- hospitals[
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==
        min(hospitals[, 11], na.rm = T), 2]
    hospitals <- sort(hospitals)
    return(hospitals[1])
  } else if(outcome == "heart failure") {
    hospitals <- hospitals[
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==
        min(hospitals[, 17], na.rm = T), 2]
    hospitals <- sort(hospitals)
    return(hospitals[1])
  } else if(outcome == "pneumonia") {
    hospitals <- hospitals[
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==
        min(hospitals[, 23], na.rm = T), 2]
    hospitals <- sort(hospitals)
    return(hospitals[1])
  }
}
rankhospital <- function(state, outcome, num = "best") {
  full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  column <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
  
  if (nrow(data_for_state) == 0) {
    stop("invalid state")	
  }
  
  data_for_state[,2] <- as.numeric(data_for_state[,2])
  ordered_data_for_state <- order(data_for_state[column], data_for_state$Hospital.Name, na.last=NA)
  
  if (num == "best") {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
  } else if (num == "worst") {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[length(ordered_data_for_state)]])
  } else if (is.numeric(num)) {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[num]])
  } else {
    stop("invalid num")
  }
}

rankall <- function(outcome, num="best") {
  
  outcome.names <- c("heart attack", "heart failure", "pneumonia")
  
  # check validity of outcome
  if (!outcome %in% outcome.names) {
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  # rename outcome columns for easy reference
  names(data)[c(11,17,23)] <- outcome.names
  
  # take just the columns we need, convert outcome column to numeric
  data <- data[,c("State","Hospital.Name",outcome)]
  data[,outcome] <- suppressWarnings(as.numeric(data[,outcome]))
  
  # get rid of rows where outcome is na
  data <- data[!is.na(data[outcome]),]
  
  # sort data by state name, then outcome, then hospital name
  data <- data[order(data$State, data[outcome], data$Hospital.Name),]
  
  # aggregate by state, choosing the row that corresponds to the rank num
  ranksbystate <- aggregate(data, by=list(data$State), function(x) {
    if (!is.numeric(num)) {
      if (num == "best") {
        num <- 1
      } else if (num == "worst") {
        num <- length(x)
      } else {
        stop("invalid num")
      } 
    }
    x[num]
  })
  
  # get just the columns we need and rename them
  out <- ranksbystate[,c(3,1)]
  names(out) <- c("hospital","state")
  
  return(out)
}