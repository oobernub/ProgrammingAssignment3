##Part2Finding the best hospital in a state


##reads the file, na.strings so don't have to coerce the values to numeric
best <- function(state, outcome){
      data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                       na.strings = "Not Available", stringsAsFactors = FALSE)
      
  
##Test if its state passed as an argument is a state in the list, if not returns invalid      
      if(!is.element(state, data$State)){
            return("invalid state")
      } 
      
##Test if its valid outcome, and sets the col that we want based on the outcome
      if(outcome == "heart attack"){
            colind <- 11
            
      } else if(outcome == "heart failure"){
            colind <- 17
            
      } else if(outcome == "pneumonia"){
            colind <- 23
            
      } else { 
            return("invalid outcome")
      }

##output - gives a list of df by state
      sdata <- split(data, data$"State")
##makes a df out of the state passed through argument
      sdata1 <- as.data.frame(sdata[state])
##order the df based on the outcome argument
      ordsdata1 <- sdata1[order(sdata1[,colind],sdata1[,2]),]
##returns the name of the hospital ranked first.
      ordsdata1[1,2]
      
}