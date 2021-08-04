##Ranking hospitals by outcome in a state


##num is hospital ranking
rankhospital <- function(state, outcome, num = "best"){
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
##gets rid of extraneous columns so that we can use the complete.cases, (name, state, outcome of interest)
      ordsdata2 <- ordsdata1[,c(2,7,colind)]
##gets rid of NA from the ranking so that "worst" will give the lowest rank that is not NA 
      good <- complete.cases(ordsdata2)
      ordsdata3 <- ordsdata2[good,]
##returns the name of the hospital or rank passed through argument
      if(num == "best"){
            ordsdata3[1, 1] 
      }
      
      else if(num == "worst"){
            ordsdata3[nrow(ordsdata3), 1]
      }
      
      else { ordsdata3[num, 1]   
      }
      
      
}


