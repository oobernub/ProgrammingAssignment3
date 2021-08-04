##Ranking hospitals in all states


##The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
##containing the hospital in each state that has the ranking specified in num.

rankall <- function(outcome, num = "best") {
      data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                       na.strings = "Not Available", stringsAsFactors = FALSE)

      
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
      
##sort the data alphabetically by state abbreviation, and then by outcome rank, and name
      data1 <- data[order(data[,7],data[,colind],data[,2]),]
##Gets rid of extraneous columns so that we can use complete.cases
      data2 <- data1[,c(2,7,colind)]
##gets rid of NA from the ranking, so worst will give the lowest rank thats not NA
      data3 <- data2[complete.cases(data2),]
##split it by state
      data4 <- split(data3, data3$State)
##since i dont know how to change "best" argument to 1, ill just use if, then statement
      if (num =="best") {
            hospname <- lapply(data4, function(x) x[1,1])
            hospname2 <- unlist(hospname)
            output <- data.frame(hospital = hospname2, state = names(hospname), row.names = names(hospname))
      }
      
      else if(num == "worst"){
            hospname <- lapply(data4, function(x) x[nrow(x),1])
            hospname2 <- unlist(hospname)
            output <- data.frame(hospital = hospname2, state = names(hospname), row.names = names(hospname))
      }
      
      else {
      hospname <- lapply(data4, function(x) x[num,1])
      hospname2 <- unlist(hospname)
      output <- data.frame(hospital = hospname2, state = names(hospname), row.names = names(hospname))
      }
      
      output
      
}
