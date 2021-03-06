## Best is a function that takes in the State Code and the outcome as arguments
## Returns the best (lowest) 30-day mortality for the specified outcome

best <- function(state, outcome){
        outcomeData <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
        outcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        if(!(outcome %in% names(outcomes))){
                stop("invalid outcome")
        } else if(!(state %in% outcomeData[,7])){
                stop("invalid state")
        } else{
                condensedData = outcomeData[,c(2,7,outcomes[outcome])]
                names(condensedData)=c("Hospital","State",outcome)
                condensedData <- condensedData[complete.cases(condensedData),]
                stateData <- condensedData[condensedData$State == state, ]
                stateData[order(stateData$Hospital),]
                minOutcome <- min(stateData[,3])
                stateData[stateData[,outcome]==minOutcome,][1,1]
        }
        
        
}