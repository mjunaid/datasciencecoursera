

rankhospital <- function(state,outcome,num = "best"){
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
                condensedData <- condensedData[condensedData$State == state, ]
                newData <- condensedData[order(condensedData[outcome],condensedData$Hospital),]
                if(num == "best"){
                newData[1,1]
                } else if(num== "worst"){
                        newData[nrow(newData),1]  
                } else
                        newData[num,1] 
        }
        
        
}