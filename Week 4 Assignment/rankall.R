rankall <-function(outcome, num = "best"){
        outcomeData <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
        outcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        if(!(outcome %in% names(outcomes))){
                stop("invalid outcome")
        } else{
                condensedData = outcomeData[,c(2,7,outcomes[outcome])]
                names(condensedData)=c("hospital","state",outcome)
                condensedData <- condensedData[complete.cases(condensedData),]
                newData <- condensedData[order(condensedData[outcome],condensedData$hospital),]
                newData
                if(num == "best"){
                        result = lapply(split(newData, newData$state),function(data) data[1,1])
                        list_Value = unlist(result)
                        list_State = names(result)
                        data.frame(hospital=list_Value,state = list_State, row.names = list_State)
                } else if(num== "worst"){
                        result =lapply(split(newData, newData$state),function(data) data[nrow(data),1])
                        list_Value = unlist(result)
                        list_State = names(result)
                        data.frame(hospital=list_Value,state = list_State, row.names = list_State)
                } else{
                        result = lapply(split(newData, newData$state),function(data) data[num,1])
                        list_Value = unlist(result)
                        list_State = names(result)
                        data.frame(hospital=list_Value,state = list_State, row.names = list_State)
                        }
                }
}