corr <-function(directory, threshold = 0){
        correlationVector = numeric()
        for (i in 1:332){
                loadedData <- readFileToDataFrame(createFileName(directory,i))
                if(sum(complete.cases(loadedData))>threshold){
                        completeData <- loadedData[complete.cases(loadedData), ] 
                        correlationVector = c(correlationVector,cor(as.vector(completeData[["sulfate"]]),as.vector(completeData[["nitrate"]])))
                }
        }
        correlationVector
}

countNumberOfCompletedCases <- function(dataFrameLoad){
        sum(complete.cases(dataFrameLoad))
}

createFileName <- function (directory , id){
        if(id<10){
                paste0(directory,"/00",id,".csv")
        } else if (id<100){
                paste0(directory,"/0",id,".csv")
        }else{
                paste0(directory,"/",id,".csv")
        }
}

readFileToDataFrame <- function (fileName){
        read.csv(fileName)
}