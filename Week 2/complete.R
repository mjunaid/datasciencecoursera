complete <- function(directory, id = 1:332){
        completeData = data.frame(id=integer(),
                                  nobs=integer())
        for (i in id){
                temp = data.frame(id=i,nobs=countNumberOfCompletedCases(readFileToDataFrame(createFileName(directory,i))))
                completeData = rbind(completeData,temp)
        }
        completeData
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