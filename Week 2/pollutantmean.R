pollutantmean <- function(directory, pollutant, id = 1:332){
        masterData = data.frame(Date=as.Date(character()),
                                sulfate=character(),
                                nitrate=character(),
                                id=integer())
    for (i in id){
          readFileToDataFrame(createFileName(directory,i)) 
          masterData = rbind(masterData,readFileToDataFrame(createFileName(directory,i)))
          
    }
        mean(masterData[[pollutant]],na.rm = TRUE)
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