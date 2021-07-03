pollutantmean <- function(pollutant = "sulfate",id = 1:332){
  name <- character(length = length(id))
  means <- numeric(length = length(id))
  j <- 0
  for(i in id){
    j <- j+1
    if(i<10){
      name[j]<-(paste("00",i,".csv",sep =""))
    }
    if(i>=10 & i<100){
      name[j]<-(paste("0",i,".csv",sep = ""))
    }
    if(i>=100){
      name[j]<-(paste(i,".csv",sep = ""))
    }
  }
  for(i in seq_along(name)){
    t<-read.csv(name[i])
    if(pollutant == "sulfate"){
      t1 <- t[, 2]
    }
    else{
      t1 <- t[, 3]
    }
    means[i] <- (mean(t1, na.rm=TRUE))
  }
  print("Means among id")
  print(mean(means[!is.nan(means)]))
  print(data.frame(name,means))
}
