library("httr")
library("readxl")
library("ggplot2")
library(tidyr)

#get and clean the data from the data.world site
NabData<-function(){
  GET("https://query.data.world/s/jdu6rivyl4ca7x7mp5ct5ns3fsm6o5", write_disk(tf <- tempfile(fileext = ".xlsx")))
  amazonData <- read_excel(tf)
  
  #Converts column names into R manageable strings
  #turns money data into numeric values
  colnames(amazonData)[(2:3)]<-c("Revenue_Millions", "Income_Millions")
  amazonData[, 2:3] <- lapply(amazonData[,2:3], as.character)
  amazonData[,2:3] <- lapply(amazonData[,c(2:3)], function(x) substring(x, 2))
  amazonData[,2:3] <- lapply(amazonData[,2:3], function(x) as.numeric(gsub(",","",x)))
  
  write.csv(amazonData, "AmazonMoney.csv", row.names=F)
}

#easy csv access that also converts the dates
GrabData<-function(){
  data <- read.csv("AmazonMoney.csv",header=TRUE,sep=",")
  data$Quarter <- as.Date(strptime(data$Quarter, format="%Y-%m-%d"))
  
  data
}

#function to mock up the area between the revenue and profit
AreaChart1<-function(){
  data <- GrabData()

  plot <- ggplot(data, aes(x=Quarter, y=Revenue_Millions))+geom_ribbon(aes(ymin=Income_Millions, ymax=Revenue_Millions))
  print(plot)
}

#keeps the bottom flat and charts all costs
AreaChart2<-function(){
  data <- GrabData()
  data$Costs<-data$Revenue_Millions-data$Income_Millions
  
  plot <- ggplot(data, aes(x=Quarter, y=Costs))+geom_area()
  print(plot)
}

#not totally accurate because profit is part of revenue
StackedArea<-function(){
  data <- GrabData()
  data <- gather(data, "Income_Type", "Value", -Quarter)

  bottomHeavy <- ggplot(data, aes(x=Quarter, y=Value, fill=Income_Type))+geom_area()
  print(bottomHeavy)
  
  data$Income_Type <- factor(data$Income_Type, levels=c("Revenue_Millions", "Income_Millions"))
  
  topHeavy <- ggplot(data, aes(x=Quarter, y=Value, fill=Income_Type))+geom_area()
  print(topHeavy)
}




