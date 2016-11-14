library(doParallel)
registerDoParallel(5)
library(caret)

tC1 <- trainControl(method = "cv",
                    number = 5,
                    verboseIter = TRUE,
                    classProbs = TRUE
)


tC2 <- trainControl(method = "cv",
                      number = 5,
                      verboseIter = TRUE
)



tC3 <- trainControl(method = "cv",
                    number = 2,
                    verboseIter = FALSE
)


print(Sys.time())


dt <- function(datasetName){
  
  dataset <- read.table(datasetName, header=TRUE, sep=";")
  colnames(dataset)[3] <- "label"
  dataset
}

dtClass <- function(datasetName){
  
  dataset <- read.table(datasetName, header=TRUE, sep=";")
  colnames(dataset)[3] <- "label"
  #dataset <- preProcess(dataset, method = c("center", "scale"))
  dataset$label<- as.factor(dataset$label)
  #print(levels(dataset$label))
  levels(dataset$label) <- c("no","yes")
  
  dataset
}



setwd("C:/BTW2017")

dataset1 <- dt("R_SourceCodes/datasets/inputSet1.csv")
dataset2 <- dt("R_SourceCodes/datasets/inputSet2.csv")
dataset3 <- dt("R_SourceCodes/datasets/inputSet3.csv")
dataset4 <- dt("R_SourceCodes/datasets/inputSet4.csv")
dataset5 <- dt("R_SourceCodes/datasets/inputSet5.csv")
dataset6 <- dt("R_SourceCodes/datasets/inputSet6.csv")
dataset7 <- dt("R_SourceCodes/datasets/inputSet7.csv")


dataset1C <- dtClass("R_SourceCodes/datasets/inputSet1.csv")
dataset2C <- dtClass("R_SourceCodes/datasets/inputSet2.csv")
dataset3C <- dtClass("R_SourceCodes/datasets/inputSet3.csv")
dataset4C <- dtClass("R_SourceCodes/datasets/inputSet4.csv")
dataset5C <- dtClass("R_SourceCodes/datasets/inputSet5.csv")
dataset6C <- dtClass("R_SourceCodes/datasets/inputSet6.csv")
dataset7C <- dtClass("R_SourceCodes/datasets/inputSet7.csv")

dataset1.purchases <- subset(dataset1, label > 0)
distUIDs <- unique(dataset1.purchases$uid)


datasets <- c(
  "dataset2", "dataset3", "dataset4", "dataset5", "dataset6", "dataset7"
)

datasetsClass <- c(
  "dataset2C","dataset3C", "dataset4C", "dataset5C", "dataset6C", "dataset7C"
)


#######################################################Simple Models###################################################

###############################################################Normalized/CDF-based Additive Model ############################################
for (dtName in datasets){
  dataset <- eval(parse(text=dtName))
  print(dtName)
  print(Sys.time())
  
  drops <- c("uid","oid","label")
  dataset.substr  <- dataset[ , !(names(dataset) %in% drops)]
  dataset.scaled <- as.data.frame(scale(dataset.substr))
  dataset.ecdfFunction <-  sapply(dataset.substr, ecdf )
  dataset.ecdfValues <-  as.data.frame(mapply( do.call, dataset.ecdfFunction, lapply( dataset.substr, list)))
  pred.scaled <- apply(dataset.scaled,1,mean,na.rm=TRUE)
  pred.ecdf <- apply(dataset.ecdfValues,1,mean,na.rm=TRUE)
  dataset$predScaled <- pred.scaled 
  dataset$predEcdf <- pred.ecdf 
  
  uids <- c()
  oids <- c()
  predScaled <- c()
  predEcdf <- c()
  res <- c()
  predTable.ecdf <- data.frame()
  predTable.scaled <- data.frame()
  iterations <-c()
  
  for (i in distUIDs) {
    test <- dataset[(dataset$uid %in% i),]	
    test <- subset(test , select = -c(dataset$uid) )

    results <-test$label
    users <- test$uid
    obj <- test$oid
    prS <- test$predScaled
    prE <- test$predEcdf

    
    uids <- append(uids,users )
    predScaled <- append(predScaled,prS )
    predEcdf <- append(predEcdf,prE )
    res <- append(res,results )
    oids <- append(oids,obj )    
  }
  predTable.ecdf <- data.frame(uids,oids,predEcdf,res)
  predTable.scaled <- data.frame(uids,oids,predScaled,res)
  
  write.table(predTable.ecdf, file = paste("R_SourceCodes/purchasePredictions/ecdf_",dtName,".csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")
  write.table(predTable.scaled, file = paste("R_SourceCodes/purchasePredictions/scaled_",dtName,".csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")  
}  

#######################################################Classification###################################################

###############################################################J48 ############################################

for (dtName in datasetsClass){
  dataset <- eval(parse(text=dtName))
  print(dtName)
  print(Sys.time())
  
  set.seed(2016)
  uids <- c()
  pred <- c()
  oids <- c()
  res <- c()
  predTable <- data.frame()
  CList <-c()
  grid <- expand.grid(C=c(0.01,0.1,0.25,0.5))
  
  k <- 0 

  for (i in distUIDs) {
    k <- k+1
    print(k)
    print(Sys.time())
    train <- dataset[!(dataset$uid %in% i),-(1:2)] 
    train <- subset(train, select = -c(dataset$uid) )
    test <- dataset[(dataset$uid %in% i),]	
    test <- subset(test , select = -c(dataset$uid) )
    
    
    mod <- train(label ~ ., data = train,
                 method = "J48",
                 trControl = tC2,
                 metric="Kappa",
                 tuneGrid = grid
    )
    
    predictionPROB <-predict(mod , newdata =  test[,-(1:2)], type = "prob")
    results <-test$label
    users <- test$uid
    obj <- test$oid
    csizes <- rep(mod$bestTune[1,"C"], each=length(users))
    
    uids <- append(uids,users )
    pred <- append(pred,predictionPROB$yes )
    res <- append(res,results )
    oids <- append(oids,obj )
    CList <- append(CList,csizes )
    #plot(mod)
    #plot(mod$finalModel)
    #browser()
    
    
  }
  predTable <- data.frame(uids,oids,pred,res,CList)
  predTable$res <- predTable$res - 1 
  
  write.table(predTable, file = paste("R_SourceCodes/purchasePredictions/j48_",dtName,".csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")
  
  
}



##################################################################Regression###################################################
multiplication_positive_factor = 10
###############################################################ADA boost with LinReg ############################################


for (dtName in datasets){
  dataset <- eval(parse(text=dtName))
  print(dtName)
  print(Sys.time())
  
  set.seed(2016)
  grid <- expand.grid(mstop=c(10,20,50),nu=c(1))
  uids <- c()
  oids <- c()
  pred <- c()
  res <- c()
  predProbYes <- c()
  predTable <- data.frame()
  iterations <-c()
  
  
  k <- 0 
  
  for (i in distUIDs) {
    k <- k+1
    print(k)
    print(Sys.time())
    train <- dataset[!(dataset$uid %in% i),] 
    train <- subset(train, select = -c(dataset$uid) )
    test <- dataset[(dataset$uid %in% i),]	
    test <- subset(test , select = -c(dataset$uid) )
    
    mod <- train(label ~ ., data = train,
                 method = "BstLm",
                 trControl = tC2,
                 metric="Rsquared",
                 tuneGrid = grid
    )
    prediction <-predict(mod , newdata = test)
    results <-test$label
    users <- test$uid
    obj <- test$oid
    tsizes <- rep(mod$bestTune[1,"mstop"], each=length(users))
    
    uids <- append(uids,users )
    oids <- append(oids,obj )
    pred <- append(pred,prediction )
    res <- append(res,results )
    iterations <- append(iterations,tsizes ) 
    
  }
  
  predTable <- data.frame(uids,oids,pred,res,iterations)
  write.table(predTable, file = paste("R_SourceCodes/purchasePredictions/adaLM_",dtName,".csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")
  
}


###############################################################Lin Reg ############################################

multiplication_positive_factor = 10

for (dtName in datasets){
  dataset <- eval(parse(text=dtName))
  print(dtName)
  print(Sys.time())

set.seed(2016)
uids <- c()
oids <- c()
pred <- c()
res <- c()
predTable <- data.frame()
k <- 0 

dataset$label = dataset$label * multiplication_positive_factor 

for (i in distUIDs) {
  k <- k+1
  print(k)
  print(Sys.time())
	train <- dataset[!(dataset$uid %in% i),-(1:2)] 
	train <- subset(train, select = -c(dataset$uid) )
	test <- dataset[(dataset$uid %in% i),]	
	test <- subset(test , select = -c(dataset$uid) )


 	mod <- train(label ~ ., data = train,
             method = "lm",
             trControl = tC3,
	       metric="Rsquared",
	       tuneLength = 1
	)

	prediction <-predict(mod , newdata = test[,-(1:2)])
	results <-test$label
	users <- test$uid
	obj <- test$oid
	
	uids <- append(uids,users )
	oids <- append(oids,obj )
	pred <- append(pred,prediction )
	res <- append(res,results )
	
	print(mod$finalModel$coefficients)
	
}
predTable <- data.frame(uids,oids,pred,res)
write.table(predTable, file = paste("R_SourceCodes/purchasePredictions/linReg_",dtName,".csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")


}





############################################################### Lasso ############################################

for (dtName in datasets){
  dataset <- eval(parse(text=dtName))
  print(dtName)
  print(Sys.time())
set.seed(2016)
uids <- c()
oids <- c()
pred <- c()
res <- c()
predTable <- data.frame()
fraction <-c()



k <- 0 

for (i in distUIDs) {
  k <- k+1
  print(k)
  print(Sys.time())
  train <- dataset[!(dataset$uid %in% i),] 
  train <- subset(train, select = -c(dataset$uid) )
  test <- dataset[(dataset$uid %in% i),]	
  test <- subset(test , select = -c(dataset$uid) )
  
  
  
  mod <- train(label ~ ., data = train,
               method = "lasso",
               trControl = tC2,
               metric="Rsquared",
               tuneLength = 5
  )
  prediction <-predict(mod , newdata = test)
  results <-test$label
  users <- test$uid
  obj <- test$oid
  fsizes <- rep(mod$bestTune[1,"fraction"], each=length(users))
  
  uids <- append(uids,users )
  oids <- append(oids,obj )
  pred <- append(pred,prediction )
  res <- append(res,results )
  fraction <- append(fraction,fsizes ) 
  

}

predTable <- data.frame(uids,oids,pred,res,fraction)
write.table(predTable, file = paste("R_SourceCodes/purchasePredictions/lasso_",dtName,".csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")


}















