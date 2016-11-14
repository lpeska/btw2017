setwd("C:/BTW2017")

getContextFactors <- function(context){
  ctBP <- quantile(context, probs=c(0,0.25,0.5,0.75,1), names=FALSE)
  ctBP[1] <- -Inf
  ctBP[length(ctBP)] <- Inf
  
  contextFactors <- unique(ctBP)
  contextFactors
}

getContextWeight <- function(variable, context, breakPoints){
  contextFactors <- cut(context, breaks=unique(breakPoints))
  df <- data.frame(variable,contextFactors)
  bpValues <- aggregate(variable~contextFactors, FUN=mean)
  weight <- sd(bpValues$variable)
  weight  
}

baselinePredictor<- function(variable, context, breakPoints){
  
  contextFactors <- cut(context, breaks=unique(breakPoints))
  df <- data.frame(variable,contextFactors)
  df$id  <- 1:nrow(df)
  bpValues <- aggregate(variable~contextFactors, FUN=mean)
  total <- merge(df,bpValues,by="contextFactors")
  total <- total[order(total$id), ]
  total$variable.y
  #browser()
}


fullUserModel <- read.table("R_SourceCodes/source/finalUserModel.csv", header=TRUE, sep=";")
fullUserModel <- subset(fullUserModel, type=="obj")

fullUserModel$visArea <- ifelse( fullUserModel$visArea > 1, 1,  fullUserModel$visArea )
fullUserModel$scrolledArea <- ifelse( fullUserModel$scrolledArea > 1, 1,  fullUserModel$scrolledArea )
fullUserModel$scrolledArea <- ifelse( fullUserModel$visArea > 0.9, 0,  (fullUserModel$scrolledArea - fullUserModel$visArea)/(1-fullUserModel$visArea)  )

fullUserModel.purchases <- subset(fullUserModel, purchase > 0)
fullUserModel.noPurchases <- subset(fullUserModel, purchase <= 0 )
trainSetCandidateUsers <- unique(fullUserModel.purchases$UID)



baselinePredictorsContext <- data.frame(
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews))
)

baselinePredictorsWeightedContext <- data.frame(
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews)),
  rep(0,length(fullUserModel$pageViews))
)


colnames(baselinePredictorsContext)<- c("pageViews", "dwellTime", "mmTime", "mmDistance", "scrTime", "scrDistance", "clicksCount", "scrolledArea", "hitBottom")
colnames(baselinePredictorsWeightedContext)<- colnames(baselinePredictorsContext)

listOfContext = c("UID", "OID","links", "images", "textSize", "pageDim", "visArea", "handheldDevice")
listOfVars = c("pageViews", "dwellTime", "mmTime", "mmDistance", "scrTime", "scrDistance", "clicksCount", "scrolledArea", "hitBottom")

ncols = length(listOfContext)*length(listOfVars)
bpList = matrix( rep(0,length(fullUserModel$pageViews)*ncols), 
                 nrow=length(fullUserModel$pageViews), 
                 ncol=ncols) 
resFeedbackList = matrix( rep(0,length(fullUserModel$pageViews)*ncols), 
                 nrow=length(fullUserModel$pageViews), 
                 ncol=ncols) 
weightsList = matrix( rep(0,length(fullUserModel$pageViews)*ncols), 
                      nrow=length(fullUserModel$pageViews), 
                      ncol=ncols) 

###########################################evaluating baseline predictors#########################
 i <- 1
for(v in listOfVars) {
  sumWeights <- 0
  listOfWeights <- c()
  
  for(cx in listOfContext) {
    print(c(v,cx)) 
    
    varTrain <- fullUserModel[[v]]
    contextTrain <- fullUserModel[[cx]]
    
    if(cx != "UID" & cx != "OID"){
      contextFactors <- getContextFactors(contextTrain)
    }else{
      contextFactors <- contextTrain
      contextFactors<- c(-Inf,contextFactors,Inf)
    }
    
    weight <- getContextWeight(varTrain, contextTrain, contextFactors)
    bpTrain <- baselinePredictor(varTrain, contextTrain, contextFactors)
    
    sumWeights <- sumWeights + weight
    baselinePredictorsContext[[v]] <- baselinePredictorsContext[[v]] +  bpTrain
    baselinePredictorsWeightedContext[[v]] <- baselinePredictorsWeightedContext[[v]] + (weight * bpTrain)
    listOfWeights <- c(listOfWeights,weight)
    bpList[,i] <- bpTrain
    resFeedbackList[,i] <- varTrain-bpTrain
    weightsList[,i] <- weight
    #browser()
    i <- i+1
  }
  # print(listOfWeights)
  
  baselinePredictorsContext[[v]] <- baselinePredictorsContext[[v]] / ncols
  baselinePredictorsWeightedContext[[v]] <- baselinePredictorsWeightedContext[[v]] / sumWeights
  #browser()
}


resFeedbackDataFrame <- as.data.frame(resFeedbackList)
colnames(resFeedbackDataFrame) <- c("pv_UID","pv_OID","pv_link","pv_img","pv_text","pv_dim","pv_visAr","pv_hhd",
                                    "dt_UID","dt_OID","dt_link","dt_img","dt_text","dt_dim","dt_visAr","dt_hhd",
                                    "mT_UID","mT_OID","mT_link","mT_img","mT_text","mT_dim","mT_visAr","mT_hhd",
                                    "mD_UID","mD_OID","mD_link","mD_img","mD_text","mD_dim","mD_visAr","mD_hhd",
                                    "sT_UID","sT_OID","sT_link","sT_img","sT_text","sT_dim","sT_visAr","sT_hhd",
                                    "sD_UID","sD_OID","sD_link","sD_img","sD_text","sD_dim","sD_visAr","sD_hhd",
                                    "cc_UID","cc_OID","cc_link","cc_img","cc_text","cc_dim","cc_visAr","cc_hhd",
                                    "sA_UID","sA_OID","sA_link","sA_img","sA_text","sA_dim","sA_visAr","sA_hhd",
                                    "hb_UID","hb_OID","hb_link","hb_img","hb_text","hb_dim","hb_visAr","hb_hhd")

colnames(weightsList) <- colnames(resFeedbackDataFrame)


#########################baseline data sets - binary##########################
trSet1 <- data.frame(
  fullUserModel$UID,
  fullUserModel$OID,
  fullUserModel$purchase,
  rep(1,length(fullUserModel$id))  
)
colnames(trSet1) <- c("uid","oid", "purchase","views")
write.table(trSet1, file = paste("R_SourceCodes/datasets/inputSet1.csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")

#########################baseline data sets - dwellTime##########################
trSet2 <- data.frame(
  fullUserModel$UID,
  fullUserModel$OID,
  fullUserModel$purchase,
  fullUserModel$dwellTime
)
colnames(trSet2) <- c("uid","oid", "purchase", "dwellTime")
write.table(trSet2, file = paste("R_SourceCodes/datasets/inputSet2.csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")

#########################raw feedback data##########################
trSet3 <- data.frame(
  fullUserModel$UID,
  fullUserModel$OID,
  fullUserModel$purchase,
  fullUserModel$pageViews,
  fullUserModel$dwellTime,
  fullUserModel$mmTime,
  fullUserModel$mmDistance,
  fullUserModel$scrTime,
  fullUserModel$scrDistance,
  fullUserModel$clicksCount
)
colnames(trSet3)<- c("uid","oid", "purchase", "pageViews", "dwellTime", "mmTime", "mmDistance", "scrTime", "scrDistance", "clicksCount")
write.table(trSet3, file = paste("R_SourceCodes/datasets/inputSet3.csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")

#########################raw feedback + raw context##########################
trSet4 <- data.frame(
  fullUserModel$UID,
  fullUserModel$OID,
  fullUserModel$purchase,
  fullUserModel$pageViews,
  fullUserModel$dwellTime,
  fullUserModel$mmTime,
  fullUserModel$mmDistance,
  fullUserModel$scrTime,
  fullUserModel$scrDistance,
  fullUserModel$clicksCount,
  fullUserModel$scrolledArea,
  fullUserModel$hitBottom,
  fullUserModel$links,
  fullUserModel$images,
  fullUserModel$textSize,
  fullUserModel$pageDim,
  fullUserModel$visArea,
  fullUserModel$handheldDevice
)
colnames(trSet4)<- c("uid","oid","purchase", "pageViews", "dwellTime", "mmTime", "mmDistance", "scrTime", "scrDistance", "clicksCount", "scrolledArea", "hitBottom",
                     "links","images","textSize","pageDim","visArea","handheldDevice")
write.table(trSet4, file = paste("R_SourceCodes/datasets/inputSet4.csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")


#########################processed feedback with context##########################
trSet5 <- data.frame(
  fullUserModel$UID,
  fullUserModel$OID,
  fullUserModel$purchase,
  (fullUserModel$pageViews - baselinePredictorsContext$pageViews),
  (fullUserModel$dwellTime - baselinePredictorsContext$dwellTime),
  (fullUserModel$mmTime - baselinePredictorsContext$mmTime),
  (fullUserModel$mmDistance - baselinePredictorsContext$mmDistance),
  (fullUserModel$scrTime - baselinePredictorsContext$scrTime),
  (fullUserModel$scrDistance - baselinePredictorsContext$scrDistance),
  (fullUserModel$clicksCount - baselinePredictorsContext$clicksCount),
  (fullUserModel$scrolledArea - baselinePredictorsContext$scrolledArea),
  (fullUserModel$hitBottom - baselinePredictorsContext$hitBottom)
)
colnames(trSet5)<- c("uid","oid", "purchase",  "pageViews", "dwellTime", "mmTime", "mmDistance", "scrTime", "scrDistance", "clicksCount", "scrolledArea", "hitBottom")
write.table(trSet5, file = paste("R_SourceCodes/datasets/inputSet5.csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")

#########################processed feedback with weighted context##########################
trSet6 <- data.frame(
  fullUserModel$UID,
  fullUserModel$OID,
  fullUserModel$purchase,
  (fullUserModel$pageViews - baselinePredictorsWeightedContext$pageViews),
  (fullUserModel$dwellTime - baselinePredictorsWeightedContext$dwellTime),
  (fullUserModel$mmTime - baselinePredictorsWeightedContext$mmTime),
  (fullUserModel$mmDistance - baselinePredictorsWeightedContext$mmDistance),
  (fullUserModel$scrTime - baselinePredictorsWeightedContext$scrTime),
  (fullUserModel$scrDistance - baselinePredictorsWeightedContext$scrDistance),
  (fullUserModel$clicksCount - baselinePredictorsWeightedContext$clicksCount),
  (fullUserModel$scrolledArea - baselinePredictorsWeightedContext$scrolledArea),
  (fullUserModel$hitBottom - baselinePredictorsWeightedContext$hitBottom)
  
)
colnames(trSet6)<- c("uid","oid", "purchase",  "pageViews", "dwellTime", "mmTime", "mmDistance", "scrTime", "scrDistance", "clicksCount", "scrolledArea", "hitBottom")
write.table(trSet6, file = paste("R_SourceCodes/datasets/inputSet6.csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")

#########################processed feedback with context one by one##########################
trSet7 <- data.frame(
  fullUserModel$UID,
  fullUserModel$OID,
  fullUserModel$purchase,
  resFeedbackDataFrame
)
colnames(trSet7)[1]<- "uid"
colnames(trSet7)[2]<- "oid"
colnames(trSet7)[3]<- "purchase"
write.table(trSet7, file = paste("R_SourceCodes/datasets/inputSet7.csv", sep=""),row.names=FALSE, na="",col.names=TRUE, sep=";")





