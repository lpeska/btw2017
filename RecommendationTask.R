#library(doParallel)
library(lsa)
library(recosystem)
library(plyr)
#registerDoParallel(6)


set.seed(2016)

setwd("C:/BTW2017/R_SourceCodes")



objectsPopularity <- function(trainSet){
  #trainSet <- trainSet[(trainSet$type == "obj"),]
  
  #oP <- aggregate((share * pred) ~ OID, data=trainSet, FUN=sum)
  oP <- aggregate((pred) ~ OID, data=trainSet, FUN=sum)
  colnames(oP) <- c("OID", "pred")
  unvisited <- knownObjects$oid[which(!knownObjects$oid %in% oP$OID)]
  
  allObjectsPopularity <- data.frame(oid = c(oP$OID, unvisited), popularity = c(oP$pred,rep(1e-10,length(unvisited))) )
  allObjectsPopularity                               
}

categoriesSimilarity <- function(extendedTrain){
  if(file.exists(paste("source/catSim",i,".csv", sep="_"))){
    catGrid <- read.table( file = paste("source/catSim",i,".csv", sep="_"), header=TRUE, sep=";")
  }else{
    extendedTrain <- extendedTrain[(extendedTrain$type == "obj"),]
    catList <- unique(catSource$category)
    catGrid <- expand.grid(catList, catList)
    jc <- c()
    for(j in 1:nrow(catGrid)) {
  
            catA <- catGrid$Var1[j]
            catB <- catGrid$Var2[j]
                    
            uCatA <- unique(extendedTrain$UID[which(extendedTrain$category == catA)])
            uCatB <- unique(extendedTrain$UID[which(extendedTrain$category == catB)])
            
            intAB <- intersect(uCatA,uCatB)
            uniAB <- union(uCatA,uCatB)
            if(length(uniAB)>0){
              jaccard <- length(intAB) / length(uniAB)
            }else{
              jaccard <- 0
            }
            jc <- c(jc,jaccard)
          }
    
    catGrid$jc <- jc
    colnames(catGrid) <- c("cat1","cat2","jc")
    
    write.table(catGrid , file = paste("source/catSim",i,".csv", sep="_"), append = FALSE,
                row.names=FALSE, na="",col.names=TRUE, sep=";")
  }
  
  catGrid
}

trainPopularSimCat <- function(extTrUser){
  #objects popularity (with/without cat_share) - done
  #!!categories similarity!!
  #user profile: visited categories
  
  prof <- merge(extTrUser,catSim, by.x = "category", by.y = "cat1")
  
  
  if(algRec == "popSimCat"){
    userProfile <- aggregate( (pred * jc) ~ cat2, data=prof, FUN=sum)
  }else if(algRec == "popSimCatCS"){
    userProfile <- aggregate( (pred * jc * share) ~ cat2, data=prof, FUN=sum)
  }else if(algRec == "popSimCatTemp"){
    userProfile <- aggregate( (pred * jc * sidFraction) ~ cat2, data=prof, FUN=sum)
  }else if( algRec == "popSimCatCSTemp"){
    userProfile <- aggregate( (pred * jc * share * sidFraction) ~ cat2, data=prof, FUN=sum)
    #userProfile <- aggregate( (pred * value * (1/sqrt(sidToLast+1))) ~ cat2, data=prof, FUN=sum)
  }
  
  colnames(userProfile) <- c("cat","value")
  userProfile
  
}

trainMF <- function(trainFactor){
  trainMF <- data_memory(as.numeric(trainFactor$uid), as.numeric(trainFactor$oid), trainFactor$rating,index1 = TRUE)
  
  r = Reco()
  optRes <- r$tune(trainMF, opts = list(
    dim      = c(10, 20, 50),
    costp_l1 = c(0, 0.1),                               
    costp_l2 = c(0.01),                               
    costq_l1 = c(0, 0.1),                               
    costq_l2 = c(0.01),                               
    lrate    = c(0.01, 0.1),
    niter    = 100,
    nfold    = 3,
    nthread  = 2,
    nmf      = TRUE
  ))
  print(Sys.time())
  print(optRes$min)
  
  r$train(trainMF, opts = c(optRes$min, nthread = 2, niter = 100, nmf = TRUE))
  
  r
}





trainMLMethod <- function(trainSet){
  #zde uz je trainset vc. negativnich prikladu
  
  #trenovani lokalnich modelu na zaklade atributu objektu
  #udelame to jako regresi, kde cilovy atribut bude predikce z predchoziho kroku
  #nahodne se prida take nekolik objektu, ktere uzivatel nenavstivil s predikci 0
  #asi spis na zakladnich datasetech, nez na binarizovanych. Nebo binarizovat set-attributes?
  #je mozne aplikovat podobne jako u VSM nasobeni share/temporal, ale tady je to mozna trochu kontraproduktivni...
  
  #merge data
  tsCropped <- data.frame(OID = trainSet$OID, pred = trainSet$pred)
  tsMerged <- merge(tsCropped, objectAttributeMatrix, by.x="OID", by.y="oid")
  
  
  library(caret)
  tcSim <- trainControl(method = "none")
  
  tcReg <- trainControl(method = "cv",
                        number = 3 ,
                        verboseIter = FALSE
  )


  
  if(algRec == "bagEarthGCV"){ #MARS
    library(earth)
    grid <- expand.grid(degree=c(1))

    met <- "Rsquared"
    mod <- train(pred ~ ., data = tsMerged,
                 method = algRec,
                 trControl = tcSim,
                 metric=met,
                 tuneGrid=grid
    )  
  }else if(algRec == "blackboost"){ #ADA boost with trees
    library(party)
    library(mboost)
    
    grid <- expand.grid(mstop=c(25,50),maxdepth=c(2,3))

    met <- "RMSE"
    mod <- train(pred ~ ., data = tsMerged,
                 method = algRec,
                 trControl = tcReg,
                 metric=met,
                 tuneGrid=grid
    )
    
  }else if(algRec == "xgbLinear"){ #gradient boosting w. linear
    library(xgboost)
    
    met <- "RMSE"
    mod <- train(pred ~ ., data = tsMerged,
                 method = algRec,
                 trControl = tcReg,
                 metric=met,
                 tuneLength = 3
    )
    
  }
  
  mod
}



### train variants of VSM algorithm
trainVSM <- function(trainSet){
  features <- objectAttributes[(objectAttributes$oid %in% trainSet$OID),] 
  prof <- merge(trainSet, features, by.x="OID", by.y="oid")
  #########################################################################################################
  # category based items can be used either directly or with respect to its share within the category page
  #userProfile <- aggregate( (pred * value * share) ~ feature, data=prof, FUN=sum)
  if(algRec == "vsm" | algRec == "popVSM"| algRec == "popAddVSM"){
    userProfile <- aggregate( (pred * value) ~ feature, data=prof, FUN=sum)
  }else if(algRec == "vsmCS" | algRec == "popVSMCS"){
    userProfile <- aggregate( (pred * value * share) ~ feature, data=prof, FUN=sum)
  }else if(algRec == "vsmTemp" | algRec == "popVSMtemp"){
    userProfile <- aggregate( (pred * value * sidFraction) ~ feature, data=prof, FUN=sum)
  }else if(algRec == "vsmCSTemp" | algRec == "popVSMCSTemp"){
    userProfile <- aggregate( (pred * value * share * sidFraction) ~ feature, data=prof, FUN=sum)
    #userProfile <- aggregate( (pred * value * (1/sqrt(sidToLast+1))) ~ feature, data=prof, FUN=sum)
    # Temporal fraction decreases importance of the object with distance from the last session (can be absolute or relative)
  }
  #########################################################################################################
  colnames(userProfile) <- c("feature","value")
  userProfile
}


###evaluate VSM algorithm
evalVSM <- function(userProfile){
  res <- lapply(knownObjects$oid, FUN = evalVsmObject, uP = userProfile)
  resFull<- data.frame(oid = knownObjects$oid)
  resFull$value <- res
  resFull$value <- unlist(resFull$value)
  arrange(resFull,desc(value))
  
  
}
###evaluate single object by VSM (cosine similarity)
evalVsmObject<- function(x, uP){
  if(nrow(uP)>0){
    oP <- objectAttributes[(objectAttributes$oid == x),] 
    v <- merge(uP, oP, by.x = "feature",by.y = "feature", all=TRUE)
    v[is.na(v)] <- 0
    c <- cosine(v$value.x, v$value.y)
  }else{
    c <- matrix(c(0))
  }
  
  #multiply by popularity
  if(algRec == "popVSM" | algRec == "popVSMCS" | algRec == "popVSMtemp" | algRec == "popVSMCStemp"){
    c <- c * objPop$popularity[(objPop$oid == x)]
  }
  
  #additive popularity model, can be parametrized in the future
  if(algRec == "popAddVSM"){
    c <- c + (objPop$popularity[(objPop$oid == x)] / max(bjPop$popularity))
  }
  as.numeric(c[1,1])
  
}

###evaluate Matrix Factorization Method
evalMF <- function(r){
  userLevel <- which(levels(trainFactor$uid) == as.character(u) ) 
  #objectsLevel <- which(levels(trainFactor$oid) == as.character(testCurrentUser$OID) ) 
  
  if(length(userLevel)>0){
    mfTest <- data.frame(uid = as.numeric(c(rep(userLevel,length(levels(trainFactor$oid))))), 
                         oid = as.numeric(seq(from=1, to=length(levels(trainFactor$oid)), by=1 )) )
    
    mft <- data_memory(mfTest$uid, mfTest$oid,index1 = TRUE)
    resMF <- r$predict(mft, out_memory())
    res <- data.frame(oid = as.numeric(levels(trainFactor$oid)), value = resMF)
  }else{
    #uknown user
    res <- data.frame(oid = objPop$oid, value = objPop$popularity)
  }
  
  arrange(res,desc(value))
}

###Evaluate popSimCat methods
evalPopularSimcat <- function(userProfile){
  if(nrow(userProfile)>0){
    extendedUP <- merge(userProfile, catSource, by.x="cat", by.y="category")
    objects <- data.frame(oid = knownObjects$oid, initVal = rep(1e-10, nrow(knownObjects)) )
  
    allOBJ <- merge(merge(objects,extendedUP,by = "oid", all.x = TRUE), objPop, by="oid")
    allOBJ$value[(is.na(allOBJ$value))] <- 0
    allOBJ$value <- (allOBJ$value+allOBJ$initVal)*allOBJ$popularity
    
    res <- data.frame(oid = allOBJ$oid, value = allOBJ$value)
    res <- arrange(res,desc(value))
    res <- res[!duplicated(res$oid),]
    
  }else{
    res <- data.frame(oid = objPop$oid, value = objPop$popularity)
    res <- arrange(res,desc(value))
  }
  res
  
  
  
}




###Evaluate Machine Learning method through caret
evalMLMethod <- function(mod){
 # print(mod)
  if(typeof(mod) == "logical"){
    resFull <- data.frame(oid = objPop$oid, value = objPop$popularity)
  }else{
    pom <- data.frame(OID = knownObjects$oid)
    testData <- merge(pom, objectAttributeMatrix, by.x="OID", by.y="oid")
    res <-predict(mod , newdata = testData)
  
    resFull<- data.frame(oid = knownObjects$oid)
    resFull$value <- res
    resFull$value <- unlist(resFull$value)
  }  
  arrange(resFull,desc(value))
}

###evaluate results
evaluateResults <- function(prediction, testedObjects){
  if(nrow(trSetUser)>0){
    n = "ok"
  }else{
    n = "noTrain"
  }
  position <- which(prediction$oid %in% testedObjects$OID)
  evalRes <- data.frame(
    uid = rep(testedObjects$UID[1], length(position)), 
    oid = prediction$oid[which(prediction$oid %in% testedObjects$OID)], 
    position,
    note = rep(n, length(position))
    )
  evalRes
}


objectAttributes <-  read.table("source/objectsBinaryAttributes.csv", header=TRUE, sep=",")
objectAttributeMatrix <-  read.table("source/ObjectAttributes.csv", header=TRUE, sep=";")
knownObjects <- data.frame(unique(objectAttributes$oid))
colnames(knownObjects)<- c("oid")





algs <- c( "vsm","popVSM")

datasets = c(

"dataset1",
  
"linReg_dataset2",
"linReg_dataset3",
"linReg_dataset4",
"linReg_dataset5",
"linReg_dataset6",
"linReg_dataset7",

"scaled_dataset2",
"scaled_dataset3",
"scaled_dataset4",
"scaled_dataset5",
"scaled_dataset6",
"scaled_dataset7",

"j48_dataset2C",
"j48_dataset3C",
"j48_dataset4C",
"j48_dataset5C",
"j48_dataset6C",
"j48_dataset7C",

"ecdf_dataset2",
"ecdf_dataset3",
"ecdf_dataset4",
"ecdf_dataset5",
"ecdf_dataset6",
"ecdf_dataset7"
)

algs <- c( "pop")

datasets = c(
  
  "dataset1",
  
  "j48_dataset2C",
  "j48_dataset3C",
  "j48_dataset4C",
  "j48_dataset5C",
  "j48_dataset6C",
  "j48_dataset7C"
)



for(dt in datasets){
  for(algRec in algs){

    
    #create results table
    write.table(data.frame(a=c("UID"),b=c("OID"),c=c("position"),d=c("note")) , file = paste("resultsRec/",dt,"_",algRec,".csv", sep=""), append = FALSE,
                row.names=FALSE, na="",col.names=FALSE, sep=";")
    
    data <- read.table(paste("purchasePredictions/",dt,".csv", sep=""), header=TRUE, sep=";")
    colnames(data)[3]<- "pred"
    data.purchases <- subset(data, res > 0)
    
    #for all cross-validation sets = for all LOOCV purchased objects
    for (i in 1:nrow(data.purchases)){
      test <- data.purchases[i, ]
      uid <- test[1]
      oid <- test[2]
      
      trainAll <- subset(data, !(data$uid %in% uid[1] & data$oid %in% oid[1]))
      trSetUser <- subset(trainAll, (trainAll$uid %in% uid[1]))
      colnames(test) <- c("UID", "OID", "pred", "res")
      colnames(trainAll) <- c("UID", "OID", "pred", "res")
      colnames(trSetUser) <- c("UID", "OID", "pred", "res")
      
      #zachovam pouze radky o kterych mam i atributy objektu
      trainAll <- trainAll[(trainAll$OID %in% knownObjects$oid),]
      trSetUser <- trSetUser[(trSetUser$OID %in% knownObjects$oid),]
      test <- test[(test$OID %in% knownObjects$oid),]
      
      #predict only for users wit non-empty train and test sets
      if(nrow(test)>0 & nrow(trSetUser)>0){
        
        #learn model on train data / collaborative, hybrid models
        objPop <- objectsPopularity(trainAll)
        
        if(algRec == "popSimCat" | algRec == "popSimCatCS" | algRec == "popSimCatTemp" | algRec == "popSimCatCSTemp"){
          catSource <- read.table("source/ObjectCategories.csv", header=TRUE, sep=";")
          extendedTrain <- merge(trainAll, catSource, by.x="OID", by.y="oid")
          catSim <- categoriesSimilarity(extendedTrain)
        }
        
        
        #learn model on train data / cb models
        if(algRec == "vsm" | algRec == "vsmCS" | algRec == "popVSM" |  algRec == "popVSMCS"| algRec == "vsmTemp" | algRec == "popVSMtemp" | algRec == "vsmCSTemp" | algRec == "popVSMCSTemp"  ){
          mod <- trainVSM(trSetUser)
        }else if(algRec == "popSimCat" | algRec == "popSimCatCS" | algRec == "popSimCatTemp" | algRec == "popSimCatCSTemp"){
          extTrUser <- extendedTrain[(extendedTrain$UID == u),]
          mod <- trainPopularSimCat(extTrUser)
        }else if(algRec == "blackboost" | algRec == "bagEarthGCV" | algRec == "xgbLinear"){
          sample <- train[sample(nrow(train), round(log2(nrow(trSetUser)+2)*10) ), ]
          sample <- sample[!(sample$UID == u),]
          sample$pred <- 0
          
          mod <- trainMLMethod( rbind(trSetUser,sample) )
        }
        
        
        
        #predict
        if(algRec == "vsm" | algRec == "vsmCS" | algRec == "popVSM" |  algRec == "popVSMCS"| algRec == "vsmTemp" | algRec == "popVSMtemp" | algRec == "vsmCSTemp" | algRec == "popVSMCSTemp"){
          res <- evalVSM(mod)
        }else if(algRec == "popSimCat" | algRec == "popSimCatCS" | algRec == "popSimCatTemp" | algRec == "popSimCatCSTemp"){
          res <- evalPopularSimcat(mod)  
        }else if(algRec == "mf"){
          res <- evalMF(mod)
        }else if(algRec == "blackboost" | algRec == "bagEarthGCV" | algRec == "xgbLinear" ){
          res <- evalMLMethod(mod)
          
        }else if(algRec == "pop"){
          #simply recommend the most popular objects
          res <- data.frame(oid = objPop$oid, value = objPop$popularity)
          res <- arrange(res,desc(value))
        }
        
        
        #evaluate prediction
        evalRes <- evaluateResults(res, test)
        
        #store results
        write.table(evalRes, file = paste("resultsRec/",dt,"_",algRec,".csv", sep=""), append = TRUE,
                    row.names=FALSE, na="",col.names=FALSE, sep=";")
        
        print( paste("User:", uid, ", object:", oid,"id:",i, dt, algRec ))
      }
    }
  }
}




