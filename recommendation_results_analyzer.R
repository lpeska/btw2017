library(StatRank)


setwd("C:/BTW2017/R_SourceCodes")

evalRow <- function(dtRow){
  pos <- dtRow[3]
  pos <- as.integer(unlist(pos))
  #print(pos)
  tot <- dtRow[4] 
  if(!is.na(pos)){
    rAt5Array <- ifelse(pos>5, 0, 1)
    rAt10Array <- ifelse(pos>10, 0, 1)
    rAt50Array <- ifelse(pos>50, 0, 1)
    if(pos <= 1){
      ndcg <- 1
    }else{
      ndcg <- Evaluation.NDCG(seq(1,pos), c(1,rep(0,(pos-1))) )
    }
    
    c(rAt5Array,rAt10Array,rAt50Array,ndcg)
  }else{
    c(NA,NA,NA,NA)
  }  

}

eval <- function(data){
  res <- apply(data,1,evalRow)
  
  rAt5 <- mean(res[1,],na.rm = TRUE)
  rAt10 <- mean(res[2,],na.rm = TRUE)
  rAt50 <- mean(res[3,],na.rm = TRUE)
  ndcg <- mean(res[4,],na.rm = TRUE)
  
  c(rAt5,rAt10,rAt50,ndcg)
}



printTable <- function(dataNames,algorithm)
{
  name <- c()
  ndcg <- c()
  recAt5 <- c()
  recAt10 <- c()
  recAt50 <- c()
  
	for(dtnm in dataNames){
		tab <- read.table(paste("resultsRec/",dtnm,"_",algorithm,".csv", sep=""), header=TRUE, sep=";")
    res<-eval(tab)

    name <- c(name, paste(algorithm,dtnm, sep="_"))
    recAt5 <- c(recAt5,res[1])
    recAt10 <- c(recAt10,res[2])
    recAt50 <- c(recAt50,res[3])
    ndcg <- c(ndcg,res[4])
	}
  
	df <- data.frame(name,recAt5, recAt10,recAt50,ndcg)
	#browser()
  print(df)
}

printTable(c("ecdf_dataset2","ecdf_dataset3","ecdf_dataset4","ecdf_dataset5"
             ),"vsm")
printTable(c("ecdf_dataset2","ecdf_dataset3","ecdf_dataset4","ecdf_dataset5","ecdf_dataset6","ecdf_dataset7"),"popVSM")

printTable(c("linReg_dataset2","linReg_dataset3","linReg_dataset4","linReg_dataset5","linReg_dataset6","linReg_dataset7"),"vsm")

printTable(c("linReg_dataset2","linReg_dataset3","linReg_dataset4","linReg_dataset5","linReg_dataset6","linReg_dataset7"),"popVSM")

printTable(c("scaled_dataset2","scaled_dataset3","scaled_dataset4","scaled_dataset5","scaled_dataset6","scaled_dataset7"),"vsm")

printTable(c("scaled_dataset2","scaled_dataset3","scaled_dataset4","scaled_dataset5","scaled_dataset6","scaled_dataset7"),"popVSM")


printTable(c("dataset1","j48_dataset2C","j48_dataset3C","j48_dataset4C","j48_dataset5C","j48_dataset6C","j48_dataset7C"),"popVSM")
printTable(c("dataset1","j48_dataset2C","j48_dataset3C","j48_dataset4C","j48_dataset5C","j48_dataset6C","j48_dataset7C"),"vsm")
printTable(c("dataset1","j48_dataset2C","j48_dataset3C","j48_dataset4C","j48_dataset5C","j48_dataset6C","j48_dataset7C"),"pop")

printTable(c("ecdf_dataset2"
            ),"pop")


binomialTest <- function(n,s) {
	p = 0
	for (k in s:n) { 
    	p <- p+choose(n,k)*0.5^n
	}
	p
}


compareRecAtK <- function(dataName1, dataName2,k){
  p<-paste(dataName1," vs.", dataName2)
  print(p)
  tab1 <- read.table(paste("resultsRec/",dataName1,".csv", sep=""), header=TRUE, sep=";")
  tab2 <- read.table(paste("resultsRec/",dataName2,".csv", sep=""), header=TRUE, sep=";")
  
  results <- data.frame(tab1$position , tab2$position)
  colnames(results) <- c("pos1", "pos2")
  results$pos1 <- ifelse(results$pos1<=k,1,0)
  results$pos2 <- ifelse(results$pos2<=k,1,0)
  
  firstBetter <- length( which(results$pos1 > results$pos2) )
  secondBetter <- length( which(results$pos1 < results$pos2) )
  different <- firstBetter + secondBetter
  
  
  print(c(different,firstBetter,secondBetter))
  print(
    c(
      First_better=binomialTest(different,firstBetter), 
      Second_better=binomialTest(different,secondBetter) 
    )
  )	
}

compareRecAtK("j48_dataset4C_popVSM","dataset1_popVSM",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset3C_popVSM",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset5C_popVSM",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset7C_popVSM",10)

compareRecAtK("j48_dataset4C_popVSM","dataset1_pop",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset3C_pop",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset4C_pop",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset5C_pop",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset7C_pop",10)

compareRecAtK("j48_dataset4C_popVSM","dataset1_vsm",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset3C_vsm",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset4C_vsm",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset5C_vsm",10)
compareRecAtK("j48_dataset4C_popVSM","j48_dataset7C_vsm",10)


compareRecAtK("j48_dataset4C_popVSM","dataset1_popVSM",10)
compareRecAtK("j48_dataset4C_popVSM","ecdf_dataset3_popVSM",10)
compareRecAtK("j48_dataset4C_popVSM","ecdf_dataset4_popVSM",10)
compareRecAtK("j48_dataset4C_popVSM","ecdf_dataset5_popVSM",10)
compareRecAtK("j48_dataset4C_popVSM","ecdf_dataset7_popVSM",10)
