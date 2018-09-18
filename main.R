rm(list=ls())
#Please use utf-8 to open file

#Program setting
DEBUG = 1
LOG = 0

#Class ENV setting
CLASS_ROW = 4
CLASS_COL = 4
CLASS_SIZE = CLASS_ROW*CLASS_COL

#Student data
STUDENT_DATA_PATH = '.\\rdata.csv'

#Fitness weight setting
ST2_WEIGHT=0.2
ST3_WEIGHT=0.1

#GA setting
POPULATION_SIZE = 6 #世代大小(必須是偶數)
CROSS_OVER_LEN = 3 #交配長度
MUTATION_ENABLE = 1
MUTATION_VALUE = 75 #10=1% 1=0.1%
ELITE_ENABLE=1 #菁英政策 0=disable 1=enable 
DO_TIMES=5000

#------function------
#cat for debug mode
dlog<-function(...){
  if(DEBUG){
    cat(...)
  }
}

#
encode<-function(populationSize,row=CLASS_ROW,col=CLASS_COL){
  ##creat a random seat list
  seatList<-array(dim=row*col) #the result will in here
  seatListB<-array(dim=row*col)
  seatListC<-array(dim=row*col)
  ##inital array
  for(i in 1:(row*col)){
    seatList[i]=as.numeric(i)
    seatListB[i]=as.numeric(0)
    seatListC[i]=as.numeric(0)
  }
  
  i=1
  while(!isTRUE(all.equal(seatList,seatListC))){
    randomNum=floor(runif(1, min=1, max=(row*col)+1))
    if(seatList[randomNum]!=0){
      seatListB[i]=seatList[randomNum]
      seatList[randomNum]=0
      i=i+1
    }
  }
  return(seatList=seatListB)
}

#一維轉二維
decode<-function(seatList,row=CLASS_ROW,col=CLASS_ROW){
  seatListMatrix<-matrix(nrow=row,ncol=col,byrow=TRUE)#about whose seat
  k=0
  for(i in 1:row){
    for(j in 1:col){
      k=k+1
      seatListMatrix[i,j]=seatList[k]
    }
  }
  return(seatListMatrix)
}
#
makeChromosome<-function(populationSize=POPULATION_SIZE,row=CLASS_ROW,col=CLASS_COL){
  chromosome<-matrix(c(1:1),nrow=populationSize,ncol=col*row,byrow=TRUE)
  for(i in 1:populationSize){
    chromosome[i,]=encode()
  }
  return(chromosome)
}

caculateFitnessValue<-function(chromosome){
  matrix = decode(chromosome)
  for(i in 1:CLASS_ROW){
    for(j in 1:CLASS_COL){
      dlog(matrix[i,j]," ")
    }
    dlog("\n")
    
    #f1與周圍同學友好度
    
    #f2
    
    #f3
    
    #f4
  }
}

#------function------




#Student data
studentData<-read.table(STUDENT_DATA_PATH, skip = 1, header = FALSE, sep =',')
#座位編號陣列
seatMatrix<-matrix(c(1:CLASS_SIZE),nrow=CLASS_ROW,ncol=CLASS_COL,byrow=TRUE)

cat("------init chromosome------\n")
chromosomes=makeChromosome()
for(i in 1:POPULATION_SIZE){
  caculateFitnessValue(chromosomes[i,])
  cat(paste0("chromosome",i),chromosomes[i,],"\n\n")
}

