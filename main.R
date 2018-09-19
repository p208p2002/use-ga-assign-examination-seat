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
STUDENT_DATA_PATH = '.\\rdata_utf8_33.csv'

#Fitness weight setting
F2_WEIGHT=0.2
F3_WEIGHT=0.1

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
decode<-function(seatList,row=CLASS_ROW,col=CLASS_COL){
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

getNearByStudents<-function(studentSeatMatrix,stuAt_y,stuAt_x){
  #
  nearByStudents = array()
  hasLeft = 0
  hasLeftUp = 0
  hasUp = 0
  hasRightUp = 0
  hasRight = 0
  
  #
  if(stuAt_x-1 >= 1)
    hasLeft = 1
  if(stuAt_x-1 >= 1 && stuAt_y-1 >= 1)
    hasLeftUp = 1
  if(stuAt_y-1 >= 1)
    hasUp = 1
  if(stuAt_x+1 <= CLASS_COL && stuAt_y-1 >= 1)
    hasRightUp = 1
  if(stuAt_x+1 <= CLASS_COL)
    hasRight = 1
  
  #
  if(hasLeft)
    nearByStudents = append(nearByStudents,studentSeatMatrix[stuAt_y,stuAt_x-1])
  if(hasLeftUp)
    nearByStudents = append(nearByStudents,studentSeatMatrix[stuAt_y-1,stuAt_x-1])
  if(hasUp)
    nearByStudents = append(nearByStudents,studentSeatMatrix[stuAt_y-1,stuAt_x])
  if(hasRightUp)
    nearByStudents = append(nearByStudents,studentSeatMatrix[stuAt_y-1,stuAt_x+1])
  if(hasRight)
    nearByStudents = append(nearByStudents,studentSeatMatrix[stuAt_y,stuAt_x+1])
  
  nearByStudents = c(nearByStudents[2:length(nearByStudents)])
  return(nearByStudents)
}

caculateFitnessValue<-function(chromosome){
  matrix = decode(chromosome)
  f1Val = 0
  f2Val = 0
  f3Val = 0
  f4Val = 0
  for(i in 1:CLASS_ROW){
    for(j in 1:CLASS_COL){
      #dlog(matrix[i,j]," ")
      self = matrix[i,j]
      nearByStudent = getNearByStudents(matrix,i,j)
      
      #f1與周圍同學友好度
      for(k in 1:length(nearByStudent)){
        f1Val = f1Val + studentData[self,5+nearByStudent[k]]
      }
      
      #f2鄰近座位平時成績接近較好
      nearByStudentUsualGrades = 0
      for(k in 1:length(nearByStudent)){
        nearByStudentUsualGrades = nearByStudentUsualGrades + studentData[nearByStudent[k],4]
      }
      selfUsualScore = studentData[self,4]
      f2Val = f2Val + abs((selfUsualScore*length(nearByStudent)-nearByStudentUsualGrades)*F2_WEIGHT/length(nearByStudent))*-1
      
      #f3操行成績越低座位要越前面
      selfPerformanceScore = studentData[self,5]
      f3Val = f3Val + (selfPerformanceScore*F3_WEIGHT*i*F3_WEIGHT)^2
      
      #f4鄰近座位性別不同較好
      selfSex = studentData[self,3]
      for(k in 1:length(nearByStudent)){
        nearByStudentSex = studentData[nearByStudent[k],3]
        if(selfSex == nearByStudentSex)
          f4Val = f4Val - 1
        else
          f4Val = f4Val + 3
      }
    }
  }
  
  fitnessValue = f1Val + f2Val + f3Val + f4Val
  cat("fitnessValue",fitnessValue," ")
  cat("f1",f1Val," ")
  cat("f2",f2Val," ")
  cat("f3",f3Val," ")
  cat("f4",f4Val,"\n")
  
  return(fitnessValue)
}

#------function------




#Student data
studentData<-read.table(STUDENT_DATA_PATH, skip = 1, header = FALSE, sep =',')
#座位編號陣列
seatMatrix<-matrix(c(1:CLASS_SIZE),nrow=CLASS_ROW,ncol=CLASS_COL,byrow=TRUE)

cat("------init chromosome------\n")
chromosomes=makeChromosome()
for(i in 1:POPULATION_SIZE){
  cat(paste0("chromosome",i),chromosomes[i,],"\n")
  caculateFitnessValue(chromosomes[i,])
  cat("\n")
}

for(i in 1:DO_TIMES){
  
}

