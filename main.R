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
CROSS_OVER_LEN = 2 #交配長度
MUTATION_ENABLE = 1
MUTATION_VALUE = 50 #10=1% 1=0.1%
ELITE_ENABLE=1 #菁英政策 0=disable 1=enable 
DO_TIMES=3500
VAILD_TIMES = 1

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

getNearByStudents<-function(studentSeatMatrix,stuAt_y,stuAt_x,withOutUpSide=0){
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
  if(withOutUpSide){
    hasRightUp = 0
    hasLeftUp = 0
  }
  
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
  
  if(length(chromosome)==1)
    return(0)
  
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
      
      nearByStudent = getNearByStudents(matrix,i,j,1)
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

crossOverLen<-function(setCrossOverLen = CROSS_OVER_LEN){
  return(floor(runif(1, min=1, max=(setCrossOverLen)+1)))
}

crossOverPoint<-function(crossOverLen,col=CLASS_COL,row=CLASS_ROW){
  return(floor(runif(1, min=1, max=(col*row-crossOverLen)+1)))
}

doMutation<-function(chromosome,col=CLASS_COL,row=CLASS_ROW){
  a=floor(runif(1, min=1, max=(col*row)+1))
  b=floor(runif(1, min=1, max=(col*row)+1))
  
  tmpa=chromosome[a]
  chromosome[a]=chromosome[b]
  chromosome[b]=tmpa
  
  return(chromosome)
}

#------function------

#Student data
studentData<-read.table(STUDENT_DATA_PATH, skip = 1, header = FALSE, sep =',')
#座位編號陣列
seatMatrix<-matrix(c(1:CLASS_SIZE),nrow=CLASS_ROW,ncol=CLASS_COL,byrow=TRUE)

#OPT ANS
optChromosome = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
optAns = caculateFitnessValue(optChromosome)
optTimes = 0

#vaild log
gaps = array()

for(x in 1:VAILD_TIMES){
  #
  globalLog = array()
  logMaxChromosome = array()
  logMaxFitnessValue = 0
  
  cat("------init chromosome------\n")
  chromosomes=makeChromosome()
  for(i in 1:POPULATION_SIZE){
    cat(paste0("chromosome",i),chromosomes[i,],"\n")
    ftv=caculateFitnessValue(chromosomes[i,])
    cat(ftv)
    cat("\n")
  }
  
  ##
  cLen=0
  cPoint=0
  
  cLen2=0
  cPoint2=0
  
  CrossOverTmp=array()
  CrossOverTmp2=array()

  for(k in 1:DO_TIMES){
    if(logMaxFitnessValue >= optAns){
      optTimes = optTimes+1
      break;
    }
    
    odd<-seq(from = 1,to = (POPULATION_SIZE),by = 2)
    for(i in odd) { 
      #清空資料
      CrossOverTmp=array()
      CrossOverTmp2=array()
      
      #隨機產生交配段1
      cLen=crossOverLen() #cross over length 交配長度
      cPoint=crossOverPoint(cLen) #cross over point 交配點
      
      #儲存交配段1
      for(j in 1:cLen){
        CrossOverTmp[j]=chromosomes[i,cPoint+j-1]
      }
      
      #隨機產生交配段2
      cLen2=cLen
      cPoint2=crossOverPoint(cLen2)
      
      #儲存交配段2
      for(j in 1:cLen2){
        CrossOverTmp2[j]=chromosomes[i+1,cPoint2+j-1]
      }
      
      #交配 奇<偶
      for(l in 1:cLen){
        chromosomes[i,cPoint+l-1]=CrossOverTmp2[l]
      }
      
      #交配 偶<奇
      for(l in 1:cLen2){
        chromosomes[i+1,cPoint2+l-1]=CrossOverTmp[l]
      }
    }
    
    ##重新排列
    #找到重複的數值
    repeatChromosomeAndPoint<-matrix(c(0:0),nrow=POPULATION_SIZE,ncol=CLASS_COL*CLASS_ROW,byrow=TRUE)
    odd2<-seq(from = 1,to = (POPULATION_SIZE),by = 2)
    for(m in odd2){
      for(i in 1:(CLASS_COL*CLASS_ROW)){
        for(j in 1:(CLASS_COL*CLASS_ROW)){
          if(i!=j){
            if(chromosomes[m,i]==chromosomes[m,j]){
              repeatChromosomeAndPoint[m,i]=chromosomes[m,i]
            }
            if(chromosomes[m+1,i]==chromosomes[m+1,j]){
              repeatChromosomeAndPoint[m+1,i]=chromosomes[m+1,i]
            }  
          }
        }
      }
    }
    
    #排除重複
    counta<-seq(from = CLASS_COL*CLASS_ROW,to = 1,by = -1)
    countb<-seq(from =1,to = CLASS_COL*CLASS_ROW,by = 1)
    for(j in 1:POPULATION_SIZE){
      for(i in counta ){
        if(repeatChromosomeAndPoint[j,i]!=0){
          tmpa=repeatChromosomeAndPoint[j,i]
          for(m in countb){
            if(repeatChromosomeAndPoint[j,m]==tmpa){
              repeatChromosomeAndPoint[j,m]=0
              break
            }
          }
        }
      }
    }
    
    #交換
    for(i in odd2){
      for(j in counta){
        if(repeatChromosomeAndPoint[i,j]!=0){
          repeatChromosomeAndPoint[i,j]=0
          for(m in counta){
            if(repeatChromosomeAndPoint[i+1,m]!=0){
              repeatChromosomeAndPoint[i+1,m]=0
              #交換
              tmpVal=chromosomes[i,j]
              chromosomes[i,j]=chromosomes[i+1,m]
              chromosomes[i+1,m]=tmpVal
              break
            }
          }
        }
      }
    }
    
    #交配完成
    fitnessValues = array()
    totalFitnessValue = 0
    selectChange = array()
    cat(paste0("世代:",k),"\n")
    for(i in 1:POPULATION_SIZE){
      #cat(paste0("chromosome",i),chromosomes[i,],"\n")
      fitnessValue = caculateFitnessValue(chromosomes[i,])
      totalFitnessValue = totalFitnessValue + fitnessValue
      selectChange[i] = totalFitnessValue
      fitnessValues[i] = fitnessValue
      cat(fitnessValue)
      cat("\n")
    }
    
    #log
    maxFtv = max(fitnessValues)
    if(maxFtv > logMaxFitnessValue){
      logMaxFitnessValue = maxFtv
      for(i in 1:POPULATION_SIZE){
        if(maxFtv==fitnessValues[i]){
          logMaxChromosome=chromosomes[i,]
        }
      }
    }
    globalLog[k] = logMaxFitnessValue
    
    
    #選擇下一代(輪盤法)
    newChromosomes=chromosomes
    Russian_Roulette=0
    for(i in 1:POPULATION_SIZE){
      Russian_Roulette[i]=(runif(1, min=0, max=(totalFitnessValue)+1))
    }
    for(i in 1:POPULATION_SIZE){
      for(j in 1:POPULATION_SIZE){
        if(Russian_Roulette[i]>=selectChange[j]){
          newChromosomes[i,]=chromosomes[j,]
        }
      }
    }
    
    #產生新世代
    chromosomes=newChromosomes
    
    #突變
    if(MUTATION_ENABLE){
      for(i in 1:POPULATION_SIZE){
        wantMutation=floor(runif(1, min=1, max=(1000)+1))
        if(wantMutation<=MUTATION_VALUE){
          cat("!!!mutation!!!\n")
          chromosomes[i,]=doMutation(chromosomes[i,])
        }
      }
    }
    
    #找出新世代最低
    minChromosomeId=0
    newFitVla = array()
    for(i in 1:POPULATION_SIZE){
      newFitVla[i] = caculateFitnessValue(chromosomes[i,])
    }
    minFtv = min(newFitVla)
    for(i in 1:POPULATION_SIZE){
      if(minFtv==newFitVla[i]){
        minChromosomeId=i
      }
    }
    
    #踢掉弱的加入菁英
    if(ELITE_ENABLE && logMaxFitnessValue != 0){
      chromosomes[minChromosomeId,]=logMaxChromosome #歷史最高的
    }
    
    
  }
  plot(globalLog)
  cat("\n")
  gaAns = caculateFitnessValue(logMaxChromosome)
  optAns = caculateFitnessValue(optChromosome)
  gap = (optAns-gaAns)/optAns*100
  gaps[x] = gap
  cat("\n")
  cat("GA ANS",gaAns)
  cat("\n")
  cat("Optimization Ans",optAns)
  cat("\n")
  cat("gap:",gap)
}

cat("\n\n")
cat("gaps avg:",mean(gaps))
cat("\n")
cat("opt times:",optTimes)