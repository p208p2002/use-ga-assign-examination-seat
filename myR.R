###

###
#變數說明係假設一個5x7(row x col)矩陣，其中col用英文表示
#程式中可自訂矩陣大小,與資料

###
#seatMatrix 坐位編號矩陣
#seatListMatrix 學生的座位矩陣（隨機產生） 
#seatList 學生座位（一維）


###
#五向
#fiveway  記錄五向的坐位編號
#fiveWaySeat 紀錄學生（五向）的座位（對照fiveWay）
#fiveWayRlsValue 紀錄學生（五向）與周圍的關係值（越大表示越差）(5ST1)
#fiveWayGv 與周圍學生的成績差（趨近0好）(5ST2)
#fiveWayConVal 操行成績評價(越大越好)(5ST3)
#fiveWaySexVal 周遭性別差異評分（越大好）(5ST4)

#三向左排(B1-G1)
#threeWayL        
#threeWayLSeat  
#threeWayLRlsValue
#threeWayLGv
#threeWayLConVal
#threeWayLSexVal

#三向右排(B5-G5)
#threeWayR
#threeWayRSeat
#threeWayRRlsValue
#threeWayRGv
#threeWayRConVal
#threeWayRSexVal

#二向
#twoWay
#twoWaySeat
#twoWayRlsValue
#twoWayGv
#twoWayConVal
#twoWaySexVal

#一向
#oneWay
#oneWaySeat
#oneWayRlsValue
#oneWayGv
#oneWayConVal
#oneWaySexVal

##資料平衡
#fiveWay*6
#threeWay*10
#twoWay*15
#oneWay*30

###inital and general function def
rm(list=ls())
printf <- function(...) cat(sprintf(...))
############################## Data Pretreatment setting ##############################

###load data
#'/Users/dsl/Desktop/rdata.csv'

#mac
#header <- read.table('/Users/dsl/Desktop/rdata.csv', nrows = 1, header = FALSE, sep =',', stringsAsFactors = FALSE)
#dat   <- read.table('/Users/dsl/Desktop/rdata.csv', skip = 1, header = FALSE, sep =',')

#windows
header <- read.table('C:\\Users\\Philip\\Desktop\\rdata.csv', nrows = 1, header = FALSE, sep =',', stringsAsFactors = FALSE,fileEncoding="latin1")
dat   <- read.table('C:\\Users\\Philip\\Desktop\\rdata.csv', skip = 1, header = FALSE, sep =',')

colnames( dat ) <- unlist(header)
stuData=dat
rm(dat)

###def a matrix 定義矩陣大小
seatMatrix<-matrix(c(1:35),nrow=7,ncol=5,byrow=TRUE) #set the size here,for debug and compare
row=as.numeric(dim(seatMatrix)[1])
col=as.numeric(dim(seatMatrix)[2])

###custom weight 自訂的權重值
wST2=0.2
wST3=0.1

###debug
dST1=0
dST2=0
dST3=0
dST4=0

###
fST1 = 0
fST2 = 0
fST3 = 0
fST4 = 0

###test
codeTest=0


############################## 

doMutation<-function(chromosome){
  a=floor(runif(1, min=1, max=(col*row)+1))
  b=floor(runif(1, min=1, max=(col*row)+1))
  
  tmpa=chromosome[a]
  chromosome[a]=chromosome[b]
  chromosome[b]=tmpa
  
  return(chromosome)
}

###encode產生chromosome
encode<-function(populationSize){
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

###decoode 把一維chromosome轉二維
decode<-function(seatList,row,col){
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

###產生Chromosome（世代大小,列,欄）
makeChromosome<-function(populationSize,row,col){
  chromosome<-matrix(c(1:1),nrow=populationSize,ncol=col*row,byrow=TRUE)
  #cat("------chromesome------\n")
  for(i in 1:populationSize){
    chromosome[i,]=encode()
    #cat("[",i,"]",chromosome[i,],"\n")
  }
  return(chromosome)
}

###
#the distance with teacher
makeSeatDistanceMatrix<-function(){
seatDistance<-matrix(c(1:(row*col)),nrow=row,ncol=col,byrow=TRUE)
seatMatrixDistanceB=array(dim=col*row)
k=1
l=1
for(i in 1:row){
 for(j in 1:col){
   seatDistance[i,j]=k
   seatMatrixDistanceB[l]=seatDistance[i,j]
   l=l+1
 }
  k=k+1
}
seatDistance=seatMatrixDistanceB
rm(seatMatrixDistanceB)
return(seatDistance)
}


###
#___Way > about the key number for seat in ___Way(seatMatrix)
#___WaySeat > about the student's id number in seat(seatListMatrix) 

###five way(B2-G4)
##set a five way range
makeRange<-function(outputSelect,seatListMatrix){
seatTotal_5w <- seq(from =1,to=(row-1)*(col-2),by=1) #5way total postion
seatRowLen_5w <-col-2#5way row len for go by next row
startPoint=c(2,2)#the position for start to cacutale the seat range 
fiveWay<-array(dim=((col-2)*(row-1)))#to save the result 
fiveWaySeat<-(length(fiveWay))

#cat(startPoint," ")
#cat("M:",seatMatrix[2,2],"\n")
for(i in seatTotal_5w){
  if(i %% 3 == 0){
    if((startPoint[1]+1)>row){ 
      break
    }
    startPoint[1]<-startPoint[1]+1
    startPoint[2]<-2
  }
  else{
    startPoint[2]<-startPoint[2]+1
  } 
  #cat(startPoint," ")
  m=startPoint[1]
  n=startPoint[2]
  #cat("M:",seatMatrix[m,n],"\n")
  
  fiveWay[1]=7
  fiveWay[i+1]=seatMatrix[m,n]
  
  fiveWaySeat[1]=seatListMatrix[2,2]
  fiveWaySeat[i+1]=seatListMatrix[m,n]
}
rm(startPoint,seatRowLen_5w,seatTotal_5w)


###three way left side(B1-G1)
seatTotal_3wl<-row-1
startPoint=c(2,1)
threeWayL<-array(dim=seatTotal_3wl)#to save the result 
threeWayLSeat<-array(dim=length(threeWayL))

for(i in 1:(seatTotal_3wl)){
  threeWayL[i]=seatMatrix[startPoint[1],startPoint[2]]
  threeWayLSeat[i]=seatListMatrix[startPoint[1],startPoint[2]]
  startPoint[1]=startPoint[1]+1
}
rm(startPoint,seatTotal_3wl)

###three way right side(B5-G5)
seatTotal_3wr<-row-1
startPoint=c(2,col)
threeWayR<-array(dim=seatTotal_3wr)#to save the result
threeWayRSeat<-array(dim=length(threeWayL))
for(i in 1:(seatTotal_3wr)){
  threeWayR[i]=seatMatrix[startPoint[1],startPoint[2]]
  threeWayRSeat[i]=seatListMatrix[startPoint[1],startPoint[2]]
  startPoint[1]=startPoint[1]+1
}
rm(startPoint,seatTotal_3wr)

###two way(A2-A4)
seatTotal_2w=col-2
startPoint=c(1,2)
twoWay<-array(seatTotal_2w)#to save the result
twoWaySeat<-array(dim=length(twoWay))
for(i in 1:seatTotal_2w){
  twoWay[i]=i+1
  twoWaySeat[i]=seatListMatrix[1,i+1]
}
rm(seatTotal_2w,startPoint)

###one way(A1,A5)
oneWay<-array(dim=2)
oneWaySeat<-array(dim=length(oneWay))
oneWay[1]=1
oneWay[2]=col
oneWaySeat[1]=seatListMatrix[1,1]
oneWaySeat[2]=seatListMatrix[1,col]

if(outputSelect==1){
  return(fiveWay)
}
if(outputSelect==2){
  return(fiveWaySeat)
}
if(outputSelect==3){
  return(threeWayL)
}
if(outputSelect==4){
  return(threeWayLSeat)
}
if(outputSelect==5){
  return(threeWayR)
}
if(outputSelect==6){
  return(threeWayRSeat)
}
if(outputSelect==7){
  return(twoWay)
}
if(outputSelect==8){
  return(twoWaySeat)
}
if(outputSelect==9){
  return(oneWay)
}
if(outputSelect==10){
  return(oneWaySeat)
}
return(0)
}



############################## Fitness Function ##############################
caculateFitnessValue<-function(seatMatrix,seatListMatrix,seatList){

fiveWaySeat=makeRange(2,seatListMatrix)
threeWayLSeat=makeRange(4,seatListMatrix)
threeWayRSeat=makeRange(6,seatListMatrix)
twoWaySeat=makeRange(8,seatListMatrix)
oneWaySeat=makeRange(10,seatListMatrix)
  
###Fitness function
##5ST1 coculate the relationship by surround 
##五向，計算與周圍同學的關係
startPoint<-c(2,2)
fiveWayRlsValue<-array(dim=length(fiveWay)) #the value of relationship in five way(5ST1)
fiveWayGv<-array(dim=length(fiveWay)) #the value of general value (5ST2)

fiveWaySexVal<-array(dim=length(fiveWay))#5ST4


for(i in 1:length(fiveWay)){
  #the student surround  you
  
  leftSt=c(startPoint[1],startPoint[2]-1)       
  upLeftSt=c(startPoint[1]-1,startPoint[2]-1)
  upSt=c(startPoint[1]-1,startPoint[2])
  upRightSt=c(startPoint[1]-1,startPoint[2]+1)
  rightSt=c(startPoint[1],startPoint[2]+1)
  
 # cat("LST:",leftSt,"(",seatListMatrix[leftSt[1],leftSt[2]],")",
  #    " ULST:",upLeftSt,"(",seatListMatrix[upLeftSt[1],upLeftSt[2]],")",
   #   "UST:",upSt,"(",seatListMatrix[upSt[1],upSt[2]],")",
    #  "URST:",upRightSt,"(",seatListMatrix[upRightSt[1],upRightSt[2]],")",
     # "RSTL",rightSt,"(",seatListMatrix[rightSt[1],rightSt[2]],")",
      #"\n")
  
  ##find the relationship by fiveWaySeat vs studata
  #convert seat number to real student number
  r_leftSt=seatListMatrix[leftSt[1],leftSt[2]]
  r_upLeftSt=seatListMatrix[upLeftSt[1],upLeftSt[2]]
  r_upSt=seatListMatrix[upSt[1],upSt[2]]
  r_upRightSt=seatListMatrix[upRightSt[1],upRightSt[2]]
  r_rightSt=seatListMatrix[rightSt[1],rightSt[2]]
  
  #relationship with
  rlsWithLeft<-as.numeric(stuData[fiveWaySeat[i],5+r_leftSt])
  rlsWithUpLeft<-as.numeric(stuData[fiveWaySeat[i],5+r_upLeftSt])
  rlsWithUpSt<-as.numeric(stuData[fiveWaySeat[i],5+r_upSt])
  rlsWithUpRight<-as.numeric(stuData[fiveWaySeat[i],5+r_upRightSt])
  rlsWithRight<-as.numeric(stuData[fiveWaySeat[i],5+r_rightSt])
  
  ##sex 5ST4
  s_count=0
  mySex=stuData[fiveWaySeat[i],3]
  leftSex=stuData[r_leftSt,3]
  upLeftSex=stuData[r_upLeftSt,3]
  upSex=stuData[r_upSt,3]
  upRightSex=stuData[r_upRightSt,3]
  rightSex=stuData[r_rightSt,3]
  
  if(mySex==leftSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==upLeftSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==upSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==upRightSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==rightSex){s_count=s_count-1}else{s_count=s_count+3}
  
  ##the result of 5ST4
  fiveWaySexVal[i]=s_count
  
  #debug
  if(dST1){
  if(i==1){
    cat("------five way stu------\n")
    cat("[yourSelf]/neighbor/(relationship value)\n")
  }
 cat("[",fiveWaySeat[i],"] ")
 cat(seatListMatrix[leftSt[1],leftSt[2]],"(",rlsWithLeft,") ")
 cat(seatListMatrix[upLeftSt[1],upLeftSt[2]],"(",rlsWithUpLeft,") ")
 cat(seatListMatrix[upSt[1],upSt[2]],"(",rlsWithUpSt,") ")
 cat(seatListMatrix[upRightSt[1],upRightSt[2]],"(",rlsWithUpRight,") ")
 cat(seatListMatrix[rightSt[1],rightSt[2]],"(",rlsWithRight,") ")
 cat("\n")
}
 
 #save the final result
 #儲存最後計算結果 5ST1
 fiveWayRlsValue[i]=rlsWithLeft+rlsWithUpLeft+rlsWithUpSt+rlsWithUpRight+rlsWithRight

 ##5ST2
 myGv<-as.numeric(stuData[fiveWaySeat[i],4])
 
 gvWithLeft<-as.numeric(stuData[r_leftSt,4])
 gvWithUpLeft<-as.numeric(stuData[r_upLeftSt,4])
 gvWithUpSt<-as.numeric(stuData[r_upSt,4])
 gvWithUpRight<-as.numeric(stuData[r_upRightSt,4])
 gvWithRight<-as.numeric(stuData[r_rightSt,4])
 
 #儲存最後計算結果 5ST2
 fiveWayGv[i]=-abs(myGv*5-(gvWithLeft+gvWithRight+gvWithUpLeft+gvWithUpRight+gvWithUpSt))*wST2/5
 
 #debug
 if(dST2){
 if(i==1){
 printf("------fiveWay StuNum(GV)------\n")
 }
 printf("MY:%d(%d) left:%d(%d) upleft:%d(%d) up:%d(%d) upright:%d(%d) right:%d(%d)\n"
        ,fiveWaySeat[i],myGv,
        r_leftSt,gvWithLeft,
        r_upLeftSt,gvWithUpLeft,
        r_upSt,gvWithUpSt,
        r_upRightSt,gvWithUpRight,
        r_rightSt,gvWithRight
        )
 if(i==length(fiveWay)){
   cat(fiveWayGv,"\n")
 }
 }
 
 
 
 #next flag
  if(i%%3==0){
    startPoint[1]=startPoint[1]+1
    startPoint[2]=2
  }
  else{
    startPoint[2]=startPoint[2]+1
  }
 
  
}

##5ST3
fiveWayDistance=array(dim=length(fiveWay))
fiveWayDistance=seatDistance[fiveWay]
myConVal=stuData[fiveWaySeat,5] #conduct value操行成績
fiveWayConVal=myConVal*0.1*fiveWayDistance*wST3



rm(upLeftSt,upRightSt,upSt,leftSt,rightSt,
   r_leftSt,r_rightSt,r_upLeftSt,r_upRightSt,r_upSt,
   rlsWithLeft,rlsWithRight,rlsWithUpLeft,rlsWithUpRight,rlsWithUpSt,
   startPoint,
   gvWithLeft,gvWithRight,gvWithUpLeft,gvWithUpRight,gvWithUpSt,
   myGv,
   fiveWayDistance,myConVal,
   s_count,leftSex,upLeftSex,upSex,upRightSex,rightSex
   )


##3ST1L(B1-G1) 
##three way (left)
startPoint<-c(2,1)
threeWayLRlsValue<-array(dim=length(threeWayL))#three way left relationship value(3ST1L)
threeWayLGv<-array(dim=length(threeWayL)) #the value of general value (3ST2L)

threeWayLSexVal<-array(dim=length(threeWayL))#3ST4L

for(i in 1:length(threeWayL)){
  #the student surround  you
  upSt=c(startPoint[1]-1,startPoint[2])
  upRightSt=c(startPoint[1]-1,startPoint[2]+1)
  rightSt=c(startPoint[1],startPoint[2]+1)
  
  #
  r_upSt=seatListMatrix[upSt[1],upSt[2]]
  r_upRightSt=seatListMatrix[upRightSt[1],upRightSt[2]]
  r_rightSt=seatListMatrix[rightSt[1],rightSt[2]]
  
  #
  rlsWithUpSt<-as.numeric(stuData[threeWayLSeat[i],5+r_upSt])
  rlsWithUpRight<-as.numeric(stuData[threeWayLSeat[i],5+r_upRightSt])
  rlsWithRight<-as.numeric(stuData[threeWayLSeat[i],5+r_rightSt])
  
  if(dST1){
  if(i==1){
    cat("------three way(L) stu------\n")
    cat("[yourSelf]/neighbor/(relationship value)\n")
  }
  cat("[",threeWayLSeat[i],"] ")
  cat(seatListMatrix[upSt[1],upSt[2]],"(",rlsWithUpSt,") ")
  cat(seatListMatrix[upRightSt[1],upRightSt[2]],"(",rlsWithUpRight,") ")
  cat(seatListMatrix[rightSt[1],rightSt[2]],"(",rlsWithRight,") ")
  cat("\n")
  }
  
  #the result of 3ST1L
  threeWayLRlsValue[i]=rlsWithRight+rlsWithUpRight+rlsWithUpSt
  
  ##3ST2L
  myGv<-as.numeric(stuData[threeWayLSeat[i],4])
  
  gvWithUpSt<-as.numeric(stuData[r_upSt,4])
  gvWithUpRight<-as.numeric(stuData[r_upRightSt,4])
  gvWithRight<-as.numeric(stuData[r_rightSt,4])
  
  threeWayLGv[i]=-abs(myGv*3-(gvWithRight+gvWithUpRight+gvWithUpSt))*wST2/3
  
  
  ##sex 3ST4L
  s_count=0
  mySex=stuData[threeWayLSeat[i],3]
  upSex=stuData[r_upSt,3]
  upRightSex=stuData[r_upRightSt,3]
  rightSex=stuData[r_rightSt,3]
  
  if(mySex==upSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==upRightSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==rightSex){s_count=s_count-1}else{s_count=s_count+3}
  
  ##the result of 3ST4L
  threeWayLSexVal[i]=s_count
  
  
  #debug
  if(dST2){
    if(i==1){
      printf("------threeWayL StuNum(GV)------\n")
    }
    printf("MY:%d(%d) up:%d(%d) upright:%d(%d) right:%d(%d)\n"
           ,threeWayLSeat[i],myGv,
           r_upSt,gvWithUpSt,
           r_upRightSt,gvWithUpRight,
           r_rightSt,gvWithRight
    )
    if(i==length(threeWayL)){
      cat(threeWayLGv,"\n")
    }
  }
  
  
  #next flag
  startPoint[1]=startPoint[1]+1
}


##3ST3L
threeWayLDistance=array(dim=length(threeWayL))
threeWayLDistance=seatDistance[threeWayL]
myConVal=stuData[threeWayLSeat,5] #conduct value操行成績
threeWayLConVal=myConVal*0.1*threeWayLDistance*wST3




rm(startPoint,
   r_upRightSt,r_upSt,r_rightSt,
   rlsWithRight,rlsWithUpRight,rlsWithUpSt,
   upSt,upRightSt,rightSt,
   gvWithRight,gvWithUpRight,gvWithUpSt,myGv,
   threeWayLDistance,myConVal,
   s_count,rightSex,upRightSex,upSex
   )

###3ST1R(B5-G5) 
##three way (Right)
startPoint<-c(2,col)
threeWayRRlsValue<-array(dim=length(threeWayR))#three way right relationship value
threeWayRGv<-array(dim=length(threeWayR)) #the value of general value (3ST2R)

threeWayRSexVal<-array(dim=length(threeWayR))#3ST4R

for(i in 1:length(threeWayR)){
  #the student surround  you
  leftSt=c(startPoint[1],startPoint[2]-1)       
  upLeftSt=c(startPoint[1]-1,startPoint[2]-1)
  upSt=c(startPoint[1]-1,startPoint[2])
  
  #
  r_leftSt=seatListMatrix[leftSt[1],leftSt[2]]
  r_upLeftSt=seatListMatrix[upLeftSt[1],upLeftSt[2]]
  r_upSt=seatListMatrix[upSt[1],upSt[2]]
  
  #
  rlsWithLeft<-as.numeric(stuData[threeWayRSeat[i],5+r_leftSt])
  rlsWithUpLeft<-as.numeric(stuData[threeWayRSeat[i],5+r_upLeftSt])
  rlsWithUpSt<-as.numeric(stuData[threeWayRSeat[i],5+r_upSt])
  
  if(dST1){
  if(i==1){
    cat("------three(R) way stu------\n")
    cat("[yourSelf]/neighbor/(relationship value)\n")
  }
  cat("[",threeWayRSeat[i],"] ")
  cat(seatListMatrix[leftSt[1],leftSt[2]],"(",rlsWithLeft,") ")
  cat(seatListMatrix[upLeftSt[1],upLeftSt[2]],"(",rlsWithUpLeft,") ")
  cat(seatListMatrix[upSt[1],upSt[2]],"(",rlsWithUpSt,") ")
  cat("\n")
  }
  
  #3ST1R result
  threeWayRRlsValue[i]=rlsWithLeft+rlsWithUpLeft+rlsWithUpSt
  
  ##3ST2R
  myGv<-as.numeric(stuData[threeWayRSeat[i],4])
  
  gvWithLeft<-as.numeric(stuData[r_leftSt,4])
  gvWithUpLeft<-as.numeric(stuData[r_upLeftSt,4])
  gvWithUpSt<-as.numeric(stuData[r_upSt,4])
 
  
  threeWayRGv[i]=-abs(myGv*3-(gvWithUpSt+gvWithUpLeft+gvWithLeft))*wST2/3
  
  ##sex 3ST4R
  s_count=0
  mySex=stuData[threeWayRSeat[i],3]
  leftSex=stuData[r_leftSt,3]
  upLeftSex=stuData[r_upLeftSt,3]
  upSex=stuData[r_upSt,3]
  
  if(mySex==leftSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==upLeftSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==upSex){s_count=s_count-1}else{s_count=s_count+3}
  
  ##the result of 3ST4R
  threeWayRSexVal[i]=s_count
  
  #debug
  if(dST2){
    if(i==1){
      printf("------threeWayR StuNum(GV)------\n")
    }
    printf("MY:%d(%d) left:%d(%d) upleft:%d(%d) up:%d(%d)\n"
           ,threeWayRSeat[i],myGv,
           r_leftSt,gvWithLeft,
           r_upLeftSt,gvWithUpLeft,
           r_upSt,gvWithUpSt
    )
    if(i==length(threeWayR)){
      cat(threeWayRGv,"\n")
    }
  }
  
  
  #next flag
  startPoint[1]=startPoint[1]+1
}

##3ST3R
threeWayRDistance=array(dim=length(threeWayR))
threeWayRDistance=seatDistance[threeWayR]
myConVal=stuData[threeWayRSeat,5] #conduct value操行成績
threeWayRConVal=myConVal*0.1*threeWayRDistance*wST3

rm(startPoint,
   leftSt,upLeftSt,upSt,
   r_leftSt,r_upLeftSt,r_upSt,
   rlsWithLeft,rlsWithUpLeft,rlsWithUpSt,
   myGv,gvWithLeft,gvWithUpLeft,gvWithUpSt,
   mySex,upLeftSex,upSex,leftSex
  )

##2ST1
startPoint<-c(1,2)
twoWayRlsValue<-array(dim=length(twoWay)) 
twoWayGv<-array(dim=length(twoWay)) #the value of general value (2ST2)

twoWaySexVal<-array(dim=length(twoWay))#2ST4

for(i in 1:length(twoWay)){
  leftSt=c(startPoint[1],startPoint[2]-1)
  rightSt=c(startPoint[1],startPoint[2]+1)
  
  r_leftSt=seatListMatrix[leftSt[1],leftSt[2]]
  r_rightSt=seatListMatrix[rightSt[1],rightSt[2]]
  
  rlsWithLeft<-as.numeric(stuData[twoWaySeat[i],5+r_leftSt])
  rlsWithRight<-as.numeric(stuData[twoWaySeat[i],5+r_rightSt])
  
  #
  if(dST1){
  if(i==1){
    cat("------two way stu------\n")
    cat("[yourSelf]/neighbor/(relationship value)\n")
  }
  cat("[",twoWaySeat[i],"] ")
  cat(seatListMatrix[leftSt[1],leftSt[2]],"(",rlsWithLeft,") ")
  cat(seatListMatrix[rightSt[1],rightSt[2]],"(",rlsWithRight,") ")
  cat("\n")
  }
  
  #save the result
  twoWayRlsValue[i]=rlsWithRight+rlsWithLeft
  
  ##2ST2
  
  myGv<-as.numeric(stuData[twoWaySeat[i],4])
  
  gvWithLeft<-as.numeric(stuData[r_leftSt,4])
  gvWithRight<-as.numeric(stuData[r_rightSt,4])
  
  #儲存最後計算結果 2ST2
  twoWayGv[i]=-abs(myGv*2-(gvWithLeft+gvWithRight))*wST2/2
  
  ##sex 2ST4
  s_count=0
  mySex=stuData[twoWaySeat[i],3]
  leftSex=stuData[r_leftSt,3]
  rightSex=stuData[r_rightSt,3]
  
  if(mySex==leftSex){s_count=s_count-1}else{s_count=s_count+3}
  if(mySex==rightSex){s_count=s_count-1}else{s_count=s_count+3}
  
  ##the result of 5ST4
  twoWaySexVal[i]=s_count
  
  #debug
  if(dST2){
    if(i==1){
      printf("------twoWay StuNum(GV)------\n")
    }
    printf("MY:%d(%d) left:%d(%d) right:%d(%d)\n"
           ,twoWaySeat[i],myGv,
           r_leftSt,gvWithLeft,
           r_rightSt,gvWithRight
    )
    if(i==length(twoWay)){
      cat(twoWayGv,"\n")
    }
  }
  
  
  #next flag
  startPoint[2]=startPoint[2]+1
}

##2ST3
twoWayDistance=array(dim=length(twoWay))
twoWayDistance=seatDistance[twoWay]
myConVal=stuData[twoWaySeat,5] #conduct value操行成績
twoWayConVal=myConVal*0.1*twoWayDistance*wST3

rm(startPoint,leftSt,rightSt,r_leftSt,r_rightSt,rlsWithLeft,rlsWithRight,
   myGv,gvWithLeft,gvWithRight,
   twoWayDistance,myConVal,
   mySex,rightSex,leftSex,s_count
   )

##1ST1
##one way
#seat at upleft 左上
r_rightSt=seatListMatrix[1,2] #右邊同學是誰
rlsWithRight<-as.numeric(stuData[oneWaySeat[1],5+r_rightSt])


#
if(dST1){
if(dST1){
  cat("------one way stu------\n")
  cat("[yourSelf]/neighbor/(relationship value)\n")
}
cat("[",seatListMatrix[1,1],"]")
cat(seatListMatrix[1,2])
cat("(",rlsWithRight,")\n")
}

#seat at upright 右上
r_leftSt=seatListMatrix[1,col-1] #左邊同學是誰
rlsWithLeft<-as.numeric(stuData[oneWaySeat[2],5+r_leftSt])

#
if(dST1){
cat("[",seatListMatrix[1,col],"]")
cat(seatListMatrix[1,col-1])
cat("(",rlsWithLeft,")\n")
}

#save the result
oneWayRlsValue<-c(rlsWithRight,rlsWithLeft)

##1ST2
oneWayGv=array(dim=2)
myGv=array(dim=2)
sideStGv=array(dim=2)

myGv[1]=stuData[oneWaySeat[1],4]
sideStGv[1]=stuData[seatListMatrix[1,2],4]

myGv[2]=stuData[oneWaySeat[2],4]
sideStGv[2]=stuData[seatListMatrix[1,col-1],4]

oneWayGv=-abs((myGv-sideStGv))*wST2

if(dST2){
    printf("------oneWay StuNum(GV)------\n")
    cat("myGv:",myGv,"\n")
    cat("sideGv:",sideStGv,"\n")
    cat(oneWayGv,"\n")
}

##1ST3
oneWayDistance=array(dim=length(oneWay))
oneWayDistance=seatDistance[oneWay]
myConVal=stuData[oneWaySeat,5] #conduct value操行成績
oneWayConVal=myConVal*0.1*oneWayDistance*wST3


##1ST4
oneWaySexVal<-array(dim=length(oneWay))
##sex 1ST4
mySex=c(0,0)
sideStSex=c(0,0)


mySex[1]=stuData[oneWaySeat[1],3]
mySex[2]=stuData[oneWaySeat[2],3]

sideStSex[1]=stuData[seatListMatrix[1,2],3]
sideStSex[2]=stuData[seatListMatrix[1,col-1],3]

##the result of 1ST4
if(mySex[1]!=sideStSex[1]){oneWaySexVal[1]=3}else{oneWaySexVal[1]=-1}
if(mySex[2]!=sideStSex[2]){oneWaySexVal[2]=3}else{oneWaySexVal[2]=-1}


#
rm(r_rightSt,r_leftSt,rlsWithLeft,rlsWithRight,
   myGv,sideStGv,
   oneWayDistance,myConVal,
   mySex,sideStSex
   )

###debug
if(dST3){
  cat("------dST3------\n")
  cat("five\n")
  cat(fiveWayConVal,"\n")
  cat("three way L\n")
  cat(threeWayLConVal,"\n")
  cat("three way R\n")
  cat(threeWayRConVal,"\n")
  cat("two way\n")
  cat(twoWayConVal,"\n")
  cat("one way\n")
  cat(oneWayConVal,"\n")
}

if(dST4){
  cat("------dST4------\n")
  cat("five\n")
  cat(fiveWaySexVal,"\n")
  cat("three way L\n")
  cat(threeWayLSexVal,"\n")
  cat("three way R\n")
  cat(threeWayRSexVal,"\n")
  cat("two way\n")
  cat(twoWaySexVal,"\n")
  cat("one way\n")
  cat(oneWaySexVal,"\n")
}

## caculate fitness value
IV=fiveWayRlsValue+fiveWayGv+fiveWayConVal+fiveWaySexVal
IIIL=threeWayLRlsValue+threeWayLGv+threeWayLConVal+threeWayLSexVal
IIIR=threeWayRRlsValue+threeWayRGv+threeWayRConVal+threeWayRSexVal
II=twoWayRlsValue+twoWayGv+twoWayConVal+twoWaySexVal
I=oneWayRlsValue+oneWayGv+oneWayConVal+oneWaySexVal

cat("st question value\n")
st1 = 0
for(i in 1:length(fiveWayRlsValue)){
  st1 = st1 + fiveWayRlsValue[i]
}
for(i in 1:length(threeWayLRlsValue)){
  st1 = st1 + threeWayLRlsValue[i]
}
for(i in 1:length(threeWayRRlsValue)){
  st1 = st1 + threeWayRRlsValue[i]
}
for(i in 1:length(twoWayRlsValue)){
  st1 = st1 + twoWayRlsValue[i]
}
for(i in 1:length(oneWayRlsValue)){
  st1 = st1 + oneWayRlsValue[i]
}


st2 = 0
for(i in 1:length(fiveWayGv)){
  st2 = st2 + fiveWayGv[i]
}
for(i in 1:length(threeWayLGv)){
  st2 = st2 + threeWayLGv[i]
}
for(i in 1:length(threeWayRGv)){
  st2 = st2 + threeWayRGv[i]
}
for(i in 1:length(twoWayGv)){
  st2 = st2 + twoWayGv[i]
}
for(i in 1:length(oneWayGv)){
  st2 = st2 + oneWayGv[i]
}

st3 = 0
for(i in 1:length(fiveWayConVal)){
  st3 = st3 + fiveWayConVal[i]
}
for(i in 1:length(threeWayLConVal)){
  st3 = st3 + threeWayLConVal[i]
}
for(i in 1:length(threeWayRConVal)){
  st3 = st3 + threeWayRConVal[i]
}
for(i in 1:length(twoWayConVal)){
  st3 = st3 + twoWayConVal[i]
}
for(i in 1:length(oneWayConVal)){
  st3 = st3 + oneWayConVal[i]
}

st4 = 0
for(i in 1:length(fiveWaySexVal)){
  st4 = st4 + fiveWaySexVal[i]
}
for(i in 1:length(threeWayLSexVal)){
  st4 = st4 + threeWayLSexVal[i]
}
for(i in 1:length(threeWayRSexVal)){
  st4 = st4 + threeWayRSexVal[i]
}
for(i in 1:length(twoWaySexVal)){
  st4 = st4 + twoWaySexVal[i]
}
for(i in 1:length(oneWaySexVal)){
  st4 = st4 + oneWaySexVal[i]
}

print(paste("st1",st1,"st2",st2,"st3",st3,"st4",st4))


rs=0
for(i in 1:length(fiveWay)){
  rs=rs+IV[i]
}
for(i in 1:length(threeWayL)){
  rs=rs+IIIL[i]+IIIR[i]
}
for(i in 1:length(twoWay)){
  rs=rs+II[i]
}
for(i in 1:length(oneWay)){
  rs=rs+I[i]
}
fitnessValue=rs
if(codeTest){
  cat("fitnessValue:",fitnessValue,"\n")
}

return(fitnessValue)
}###end 


###

###inital data sturct
seatList=encode() 
seatListMatrix=decode(seatList,row,col)
seatDistance=makeSeatDistanceMatrix()

###inital data
fiveWay=makeRange(1,seatListMatrix)
fiveWaySeat=makeRange(2,seatListMatrix)
threeWayL=makeRange(3,seatListMatrix)
threeWayLSeat=makeRange(4,seatListMatrix)
threeWayR=makeRange(5,seatListMatrix)
threeWayRSeat=makeRange(6,seatListMatrix)
twoWay=makeRange(7,seatListMatrix)
twoWaySeat=makeRange(8,seatListMatrix)
oneWay=makeRange(9,seatListMatrix)
oneWaySeat=makeRange(10,seatListMatrix)


############################## Genetic Algorithm ##############################
##


###
log=0
logMaxChromosome=0
###
G1d=1
G2d=0
G3d=0
G4d=0

##
show1=1
show2=0

##
#編碼方式:實數編碼
#交配方法:PMX(Partially Matched Crossover)

###GA setting
populationSize=6 #世代大小(必須是偶數)
setCrossOverLen=3 #交配長度

mutation=1 #0=disable 1=enable 
mutation_value=75 #10=1% 1=0.1%

elite=1 #菁英政策 0=disable 1=enable 

doTimes=5000


##
cat("Genetic Algorithm\n\n")
###
chromosome=makeChromosome(populationSize,row,col)
fitnessValue=0

if(G1d){
cat("------產生初始世代------\n")
cat("chromesome/fitnessvalue\n")
for(i in 1:populationSize){
seatList=chromosome[i,]
seatListMatrix=decode(seatList,row,col)
fitnessValue[i]=caculateFitnessValue(seatMatrix,seatListMatrix,seatList)
cat(chromosome[i,],"/",fitnessValue[i],"\n")
}
}
Sys.sleep(3)

oChromosomeFt=fitnessValue

crossOverLen<-function(){
 
  return(floor(runif(1, min=1, max=(setCrossOverLen)+1)))
}

crossOverPoint<-function(crossOverLen){
  
  return(floor(runif(1, min=1, max=(col*row-crossOverLen)+1)))
}

if(G2d){cat("------ G2d ------\n")}
cLen=0
cPoint=0

cLen2=0
cPoint2=0

CrossOverTmp=0
CrossOverTmp2=0

for(k in 1:doTimes){
  
  odd<-seq(from = 1,to = (populationSize),by = 2)
  for(i in odd) { 
    
  #清空資料
  CrossOverTmp=0
  CrossOverTmp2=0
  
  #隨機產生交配段1
  cLen=crossOverLen() #cross over length 交配長度
  cPoint=crossOverPoint(cLen) #cross over point 交配點
  #printf("cLen:%d cPoint:%d\n",cLen,cPoint)
  
  if(G2d){printf("chromosome%d cLen:%d cPoint:%d\n",i,cLen,cPoint)}
  
  #儲存交配段1
  for(j in 1:cLen){
    #cat(i,"in\n")
    CrossOverTmp[j]=chromosome[i,cPoint+j-1]
    
  }
  if(G2d){cat("gFragments:",CrossOverTmp,"\n")}
  
  #隨機產生交配段2
  cLen2=cLen
  cPoint2=crossOverPoint(cLen2)
  if(G2d){printf("chromosome%d cLen:%d cPoint:%d\n",i+1,cLen2,cPoint2)}
  
  #儲存交配段2
  for(j in 1:cLen2){
    #cat(i,"in\n")
    CrossOverTmp2[j]=chromosome[i+1,cPoint2+j-1]
  }
  if(G2d){cat("gFragments2:",CrossOverTmp2,"\n\n")}
  
  #交配 奇<偶
  for(l in 1:cLen){
    chromosome[i,cPoint+l-1]=CrossOverTmp2[l]
  }
  
  #交配 偶<奇
  for(l in 1:cLen2){
    chromosome[i+1,cPoint2+l-1]=CrossOverTmp[l]
  }
  
}
  
  ##重新排列
  #找到重複的數值
  
  repeatChromosomeAndPoint<-matrix(c(0:0),nrow=populationSize,ncol=col*row,byrow=TRUE)
  
  odd2<-seq(from = 1,to = (populationSize),by = 2)
  for(m in odd2)
  for(i in 1:(col*row)){
    for(j in 1:(col*row)){
      if(i!=j){
      if(chromosome[m,i]==chromosome[m,j]){
       #cat("chromosome",m," point1:",i," point2:",j," value:",chromosome[m,i],"\n")
        repeatChromosomeAndPoint[m,i]=chromosome[m,i]
      }
        
        if(chromosome[m+1,i]==chromosome[m+1,j]){
          #cat("chromosome",m+1," point1:",i," point2:",j," value:",chromosome[m+1,i],"\n")
          repeatChromosomeAndPoint[m+1,i]=chromosome[m+1,i]
        }  
      }
    }
  }
  
  #排除重複
  counta<-seq(from = col*row,to = 1,by = -1)
  countb<-seq(from =1,to = col*row,by = 1)
  for(jj in 1:populationSize)
  for(ii in counta ){
    if(repeatChromosomeAndPoint[jj,ii]!=0){
      tmpa=repeatChromosomeAndPoint[jj,ii]
      for(kk in countb){
        if(repeatChromosomeAndPoint[jj,kk]==tmpa){
          repeatChromosomeAndPoint[jj,kk]=0
          break
        }
      }
      
    }
      
  }
  
  #交換
  for(ii in odd2){
    for(jj in counta){
      if(repeatChromosomeAndPoint[ii,jj]!=0){
        repeatChromosomeAndPoint[ii,jj]=0
        for(kk in counta){
          if(repeatChromosomeAndPoint[ii+1,kk]!=0){
            repeatChromosomeAndPoint[ii+1,kk]=0
            #交換
            tmpVal=chromosome[ii,jj]
            chromosome[ii,jj]=chromosome[ii+1,kk]
            chromosome[ii+1,kk]=tmpVal
            break
          }
        }
      }
    }
  }
  
  
  
  
  
  
  #計算fitness value
  
  if(show1){cat("世代:",k,"\n")}
  
  ftv=0
  totalValue=0
  selectChange=0
  for(i in 1:populationSize){
    ftv[i]=caculateFitnessValue(seatMatrix,decode(chromosome[i,],row,col),chromosome[i,])
    if(show1){cat(ftv[i],"\n")}
    
    totalValue=totalValue+ftv[i]
    selectChange[i]=totalValue
  }
  if(show1){cat("\n")}
  
  #if(show2){cat("Population:",k,",",k/doTimes*100,"% Max Fitness value:",max(log),"\n")}
  if(show2){
    printf("Population:%d,Processing rate:%.4f,Max Fitness value:%.8f\n",k,k/doTimes,max(log))
  }
  #產生下一代
  newChromosome=chromosome
  Russian_Roulette=0
    for(i in 1:populationSize){
      Russian_Roulette[i]=(runif(1, min=0, max=(totalValue)+1))
      #cat(Russian_Roulette[i],"\n")
      
    }
  
  
  for(i in 1:populationSize){
    for(j in 1:populationSize){
      if(Russian_Roulette[i]>=selectChange[j]){
        newChromosome[i,]=chromosome[j,]
        #break
      }
  }
  }
  
  #log
  log[k]=max(ftv)
  
  if(max(ftv)>=max(log)){
  for(i in 1:populationSize){
    if(max(ftv)==ftv[i]){
      logMaxChromosome=chromosome[i,]
    }
  }
  }
  #菁英政策
  if(elite){
  maxFtv=max(ftv)
  minFtv=min(ftv)
  maxChromosome=0
  minChromosome=0
  for(i in 1:populationSize){
    if(maxFtv==ftv[i]){
      maxChromosome=chromosome[i,]
    }
  }
  for(i in 1:populationSize){
    if(minFtv==ftv[i]){
      minChromosome=i
    }
  }
  }
  
  #產生新世代
  chromosome=newChromosome
  
  #踢掉弱的加入菁英
  if(elite){
    #chromosome[minChromosome,]=maxChromosome #保留上一代最高的
    chromosome[minChromosome,]=logMaxChromosome #歷史最高的
  }
  
  #突變
  if(mutation){
    for(i in 1:populationSize){
      wantMutation=floor(runif(1, min=1, max=(1000)+1))
      if(wantMutation<=mutation_value){
        cat("!!!Do mutation!!! !!!Do mutation!!! !!!Do mutation!!!\n")
        #cat("突變",chromosome[i,],"\n")
        chromosome[i,]=doMutation(chromosome[i,])
        #cat("結果",chromosome[i,],"\n")
      }
    }
  }
  
}



#繪出每次交配的最高值
plot(log)

#
cat("Max Fitness value:",max(log),'\n')


##顯示結果
#decode(logMaxChromosome,row,col)  

#caculateFitnessValue(seatMatrix,decode(chromosome[1,],row,col),chromosome[1,])
#caculateFitnessValue(seatMatrix,decode(logMaxChromosome,row,col),logMaxChromosome)

###
#rm(i,j,k,l,m,n,
#   dST1,dST2,dST3,dST4,
#   wST2,wST3,
#   codeTest
#   )
#rm(seatMatrix)