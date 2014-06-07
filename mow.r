library("e1071")
library("klaR")
library("rpart")

# prepare data to read from csv files
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
breastPath = paste(PATH,"/breast.csv",sep="")
winePath = paste(PATH,"/wine.csv",sep="")
irisPath = paste(PATH,"/iris.csv",sep="")

gdata=0
gcol=0

readCsvData = function(filepath,colname="",colnr=0){
  isHeader = !colname==""
  data = read.csv(filepath, header=isHeader) 
  if (isHeader) {
    colnr = match(colname,colnames(data))
  } else {
    colname =  colnames(data)[colnr]
  }
  
  list(data,colnr,colname)
}

#-----------utils functions-----------

#vec01 = vector of {0;1}
#convert vec01 to collection of column numbers
columnsToClusterNatural <- function (myvec){
  colnum = 1
  returnvec = c()
  for(i in myvec)
  {
    if(i == 1)
    {
      returnvec = append(returnvec,colnum)
    }
    colnum = colnum + 1
  }
  returnvec
}

#input: data, vec01
#output: data with columns indicated by vec01
extractCols <- function (mydata, myvec) {
  mydata[columnsToClusterNatural(myvec)]
}

getColNumber = function (data,colName){
  colNumber = match(colName,colnames(data))
  colNumber
}

deleteColumnByName = function (data,colName){
  new_data = data[,-getColNumber(data,colName)]
} 

appendColumn = function (data,column){
  data[,colnames(column)[1]]=column
  data
}

selectRandom = function(vecTable){
  rand = sample(1:(dim(vecTable)[2]),1)
  vecTable[,rand]
}

generateNeighbours = function(vec01, k){
  len01 = length(vec01)
  returnvec = array(0,c(len01,choose(len01,k)))
  x = combn(length(vec01),k)
  veccount = 1
  for(i in seq(1,ncol(x))) 
  { 
    newvec = vec01
    for(j in seq(1,nrow(x)))
    {
      newvec[x[j,i]] = (1 - vec01[x[j,i]])
    }
    if (sum(newvec)==sum(vec01))
    {
      #append vector
      returnvec[,veccount] = newvec
      veccount = veccount + 1
    }
  }
  returnvec=returnvec[,1:veccount-1,drop=FALSE]
  returnvec
  
}

randomVector = function(dim, atrCount){
  vect = array(dim=dim,data=0)
  vect[sample(dim,atrCount)]=1
  vect
}

callMethod = function(method,indata,colName,vect){
  column = indata[colName]
  workdata=deleteColumnByName(indata,colName)
  workdata = extractCols(workdata,vect)
  workdata = appendColumn(workdata,column)
  method(workdata,colName) 
}

selectBest = function(neigh,indata,method,colName) {
  bestVector = 0
  bestScore = 0
  for(i in 1:ncol(neigh))
  {
    vect = neigh[,i]
    score = callMethod(method,indata,colName,vect)
    if(score > bestScore)
    {   
      bestScore = score
      bestVector = vect
    }        
  }
  list(bestVector,bestScore)
}

#-----------score functions-----------

SVM = function(data,colName){
  formul = as.formula(paste(colName,"~."))
  colNumber = match(colName,colnames(data))
  model = svm(formul, data)
  zmienna=predict(model, data[-colNumber])
  tryCatch({zmienna=round(zmienna)},error=function(e){})
  quality=sum(as.integer(data[,colNumber]==zmienna))/dim(data)[1]
  quality
}

NB = function(data,colName){
  formul = as.formula(paste(colName,"~."))
  colNumber = match(colName,colnames(data))
  model = NaiveBayes(formul,data)
  zmienna=predict(model, data[-colNumber])$class
  tryCatch({zmienna=round(zmienna)},error=function(e){})
  quality=sum(as.integer(data[,colNumber]==zmienna))/dim(data)[1]
  quality
}

DT = function(data,colName){
  formul = as.formula(paste(colName,"~."))
  colNumber = match(colName,colnames(data))
  model = rpart(formul,data)
  zmienna=predict(model,type = "class")
  tryCatch({zmienna=round(zmienna)},error=function(e){})
  quality=sum(as.integer(data[,colNumber]==zmienna))/dim(data)[1]
  quality
}

#-----------optimization functions-----------

MMC = function(indata,colName,atrCount,iterCount, method){
  best=-1
  for (i in 1:iterCount) {
    vect = randomVector(ncol(indata)-1,atrCount)
    qual=callMethod(method,indata,colName,vect)
    if (qual>best){
      best=qual
      bestvect=vect
    }
  }
  list(bestvect,best)
}

RW = function(indata,colName,atrCount,iterCount, method){
  best=0
  vect = randomVector(ncol(indata)-1,atrCount)
  for (i in 1:iterCount) {
    qual=callMethod(method,indata,colName,vect) 
    if (qual>best){
      best=qual
      bestvect=vect
    }
    vect = selectRandom(generateNeighbours(vect,2))
  }
  list(bestvect,best)
}

VNS = function(indata,colName,atrCount,maxDist, method){
  column = indata[colName]
  best=0
  bestvect = randomVector(ncol(indata)-1,atrCount)
    
  stop = FALSE
  while(!stop)
  {
    k = 2
    repeat{
      neigh = generateNeighbours(bestvect,k)
      yList = selectBest(neigh,indata,method,colName) #TODO
      yvect = yList[[1]]
      ybest = yList[[2]]
      #until
      if(ybest > best) #save found solution and start with with k = 1
      {
        best=ybest
        bestvect=yvect
        break
      }
      k = k + 2
      if(k > maxDist || k>2*atrCount)
      {
        #if inside it means that no y was better and maximum radius was reached
        stop = TRUE
        break
      }
    }
  }
  list(bestvect,best)
}

MOW = function (datacsv,method,strategy,strategyParam){
  workdata = datacsv[[1]]
  colNumber = datacsv[[2]]
  colName = datacsv[[3]]
  
  quality=c()
  atrCount=c()
  for(i in 1:(ncol(workdata)-1)) {
    wd=workdata
    result = strategy(wd,colName,i,strategyParam,method)
    atrCount=append(atrCount,i)
    quality=append(quality,result[[2]])
  }
  margin = 0.1*(max(quality)-min(quality))
  print((quality))
  print(min(quality))
  plot(atrCount,quality,
       ylim=c(min(quality),max(quality))) 
}


dataWCSV = readCsvData(winePath,"Alcohol")
dataICSV = readCsvData(irisPath,"Species")
dataBCSV = readCsvData(breastPath,colnr=2)

#print(RW(workdata,colName,3,10,NB))
#print(MMC(workdata,colName,2,10,DT))
#print(VNS(workdata,colName,2,6,DT))
