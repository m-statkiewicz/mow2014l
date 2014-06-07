frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
breastData = function(){read.csv(paste(PATH,"/breast.csv",sep=""), header=FALSE)}
wineData = function(){read.csv(paste(PATH,"/wine.csv",sep=""), header=FALSE)}
irisData = function(){read.csv(paste(PATH,"/iris.csv",sep=""), header=FALSE)}


#irisKmeans = function(){
  datacsv = irisData()
  groups = datacsv[,5]
  extData = datacsv[,1:4]
  extData = scale(extData)
  
  K = 2
  
#}

