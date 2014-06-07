require(pvclust)
require(mclust)
require(fpc)

columnCorrection = function(value,columnCount) {
    value - columnCount*0.00001;
}
inverseColumnCorrection = function(value,columnCount) {
    value + columnCount*0.00001;
}

#load data files functions
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])

rld = function(){source(paste(PATH,"/vns.R",sep=""))}
breastData = function(){read.csv(paste(PATH,"/breast2.csv",sep=""), header=FALSE)}
banknoteData = function(){read.csv(paste(PATH,"/kasa.csv",sep=""), header=T)}
irisData = function(){read.csv(paste(PATH,"/iris.csv",sep=""), header=T)}

#vec01 = vector of {0;1}
#convert vec01 to collection of column numbers
columnsToClusterNatural <- function (myvec) 
{
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
extractCols <- function (mydata, myvec) #
{
    mydata[,columnsToClusterNatural(myvec)]
}

MclustAlgo = function(mydata, nclust)
{
    x<-Mclust(mydata,G=nclust)
    #y<-MclustDR(x)
    x$class
}

kmeansAlgo = function(mydata, nclust)
{
    x<-kmeans(mydata,nclust)
    x$cluster
}

quality = function(mydata,myvec, mygroups,kclusters,clusterAlgo)
{
    mydataFiltered = extractCols(mydata,myvec)
    columnCount = ncol(mydataFiltered)
    if(is.null(columnCount))
    {
        columnCount = 1 #takie rzeczy to tylko w eRze
    }
    if(columnCount == 0 )
    {
        return (-1)
    }
    c1out = clusterAlgo(mydataFiltered,kclusters)
    randindex = cluster.stats(dist(mydataFiltered), c1out,mygroups)$corrected.rand
    columnCorrection(randindex,columnCount)
}

generateAllPerm = function(k)
{
    vec01 <- array(dim=k,data=0)
    returnvec = array(0,c(k,2^k))
    veccount = 2
    for(kk in seq(1,k))
    {
        x = combn(k,kk)
        for(i in seq(1,ncol(x)))
        { 
            newvec = vec01
            for(j in seq(1,nrow(x)))
            {
                newvec[x[j,i]] = (1 - vec01[x[j,i]])
            }       
            #append vector
            returnvec[,veccount] = newvec
            veccount = veccount + 1
        }
    }
    returnvec
}

generateNeighbours = function(vec01, k)
{
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
        #append vector
        returnvec[,veccount] = newvec
        veccount = veccount + 1
    }
    returnvec
}

selBest = function(neigh,mydata,mygroups,kclusters,clusterAlgo)
{
    bestIndex = 0
    bestScore = -1
    for(i in seq(1,ncol(neigh)))
    {
        score = quality(mydata, neigh[,i], mygroups, kclusters, clusterAlgo)
        cat("        ",neigh[,i], " - score is ",score,"\n")
        if(score > bestScore)
        {   
            bestScore = score
            bestIndex = i
        }        
    }
    list(neigh[,bestIndex],bestScore)
}

#example usage
#mydata:
#1, 200, 75
#0, 175, 90
#1, 180, 91
#0, 177, 86
#mygroups: [1,2,1,2]
#K - VNS param: 3 (global search)
#clusterAlgo: kmeansAlgo
findParams = function(mydata,mygroups, K, clusterAlgo)
{
    kclusters = length(unique(mygroups))
    # x is vec01
    # init vector of length equal to number of data columns
    #it could be random for non deterministic results
    x <- array(dim=ncol(mydata),data=0)
    xscore = quality(mydata,x, mygroups,kclusters,clusterAlgo)
    
    cat("findParams started.\n Init vector is: ",x," and its value is: ", xscore,"\n")
    
    #select best => we've got only one vector: vec01
    stop = FALSE
    while(!stop)
    {
        k = 1
        bestyscore = -2 #lower bound of quality function
        repeat{
            cat("    looking for neighbours with distance ",k,"\n")
            
            #construct whole neighbourhood as vector of vec01
            neigh = generateNeighbours(x,k)

            yList = selBest(neigh,mydata,mygroups,kclusters,clusterAlgo)
            y = yList[[1]]
            yscore = yList[[2]]
            cat("    best y found is: ",y," and its value is: ", yscore,"\n")
        #until
            if(yscore > xscore) #save found solution and start with with k = 1
            {
                break
            }
            if(k == K)
            {
                #if inside it means that no y was better and maximum radius was reached
                stop = TRUE
                break
            }
            k = k + 1
        }
        cat("end of loop:\n")
        if(yscore > xscore) #need to check in case of algorithm last iteration case: global maximum found and it's x, no y will be better.
        {
            x = y 
            xscore = yscore
            cat("x changed to: ",x," with value is: ", xscore,"\n")
        }
    }
    cat("len is: ",length(columnsToClusterNatural),"\n")
    
    xscore = inverseColumnCorrection(xscore,length(columnsToClusterNatural))
    
    list(x,xscore)
}

kasaKmeans = function(){
    datacsv = banknoteData()
    groups = datacsv[,1]
    extData = datacsv[,2:ncol(datacsv)]
    extData = scale(extData)
    
    K = 2
    
    findParams(extData,groups, K, kmeansAlgo)
}

kasaMclust = function(){
    datacsv = banknoteData()
    groups = datacsv[,1]
    extData = datacsv[,2:ncol(datacsv)]
    extData = scale(extData)
    
    K = ncol(extData)
    
    findParams(extData,groups, K, MclustAlgo)
}

kasatestq = function(){    
    datacsv = banknoteData()
    groups = datacsv[,1]
    extData = datacsv[,2:ncol(datacsv)]
    extData = scale(extData)
    
    quality(extData,c(0,0,0,1,0,1),groups,2, kmeansAlgo)
}

kasatestbrute = function(){    
    datacsv = banknoteData()
    groups = datacsv[,1]
    extData = datacsv[,2:ncol(datacsv)]
    extData = scale(extData)
    
    neigh = generateAllPerm(ncol(extData))
    selBest(neigh,extData,groups,2,kmeansAlgo)
}

irisKmeans = function(){
    datacsv = irisData()
    groups = datacsv[,5]
    extData = datacsv[,1:4]
    extData = scale(extData)
    
    K = 2
    
    findParams(extData,groups, K, kmeansAlgo)
}

irisMclust = function(){
    datacsv = irisData()
    groups = datacsv[,5]
    extData = datacsv[,1:4]
    extData = scale(extData)
    
    K = 2
    
    findParams(extData,groups, K, MclustAlgo)
}

iristestbrute = function(){    
    datacsv = irisData()
    groups = datacsv[,5]
    extData = datacsv[,1:4]
    extData = scale(extData)
    
    neigh = generateAllPerm(ncol(extData))
    selBest(neigh,extData,groups,3,kmeansAlgo)
}

iristestbruteM = function(){    
    datacsv = irisData()
    groups = datacsv[,5]
    extData = datacsv[,1:4]
    extData = scale(extData)
    
    neigh = generateAllPerm(ncol(extData))
    selBest(neigh,extData,groups,3,MclustAlgo)
}

breastKmeans = function(){
    datacsv = breastData()
    groups = datacsv[,2]
    extData = datacsv[,3:32]
    extData = scale(extData)
    
    K = 2
    
    findParams(extData,groups, K, kmeansAlgo)
}

breastMclust = function(){
    datacsv = breastData()
    groups = datacsv[,2]
    extData = datacsv[,3:32]
    extData = scale(extData)
    
    K = 1
    
    findParams(extData,groups, K, MclustAlgo)
}

breastq = function(){
    datacsv = breastData()
    groups = datacsv[,2]
    extData = datacsv[,3:32]
    extData = scale(extData)
    
    quality(extData,
      c(0,1,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,
        0,0,0,1,1,0,0,0,0,0),groups,2, MclustAlgo)
}

#kasa test
lkk = function(){rld();kasaKmeans()}
lkm = function(){rld();kasaMclust()}
lkq = function(){rld();kasatestq()}
lkbk = function(){rld();kasatestbrute()}

#iris test
lik = function(){rld();irisKmeans()}
lim = function(){rld();irisMclust()}
libk = function(){rld();iristestbrute()}
libm = function(){rld();iristestbruteM()}

#breast test
lbk = function(){rld();breastKmeans()}
lbm = function(){rld();breastMclust()}
lbq = function(){rld();breastq()}