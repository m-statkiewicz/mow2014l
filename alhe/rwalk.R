rwalkOdb<- function (iter)
{
    col <- array(1:iter)
    x <- runif(1,-10,10)
    for(i in seq(1,iter))
    {
        x <- x+runif(1,-1,1)
        if(x < -10) x <- -20 - x
        if(x > 10) x <- 20 - x
        
        col[i] <- x
    }
    print("rwalkOdb Done")
    col
}

rwalkRzut<- function (iter)
{
    col <- array(1:iter)
    x <- runif(1,-10,10)
    for(i in seq(1,iter))
    {
        x <- x+runif(1,-1,1)
        y <- x
        if(x < -10) y <- -10
        if(x > 10) y <- 10
        
        col[i] <- y

    }
    print("rwalkRzut Done")
    col
}

rwalkZawi<- function (iter)
{
    col <- array(1:iter)
    x <- runif(1,-10,10)
    for(i in seq(1,iter))
    {
        x <- x+runif(1,-1,1)
        if(x < -10) x <- x+20
        if(x > 10) x <- x-20
        
        col[i] <- x
    }
    print("rwalkZawi Done")
    col
}

rwalkRand<- function (iter)
{
    col <- array(1:iter)
    x <- runif(1,-10,10)
    for(i in seq(1,iter))
    {
        x <- x+runif(1,-1,1)
        if(x < -10) x <- runif(1,-10,10)
        if(x > 10) x <- runif(1,-10,10)
        
        col[i] <- x
    }
    print("rwalkZawi Done")
    col
}

testfun <- function(iter)
{
	set.seed(42)
	p1 <- hist(rwalkOdb(iter),breaks=50)                     # centered at 4
	p2 <- hist(rwalkZawi(iter),breaks=50)                     # centered at 6
	p3 <- hist(rwalkRand(iter),breaks=50)
	p4 <- hist(rwalkRzut(iter),breaks=50)
	plot( p3, col=rgb(0,0,1,1))  # first histogram
	plot( p4, col=rgb(1,1,0,1), add=T)  # second
	plot( p1, col=rgb(1,0,0,1), add=T)  # first histogram
	plot( p2, col=rgb(0,1,0,1), add=T)  # second


}

# c is collection in R

