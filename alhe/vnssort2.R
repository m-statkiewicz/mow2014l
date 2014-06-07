vnssort <- function(x, K) {

    incol <- x
    col <- NULL

    for (i in seq (1, length(x))) {
        
        y <- vnssortstep(incol, K)
        col <- append(col, y)
        incol <- incol[-which(incol==y)]

    }

    col

}

vnssortstep <- function(x, K) {

    minv <- x[floor(runif(1, 1, length(x)+1))] 

    while(1) {
    
        k <- 1
    
        repeat {
                
            Y <- nbrdgen(x, which(x==minv), k)
            y <- min(Y)
            k <- k + 1  

            if (minv > y | k==K) break()
        }

        if(k==K) break()

        minv <- y
    
    }

    minv

}

nbrdgen <- function(col, minindex, k) {  # todo: use set

    minx <- ifelse(minindex-k >=1, minindex-k, 1)
    maxx <- ifelse(minindex+k <= length(col), minindex+k, length(col))
    outcol <- col[minx:maxx]

    outcol

}
