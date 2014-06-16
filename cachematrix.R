## Following two functions calculates inversed matrix and save in a cache
## in preparation for next use.

## It creates a list of functions and posesses the result of inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        r_mat <- NULL
        set <- function(y){
                x <<- y
                r_mat <<- NULL
        }
        get <- function()       x
        setMat <- function(revMat)      r_mat <<- revMat
        getMat <- function()   r_mat
        list(set = set, get = get, 
             setMat = setMat,
             getMat = getMat)
}


## If the inversed matrix has already calculated, the result will be passed 
## from "cache" with a comment 'getting cached data'
## otherwise, it calculate inversed matrix using function 'solve()'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getMat()
        if(!is.null(r)){
                message("getting cached data")
                return(r)
        }
        m <- x$get()
        r <- solve(m, ...)
        x$setMat(r)
        r
}
