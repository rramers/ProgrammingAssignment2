## The purpose of these functions is cache the inverse of a matrix so that it does not have to be computed

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x<<- y
                matinv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) matinv <<- inverse
        getInv <- function() matinv
        list(set=set,get = get,setInv = setInv,getInv = getInv)
}


## This function computes the inverse of the special matrix object returned from above

cacheSolve <- function(x, ...) {
        matinv <- x$getInv()
        if(!is.null(matinv)) {
                message("...working")
                return(matinv)
        }
        mymatrix <- x$get()
        matinv <- solve(mymatrix, ...)
        x$setInv(matinv)
