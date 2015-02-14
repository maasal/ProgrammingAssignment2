## The first function transforms an invertible square matrix to a list that can
## handle cached values. The second function uses this list to calculate the invers
## if it has not been calculated already in which case it returns a cached value.

## First transform the matrix using this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function(){
        return(x)
    }
    setinv <- function(calculatedInvMatrix){
        inv <<- calculatedInvMatrix
    }
    getinv <- function(){
        return(inv)
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Use this function to calculate the output of makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inversematrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
    return(inv)
}
