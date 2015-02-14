## The first function transforms an invertible square matrix to a list that can
## handle cached values. The second function uses this list to calculate the invers
## if it has not been calculated already in which case it returns a cached value.

## First transform the matrix using this function

makeCacheMatrix <- function(x = matrix()) {
    #Makes init values
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function(){ 
        return(x)
    }
    #Sets inverse matrix to cache
    setinv <- function(calculatedInvMatrix){
        inv <<- calculatedInvMatrix
    }
    #Returns the cached inverse matrix 
    getinv <- function(){
        return(inv)
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Use this function to calculate the output of makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    #Checks if already calculated the inverse matrix
    if(!is.null(inv)){
        message("getting cached inversematrix")
        return(inv)
    }
    matrix <- x$get()
    #calculates the inverse matrix and puts it in cache
    inv <- solve(matrix)
    x$setinv(inv)
    return(inv)
}
