## The following functions calculate the inverse of a square matrix and stores it in cache
## so that it can be retrieved from there without having to calculate it again, given that it doesn't change.

##square matrix examples and usage
## mat1 <- matrix(c(4, 3, 3, 2), 2, 2)
## - or -
## mat1 <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), 3, 3)
## myMat <- makeCacheMatrix(mat1)
## cacheSolve(myMat)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    #create an empty matrix object
    inv <- NULL
    
    #Set the value of the matrix
    # the << operator which can be used to assign a value to an object 
    # in an environment that is different from the current one
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #get the value of the matrix
    get <- function() x
    
    #set the value of the inverse in another environment
    setInv <- function(solve) inv <<- solve
    
    #get the value of the inverse
    getInv <- function() inv
    
    #return list of functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    
    #Determine if inverse has already been calculated
    if(!is.null(inv)) {
        message("Getting cached inverse data")
        return(inv)
    }
    
    #get the value of the matrix
    data <- x$get()
    
    #calculate the ivnerse and set it to cache
    inv <- solve(data, ...)
    x$setInv(inv)
    
    #return the inverse
    inv
}









