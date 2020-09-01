## This assignment is to write a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special matrix object "mat" that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {

    inverse <- NULL
    
    #Create / set matrix, set inverse to NULL
    set <- function(vmat) {
        mat <- vmat
        inverse <- NULL
    }
    
    #Retrieve / get matrix
    get <- function() mat
    
    #Set Inverse matrix
    setinverse <- function(i) inverse <- i
    
    #Retrieve the Inversed matrix
    getinverse <- function() inverse
    
    #Create list of functions to set and get matrix and its inverse respectively
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special matrix "mat" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
# changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
    ## Return a matrix that is the inverse of 'mat'

    #Calculate inverse of the matrix
    inverse <= mat$getinverse()
    
    # Check if the inverse exists in the cache, if it does - return from cache
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    #This line executes only when the inverse is not cached. 
    #Here, get the matrix, calculate the inverse, set the inverse values and return inverse
    matrix <- mat$get()
    inverse <- solve(matrix, ...)
    mat$setinverse(inverse)
    
    inverse
}
