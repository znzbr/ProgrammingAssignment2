## Creates cache for matrix
makeCacheMatrix <- function(x = matrix()) {
    #init inverse matrix with null
    inv <- NULL
    
    #set new matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #get matrix
    get <- function() x
    
    #set inverse matrix
    setinv <- function(inverse) inv <<- inverse
    
    #get inverse matrix
    getinv <- function() inv
    
    #resulting list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Get inverse matrix from cache or calculate it if cache not exist yet
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
