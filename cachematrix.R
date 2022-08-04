## This function sets the value of a matrix, gets the value of a matrix,
## sets the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computed the inverse of a matrix. If the inverse has already
## calulated, then then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

A <- matrix(c(3, 2, 5, 2, 3, 2, 5, 2, 4), 3 ,3)
B <-makeCacheMatrix(A)
C <-cacheSolve(B)
print(A)
print(B)
print(C)
    

C2 <- cacheSolve(makeCacheMatrix(A))
print(C2)
