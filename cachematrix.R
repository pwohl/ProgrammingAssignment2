#################################################################
## This is a pair of functions to cache the inverse of a matrix##
#################################################################


##The first function "makeCacheMatrix" creates a special "matrix" 
##object that can cache its inverse, in more detail, it is a 
##list that contains a function to:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvers <- function(solve) inv <<- solve
        getinvers <- function() inv
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)

}


##The cacheSolve function calculates the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has
##already been calculated (and the matrix has not changed), then 
##the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
