## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Below code will create a list of function that will help in caching the list

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x ##get the value of object x using the x$get() function
    setInverse <- function(solve) x_inv <<- solve ##set the x_inv to be the same as the function solve
    getInverse <- function() x_inv ##get the value of x_inv using the x$getInverse() function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) ## return all of the result as a list
}

## Write a short comment describing this function
## This function will get the Inverse of the matrix fed in the makeCacheMatrix 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    val <- x$get() ##get the matrix that will be inverse
    inv <- solve(val, ...) ##inverse the matrix that was inputted.
    x$setInverse(inv) 
    inv
}
