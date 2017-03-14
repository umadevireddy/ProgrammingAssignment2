## Given an invertible matrix , the below two functions either create the inverse of the matrix or retrieve the inverse 
## from the cache.


## "makeMatrix” creates a  “matrix” object that can cache its inverse
## "makeMatrix” function returns a list of four functions set,get,setinverse,getinverse

makeMatrix <- function(x = matrix()) {

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


## The below “cacheSolve” function derive the inverse of the matrix returned by "makeMatrix” function
## If the inverse has been computed already then it should retrieve the inverse from cache

cacheSolve <- function(x, ...) {
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

