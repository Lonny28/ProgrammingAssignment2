## This is programming assignment 2, where I will attempt to modify
## two functions.  The first will set up the cache for the matrix,
## the second will either solve the matrix and store it, or pull the
## soution if it has already been solved.

## here is how I tested my code to make sure it works - I used this test matrix:
Test<-matrix(c(9,1,3,6,13,11,7,0,5,7,4,7,2,6,1,10),nrow=4,ncol=4)
## I typed: solvetest<-makeCacheMatrix(Test)
## then: cacheSolve(solvetest)
## then: cacheSolve(solvetest)
## the first time it returned the solution, the second it said:
## "Getting Cached Data" before returning the same solution


## Here is the first function, called makeCacheMatrix
## I'm just taking the example from the assignment and changing
##numeric to matrix, and mean to solve (and of course the name of the function)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        ## for each row in the list, these four items get stored
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The second function cache solve follows.
## Same thing here, just taking the example and using solve instead of mean

cacheSolve <- function(x, ...) {
        ## grab whtever is in the getsolve slot
        m <- x$getsolve()
        ##if it's not null, then return whatever is in there and be done
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##otherwise, solve it, stick it in x$setsolve and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
