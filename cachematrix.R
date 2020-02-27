##The purpose of this assignment is to create 2 functions which allows us to cache the inverse of a matrix. 

## This first function creates a special "matrix", which is a list containing a function to:
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse
# 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
    set<- function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setinverse<- function(inverse) i<<-inverse
    getinverse<- function() i
    list(set=set, get= get, setinverse=setinverse, getinverse=getinverse)

}


## The second function calculates the inverse of the special "matrix" created before.
##It checks if the inverse has already been calculated. If so, it gets the inverse from the cache
#and skips the computation. If not, it calculates the inverse of the data and sets the value in the cache.

cacheSolve <- function(x, ...) {
    i<- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
    i
}

