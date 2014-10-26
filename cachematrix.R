## Matrix Inversion is ususally a costly computation and their may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. The following pair of functions will be used to cache
## the given inverse of matrix.


## The function "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse by a list containing a function to
## 1. set the matrix value
## 2. get the matrix value
## 3. set the inverse of the matrix
## 4. set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) i<<-inverse
    getinv<-function() i
    list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)

}


## The function "cacheSolve" computes the inverse of the given matrix.
## It checks first if the inverse of the matrix already been calculated 
## and exists in cache or not. If exists, then this function simply
## gets the value from cache without doing the calculation and return the result. 
## If not in the cache, then computes the inverse and sets the value in cache
## and return the value.
## 

cacheSolve <- function(x, ...) {
    i<-x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data, ...)
    x$setinv(i)
    return(i)
}

