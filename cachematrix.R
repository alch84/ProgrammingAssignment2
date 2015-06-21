## The pair of functions below caches the inverse of a matrix 
## Caching the inverse of a matrix ensures computation is quicker 

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        minv<-NULL
        set<-function(y){
                x<<-y
                minv<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) minv<<-inverse
        getinverse<-function() minv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv<-x$getinverse()
        if(!is.null(minv)){
                message("getting cached data")
                return(minv)
        }
        data<-x$get()
        minv<-solve(data,...)
        x$setinverse(minv)
        return(minv)
}
