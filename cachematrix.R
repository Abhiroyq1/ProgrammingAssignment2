## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
            x<<-y
            i<<-NULL
        }
        get<-function() x
        setI<-function(inverse) i<<-inverse
        getI<-function() i
        list(set=set,get=get,setI=setI,getI=getI)
}


## Write a short comment describing this function
## Function cacheSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getI()
        if(!is.null(i)){                        ##checking if the inverse is calculated
            print("getting cached inverse matrix")
            return(i)
        }
        data<-i$get()
        i<-solve(data,...)
        x$setI(i)
        i
}
