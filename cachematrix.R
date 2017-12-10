## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # initializing the cache Matrix 'cacheMatrix'
    # assign the value NULL for the first initialization
        i<-NULL
    #defining the method of setting matrix as "set"
        set<-function(y){
            x<<-y
            i<<-NULL
        }
    #defining the method of getting matrix as "get"
        get<-function() x
    #function for setting the inverse of the matrix
        setI<-function(inverse) i<<-inverse
    #function for getting the inverse of the matrix
        getI<-function() i
    #listing aal the functions as list
        list(set=set,get=get,setI=setI,getI=getI)
}


## Write a short comment describing this function
## Function cacheSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getI()
        if(!is.null(i)){                        ##checking if the inverse is calculated and populating if calculated
            print("getting cached inverse matrix")
            return(i)
        }
        data<-x$get()                           ##if the inverse is not calculated
        i<-solve(data,...)
        x$setI(i)
        return(i)
}
