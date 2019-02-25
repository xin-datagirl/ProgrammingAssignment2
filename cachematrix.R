## The makeCacheMatrix creates a special matrix that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  var_inv<-NULL
  set<-function(y){
    x<<-y
    var_inv<<-NULL
  }
  get<-function() x
  setinverse <- function(inverse) var_inv <<- inverse
  getinverse <- function() var_inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function returns the inverse of a matrix
## Before it does the computation, it checks if the inverse has been calculated 
## and stored in cache. If so, it retrieve the inverse from the cache. If not, it
## uses the 'setinverse' function to calculat the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  var_inv<-x$getinverse()
  if(!is.null(var_inv)){
    message("getting cached inverse")
    return(var_inv)
  }
  else{
    data<-x$get()
    var_inv<-solve(data)
    x$setinverse(var_inv)
    var_inv
  }
}
