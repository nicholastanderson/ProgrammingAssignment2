##The ultimate result of these two functions is being able to
##Call "cacheSolve" instead of "solve" to utilize cacheing. This 
##would be useful if you will often be re-computing the inverse
##of the same matrix.

## This function handles functionality of the cache, namely,
## "setting" inverse data in the cache and "getting" that 
## inverse data from the cache.
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}



## This function first checks to see if the matrix is cached. 
## If it is cached, the function will return the chached 
## inverse.If it is not cached, it will compute the inverse
## of the matrix, cache that result, and then return that result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
