## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   inve<-NULL
   set<-function(y){
     x <<- y
     inve<<- NULL
   }
   get <-function() x
   setinve<-function(inverse) inve <<-inverse 
   getinve <- function() inve
   list(set = set, get = get,
        setinve = setinve,
        getinve = getinve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inve <- x$getinve()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setmean(inve)
  inve
}
