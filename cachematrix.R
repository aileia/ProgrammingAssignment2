## This assignment gives cache in the memory, if exist 
## 

# This function creates an object that can have cache inverse
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


## this function brings cached version of matrix inverse

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
