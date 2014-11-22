## The functions below return cached values of an input matrix and its inverse matrix.


## makeCacheMatrix reads and keep the value of the input matrix x.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() { x }
  
  setinv <- function(solve) {m <<- solve }
  
  getinv <- function() { m }
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv )
  
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x) {
        
  m <- x$getinv()               # accesses the object 'x' and gets the value of the inverse
  
  if(!is.null(m)) {              # if mean was already cached (not NULL)
    
    message("getting cached data")
    
    return(m)                       # return the inverse
    #   the function cachemean(), note
  }
  
  data <- x$get()        
  
  m <- solve(data)   # if m was NULL then we have to calculate the inverse
  
  x$setinv(m)         
  
  m                      
  
}
