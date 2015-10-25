## Programing assignment 2

makeCacheMatrix <- function(d = matrix()) {
  m <- NULL
  set <- function(e) {
    d <<- e
    m <<- NULL
  }
  get <- function() d
  setinvert <- function(mx) m <<- (mx)
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


cacheSolve <- function(d, ...) {
  r <- d$getinvert() ## look for cached result
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  ## no cached result, hence set for initial invocation
  data <- d$get()
  r <- solve(data)
  d$setinvert(r)
  return(d$getinvert())
}

## Example of function usage
m9 <-matrix(1:4,2,2) 
m10<-cacheSolve(makeCacheMatrix(m9))
