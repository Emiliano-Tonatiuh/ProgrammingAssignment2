## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  est <- function(y){ x <<- y 
  inversa <<- NULL}
  obt <- function() x
  estInv <- function(solveMatrix) inversa <<- solveMatrix
  obtInv <- function() inversa
  list(estInv = estInv, obtInv = obtInv,obt = obt,est = est)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## REGRESA LA MATRIZ INVERSA DE  'X'
  inversa <- x$obtInv()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  data <- x$obt()
  inversa <- solve(data)
  x$estInv(inversa)
  inversa 
}
