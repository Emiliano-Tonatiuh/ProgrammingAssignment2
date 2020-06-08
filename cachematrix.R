

## Write a short comment describing this function

#Esta función calcula la matriz inversa de una matriz dada 

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  est <- function(y){ x <<- y 
  inversa <<- NULL}
  obt <- function() x
  estInv <- function(solveMatrix) inversa <<- solveMatrix  #Aquí se calcula la matriz inversa
  obtInv <- function() inversa 
  list(estInv = estInv, obtInv = obtInv,obt = obt,est = est)
}



#Esta función calcula la matriz inversa de la proveniente de la función de arriba
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
