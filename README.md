## RProgrammingAssignment2

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,
       setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x

cacheSolve <- function(x, ...) 
{
  if(require("corpcor")){
    print("corpcor is loaded correctly")
  } else {
    print("trying to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("matrix is in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if exist) is gonna be computed")
  data <- x$get()
  inverse <- pseudoinverse(data, ...)
  x$setinverse(inverse)
  inverse
}

# square matrix
x <- matrix(rpois(25,4), nrow = 5)
cx <- makeCacheMatrix(x)
cx$get()
cacheSolve(cx)
cacheSolve(cx)
invX <- cacheSolve(cx)

# rows > cols
y <- matrix(rpois(18,2), nrow = 4, ncol = 3)
cy <- makeCacheMatrix(y)
cy$get()
cacheSolve(cy)
cacheSolve(cy)
invy <- cacheSolve(cy)

# rows < cols
z <- matrix(rpois(23,2), nrow = 4, ncol = 5)
cz <- makeCacheMatrix(z)
cz$get()
cacheSolve(cz)
cacheSolve(cz)
invz <- cacheSolve(cz)

# multiplication must return identity or closer
invx %*% x
x %*% invx
invy %*% y 
z %*% invz 
