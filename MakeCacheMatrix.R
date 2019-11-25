## P Programming
## Week 3, Assignment 2
## Aim is to make matrix inversion functions that
## cache previous results so that it saves time when called multiple times
## Note - doesn;t handle noninvertible matrices

## to invert matrix A, you need to:
## cacheSolve(makeCacheMatrix(A))



## makeCacheMatrix creates a special 'matrix', which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<- y
    inv<<- NULL
  }
  get <-function()x
  setinverse<-function(invertmatrix) inv <<- invertmatrix
  getinverse<-function() inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## CacheSolve calculates the inverse of the special 'matrix'
## created by makeCacheMatrix, first checking to see if already calculated
## If it has, it gets the inverse from the cache and skips the computation
## If not, it calculates the inverse and stores it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrixdata<-x$get()
  inv<-solve(matrixdata)
  x$setinverse(inv)
  return(inv)
}