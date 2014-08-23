##makeCacheMatrix is a function which returns a list of FOUR other functions.
##These functions do the following (1) set a matrix (2) get a matrix
##(3) set an inverse of the matrix and (4) get an inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # define a matrix with same dimensions as x - but with NA values
  m<-matrix(,nrow=nrow(x),ncol=ncol(x))
    set<-function(y=matrix()){
       x<<-y
       m<<-matrix(,nrow=nrow(x),ncol=ncol(x))
    }
    get <- function() x
    setinverse <- function(inverse=matrix()) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
     }

## cacheSolve is a function which checks to see whether the inverse of
## a matrix has already been calculated.  If already calculated, the fctn
## skips the computation and returns the previously calculated inverse which is
## stored in a cache.  Otherwise, the function computes the
## inverse and stores it in the cache.
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  
  if(!is.na(m[1,1])) {
     message("Getting cached inverse")
     return(m)
   }
  
   data<-x$get()
   m<-solve(data) ##return a matrix that is the inverse of 'x'
   x<-x$setinverse(m)
   m
  }



  