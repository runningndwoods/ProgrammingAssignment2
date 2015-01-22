## This set of functions takes a square, invertable matrix and returns the matrix
## to reduce computation time, it creates a cache of inverted matrix that is checked for pre-computed values

##The makeCacheMatrix() function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL        #creates an empty object
  set<-function(y){
    #set is an function object which sets the given value of y to the 
    #value of x and assigns m a NULL value
    #x and m are in the parent environment of set
    x<<-y
    m<<-NULL
  }
  get <- function() {return(x)} #returns the value of x, assigned to get variable
  setinverse <- function(solve) {m <<- solve} #computes the inverse matrix of m 
                                            #and assigns the new value to m 
                                            #m is in the parent environment of this function
  getinverse <- function() {return(m)}   #returns the new value of m calculated above
  list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)
    #all variables are assigned in a list and the list is returned
}


##The cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', where x is a list created by makeCacheMatrix
        ## If the inverse has already been calculated (and the matrix has not changed), 
  ##then the cachesolve should retrieve the inverse from the cache.
  m<-x$getinverse() #retrieve and chace from makeCaheMatrix()
  if(!is.null(m)){ #if m already has a cached value it is retreived
    message("getting cached data")
    return(m)
  }
  mx<-x$get()     #assigns mx the value of the argument given to the function makeCacheMatrix()
  m<-solve(mx,...) #assigns m the value of the inverse matrix of mx
  x$setinverse(m)  #sends the value of m back to the cache created by makeCacheMatrix()
  m               #returns the value of the inverse matrix
}
