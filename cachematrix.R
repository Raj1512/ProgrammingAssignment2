## Let's create a function call makeCacheMetrics which will take matrix as argument
## This first function, makeCacheMetrix() creates a special "Matrix", which is really a list containing a function to

## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse
## get the value of the Inverse


makeCacheMatrix <- function(x = matrix()) {
  
  
  ## First we have to create a Cache Variable and initialize it to NULL
  
          mCache <- NULL
  
  ## Since we want to use this cached Matrix inside this function so we will need to create a matrix 
  ##in working environment of this function only
  
    set <- function(y)
      {
            x <<- y
             mCache <<- NULL
      }
  
  ## Get the Value of Cached Matrix
  
    get <- function() x
  
  ## Now let's capture inverse of this matrix
  
      setInverse <- function(inverse)
     
       mCache <<- inverse
  
  ## Get inverted Matrix from the Cache
  
      getInverse <- function() mCache
  
  ## Now let's create a list to store the value inside the function
  
     list(set = set, get = get, setinverse = setInverse, getinverse = getInverse)
}

## Now let's create a new function which is going to use the Cached Matrix information from the function above
## The following function calculates the inverse of the 2 by 2 invertible Metrix created
## with the above function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
 ## via the setinverse function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of matrix 'x'
  
    mCache <- x$getinverse()
    
 ## 3 If mCache is  not null then return the previously cached result set.
 
    if(!is.null(mCache)) {
      
    message("Getting  Previously Cached Data!")
    
    return(mCache)
    
  }
 
  data <- x$get()
 
  mCache <- solve(data, ...)
 
  x$setinverse(mCache)
  
  mCache
}
## Testing the function
## load the R code
## source(""Your working Directory"/CacheMetrixRw3.R')
## b<- makeCacheMatrix()
## metR <- matrix(c(6,7,8,9), 2,2)
## b$set(metR)
## cacheSolve(b)
##cacheSolve(b)
##      [,1] [,2]
##[1,] -4.5    4
##[2,]  3.5   -3
## cacheSolve(b)
##getting  previously cached data
##      [,1] [,2]
##[1,] -4.5    4
##[2,]  3.5   -3
