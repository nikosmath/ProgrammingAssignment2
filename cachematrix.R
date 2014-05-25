## The first function store the input matrix and also creates a space
## to cache the inverse matrix.
## The second function returns the inverse matrix. If is already calculated
## then returns it from the cache. If not calculates it, cache it and return it

## The first function, makeCacheMatix creates a special "matix", 
## which is really a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse matrix
# 4) get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        invMat<-NULL
        
        # Set the matrix
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        
        # Get the matrix
        get <- function() x
        
        # Set the inverse matrix
        setinverse <- function(inverse) invMat <<- inverse
        
        # Get the inverse matrix
        getinverse <- function() invMat
        
        # Create and return the list with the above defined functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## Returns the cached inverse of the given matrix if already been calculated, 
## otherwise calculates the inverse matri
cacheSolve <- function(x, ...) {
        
        # Get the inverse matrix        
        invMat <- x$getinverse()
        
        # Check if the matrix is cached 
        if(!is.null(invMat)){
                
                # Return the inverse matrix and a message that this comes from cached memory
                message("getting cached data")
                return(invMat)
        }
        
        # Take the matrix data
        data<-x$get()
        
        # Check if the matrix is square
        if(dim(data)[1]==dim(data)[2]){
                # Calculate the inverse matrix
                invMat<-solve(data,...)
                
                # Cache the inverse matrix
                x$setinverse(invMat)
                
                # Return the inverse matrix
                invMat
        }
        else{
                
                message("the matrix is not Square")
        }
}