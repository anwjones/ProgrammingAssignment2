## The first function saves a matrix in its own namespace
## The second function checks if its inverse has been calculated.  If the
## inverse is already calculated then the function returns the previously 
## calculated inverse. If the inverse is not yet calculated then the 
## function calculates the inverse and saves it in the namespace
## as well as returning the inverse

## Creates a namespace and saves the passed matrix in the namespace
## Default value of the matrix to be set up is an empty matrix
makeCacheMatrix <- function(x = matrix()) {
    ii <- NULL                   #sets up location for the inverse matrix
                                 #in the evironment "closure" created for the code
    
    set <- function (y){         #define a set function within the namespace that can
                                 #use its parent closure for storing variables between
                                 #function calls (so called "static")
        x <<- y                  #passed value y assigned to matrix
        ii <<- NULL              #reset the inverse as x has changed so the
    }                            #old inverse is invalid
    
    get <- function() x          #get() called with no parameters returns the matrix
    
    setinv <- function(inv){     #setinv(inv) assigns the stored inverse (ii) to 
        ii <<- inv               #the parameter passed to the function
    }
    
    getinv <- function(){        #getinv called with no parameters returns
        ii                       #the saved inverse (ii)
    } 
 
    list(set = set, get = get,   #the makeCacheMatric function returns a list
         setinv = setinv,        #of the functions so x$setinv() now exists, etc
         getinv = getinv)

}


## This function uses the above functions to see if the matrix has changed since
## the last time its inverse was calculated.  If not then it returns the cached
## value. If it has changed or the inverse has not yet been calculated then it
## calculates, caches and returns the inverse.
cacheSolve <- function(x, ...) {
    if(is.null(x$getinv())) {
        #the inverse is not yet calculated
        print("Calculating Inverse")
        inv<-solve(x$get())     #calculate the inverse (assume invertible)
        x$set(x)                #cache the matrix
        x$setinv(inv)           #cache its inverse
        inv                     #return the inverse
    } else {
        #the inverse is still valid so just return it
        print("Retrieving Cached Inverse")
        x$getinv()
    }
}
