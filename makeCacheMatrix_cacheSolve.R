makeCacheMatrix <- function(om = vector()) { ##creating a function that takes an
    ##empty vector
    im <- NULL ##intializes 'im' as an object within makeCasheMatrix to be used later
    set <- function(aMatrix) { ##function that will: 
        om <<- aMatrix ##assigns 'aMatrix' to 'om' in the parent environment
        im <<- NULL ##clears any valid inversed matrix that is already cached
    }
    getom <- function() om ##retrieves 'om' from the parent environment
    setinv <- function(inv) im <<- inv ##assigns value of inv to 'im' following cacheSolve()
    getinv <- function() im ##Allows retrieval of inversed matrix later
    list( set = set, ##gives name 'set' to the set() function above
          getom = getom, ##gives the name 'getom' to the getom() function above
          setinv = setinv, ##gives name 'setinv' to setinv() above
          getinv = getinv) ##give name 'getinv' to getinv() above
}
cacheSolve <- function(om, ...) { ##function to populate or retrieve the inverted
    ##matrix from makeCacheMatrix
    im <- om$getinv() ##tries to retrieve a cahced inverse matrix from
    if(!is.null(im)) { ##if there is an inverse matrix it will return the following:
        message("getting chached data")
        return(im)
    }
    data <- om$getom() ##retrieves the matrix from makeCacheMatrix and assigns it to 'data'
    im <- solve(data, ...) ##solves the inverse of 'data' and assigns it to 'im'
    om$setinv(im) ##retrieves 'im' from makeCacheMatrix
    im ##returns 'im'
}