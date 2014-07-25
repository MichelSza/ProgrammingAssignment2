## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
##1.  set the matrix
##2.  get the matrix
##3.  set the inverse of the matrix
##4.  get the inverse of the matrix
makeCacheMatrix <- function(mat = matrix()) {
		matInv <- NULL 
		set <- function(y) {
				mat <<- y
				matInv <<- NULL
		}
		get <- function() mat
		
		## store the inverse of the matrix in the global value matInv
		setinv <- function(inverse) matInv <<- inverse 
		getinv <- function() matInv
		
		# return the vector of functions
		list(set = set, 
			get = get,
			setinv = setinv,
			getinv = getinv)
    }


## `cacheSolve`: This function computes the inverse of the special
##    "matrix" returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##    `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matInv <- x$getinv()
		## if matInv is not null, return the previously cached inversed matrix
		if(!is.null(matInv)) {
				message("getting cached data")
				return(matInv)
		}
		
		## compute the inverse of the matrix, store the matrix and return the matrix
		data <- x$get()
		matInv <- solve(data)
		x$setinv(matInv) 
		matInv
    }
