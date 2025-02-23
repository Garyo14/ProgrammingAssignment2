## The following functions work together to cache the inverse of a matrix.
## Since matrix inversion can be computationally expensive, caching the result
## can improve performance when the inverse needs to be used multiple times.

## makeCacheMatrix: Creates a special "matrix" object that can store its inverse
## and provides functions to set and get the matrix and its inverse.

## cacheSolve: Computes the inverse of the special "matrix" object created
## by makeCacheMatrix. If the inverse has already been calculated and cached,
## it retrieves the cached result instead of recomputing it.

## makeCacheMatrix: Membuat objek matriks khusus yang bisa menyimpan inversnya
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Menyimpan nilai invers
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cache jika matriks berubah
  }
  get <- function() x  # Mengambil matriks
  setInverse <- function(inverse) inv <<- inverse  # Menyimpan invers
  getInverse <- function() inv  # Mengambil invers dari cache
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: Menghitung atau mengambil invers dari cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Mengambil dari cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)  # Menghitung invers
  x$setInverse(inv)  # Menyimpan ke cache
  inv
}
