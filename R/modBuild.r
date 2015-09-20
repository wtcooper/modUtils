
#' Simple fold index generator, all idx used,
#' randomly sorted.   
#' 
#' @param n dataset size
#' @param K number of folds
#' @return vector of folds as integers
#' @export
getCVFolds <- function(n, K) {
	rep(1:K, times=floor(n/K+1))[1:n][sample(1:n)]
}


