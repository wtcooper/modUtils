
#' Create some folders for output
#' 
#' @param path the filepath for folder to create
#' @param replc logical for whether to replace the folder 
#' @export
make_path <- function(path, replc=FALSE) {
	if (file.exists(path)) {
		if (replc) {
			file.remove(path)
			unlink(path, recursive=TRUE)
			dir.create(paste0(path,"/",sep=""),recursive=TRUE, showWarnings=FALSE)
		}
	} else {
		dir.create(paste0(path,"/",sep=""),recursive=TRUE, showWarnings=FALSE)
	}
}




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


