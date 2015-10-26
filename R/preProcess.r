
#' Removes columns with high multicollinearity automatically based on VIF.
#' Can also include list of columns to keep, but these will be added
#' at the end of the consecutive search so it'll retain any multicollinearity.
#' 
#' @param df data frame
#' @param vifCutoff VIF cutoff to use (default = 4)
#' @param colsToKeep columns to manually keep
#' @export
removeHighVIF <- function (df, vifCutoff=4, colsToKeep=NULL) {
	require(car)
	require(dplyr)
	require(data.table)
	
	isDT = "data.table" %in% class(df) 
	
	## Need to convert to data.frame 
	dftemp = df %>% data.frame()
	
	dftemp$Y=1
	repeat
	{
		mod=lm(Y~., data=dftemp)
		
		coefs=coef(mod)
		if (any(is.na(coefs))){
			dftemp = dftemp %>% select(-one_of(names(coefs)[is.na(coefs)]))
			mod=lm(Y~., data=dftemp)
		} 
		
		
		result <- data.frame(GVIF=suppressWarnings(vif(mod)))
		result=result[order(result$GVIF, decreasing=T),,drop=F]
		
		if (result[1,1]<vifCutoff) break #breaks repeat loop
		
		nm=rownames(result)[1]
		cat(paste("Removing ",nm," with VIF=",result[1,1],"\n",sep=""))
		dftemp = dftemp %>% select(-contains(nm))
	}
	
	
	nms = unique(c(names(dftemp), colsToKeep))
	df = df %>% select(one_of(nms))
	
	if (isDT) df = df %>% data.table()
	df
}





#' This is rip of caret function: just looks at % of most frequent, not 
#' relative to the second most frequent
#' 
#' @param df data frame
#' @param freqCutoff minimum % of the most common value (simple tweak of caret function)
#' @param colsToKeep vector of column names that should not be removed 
#' @export
removeZeroVar <- function(df, freqCutoff=0.95, colsToKeep=NULL) {
	require(caret)
	require(dplyr)
	require(data.table)
	
	isDT = "data.table" %in% class(df) 
	
	## Need to convert to data.frame 
	if (isDT) df = df %>% data.frame()
	
	
	## This is rip of caret function: just looks at % of most frequent, not
	##  relative to the second most frequent
	freqRatio <- apply(df, 2, function(data) {
				t <- prop.table(table(data[!is.na(data)]))[1]
			})
	nms = names(freqRatio[freqRatio>freqCutoff])
	if (!is.null(colsToKeep)) nms = nms[which(!(nms %in% colsToKeep))] # remove some of the columns to remove
	df=df %>% select(-one_of(nms))  
	
	if (isDT) df = df %>% data.table()
	df
}





#' Convert specified columns in a dataset to binary 0 or 1.
#' 
#' @param df data frame
#' @param vifCutoff VIF cutoff to use (default = 4)
#' @export
convToBinary <-function(df, colNms) {
	require(dplyr)
	require(data.table)
	
	## Need to convert to data.frame 
	isDT = "data.table" %in% class(df) 
	if (isDT) df = df %>% data.frame()
	
	binaryIdx = which(names(df) %in% colNms )
	df[,binaryIdx]=data.frame(lapply(df[,binaryIdx],function(x) ifelse(x>0,1,0)))
	
	if (isDT) df = df %>% data.table()
	df
	
}


#' Dummy code the specified columns in a dataset.  The returned
#' columns will be of the form colNm1_val1, colNm1_val2, etc
#' 
#' @param df data.frame or data.table
#' @param colNms a list of column names to dummy code
#' @export
convToDummy <- function(dfOrig, colNms) {
	require(dplyr)
	require(data.table)
	
	
	
	isDT = "data.table" %in% class(df) 
	if (!isDT) df = df %>% data.table()
	
	# make a copy so leaves the original as is
	df=data.table(data.frame(dfOrig))
	
	
	for (colNm in colNms) {
		uniVals = as.character(unlist(unique(df %>% select(one_of(colNm)))))
		uniValsColNms = paste(colNm,"_",gsub("[[:space:]/,&()-.]","",uniVals), sep="")
		
		df[,(uniValsColNms):=lapply(uniVals,function(x)(get(colNm)==x)*1)]
		
	}
	
	if (!isDT) df = df %>% data.frame()
	df %>% select(-one_of(colNms))
	
}











#' Simple balancing of classes for multinomial. If up-sample, do with 
#' replacement, if down sample no replacement.
#' 
#' @param df data frame
#' @param target target name of the dataset
#' @param classSize the number of observations in each target class
#' @export
balanceClasses <- function(df, target, classSize){
	require(dplyr)
	
	tab=table(df %>% select_(target))
	
	dfls=list()
	for (i in 1:length(tab)) {
		dfls[[i]]=eval(substitute(filter(df, target == names(tab)[i]), list(target = as.name(target))))
		len=length(dfls[[i]][,1])
		
		if (len==classSize) {
			dfls[[i]]=dfls[[i]]
		} else if (len<classSize) {
			# add new observations
			newObs=dfls[[i]][base::sample(len, classSize-len, replace=TRUE),]
			dfls[[i]]=rbind(dfls[[i]], newObs)
		} else {
			dfls[[i]]=dfls[[i]][base::sample(len, classSize, replace=FALSE),]
		}
	}
	do.call("rbind", dfls)
}



#' Get's the basic summary stats for a data set.
#' 
#' @param df data.frame or data.table
#' @export
getSummaryTable <- function(df) {
	require(dplyr)
	
	df = df %>% dplyr::ungroup() %>% data.frame()
	
	## Remove factor or character variables
	gdNms = names(df[, !sapply(df, function(x) is.character(x) | is.factor(x) ), drop=FALSE])
	df= df %>% dplyr::select(one_of(gdNms))
	
	
	minFnx=function(x) min(x, na.rm=T)
	maxFnx=function(x) max(x, na.rm=T)
	meanFnx=function(x) mean(x, na.rm=T)
	quant1Fnx=function(x) quantile(x, probs=c(0.25), na.rm=T)
	medianFnx=function(x) median(x, na.rm=T)
	quant3Fnx=function(x) quantile(x, probs=c(0.75), na.rm=T)
	missingFnx = function(x) length(x[is.na(x)])
	
	summy = rbind(
			df %>% dplyr::summarize_each(funs(minFnx)),
			df %>% dplyr::summarize_each(funs(quant1Fnx)),
			df %>% dplyr::summarize_each(funs(medianFnx)),
			df %>% dplyr::summarize_each(funs(meanFnx)),
			df %>% dplyr::summarize_each(funs(quant3Fnx)),
			df %>% dplyr::summarize_each(funs(maxFnx)),
			df %>% dplyr::summarize_each(funs(missingFnx)))
	
	summy = data.frame(names(summy), t(summy), stringsAsFactors=FALSE)	
	names(summy)=c("Var","Min","LowQuart","Median","Mean","UpQuart","Max","NumNA")
	rownames(summy)=NULL
	summy	
}