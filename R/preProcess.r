
#' Does some basic cleaning of the dataset (imputation, zero variance, VIF for multicollinearity,
#' or transformations).  Note: imputation and transformations done using caret::preProcess,
#' VIF done using car::vif, and zero inflation done using a modified version of caret's zeroinf function.
#' The VIF function is slow, but not sure how to speed up unless using a multithreaded version
#' of covariance functions used internally by car::vif(), maybe MS/Revolution R Open would do this, 
#' haven't tried though.
#' 
#' @param df data frame
#' @param cleanFnx list of cleaning/preprocessing to do, defaults: c("impute", "zeroVar", "vif", "transform")
#' @param transType c("center","scale") (default), also can do 'Range', 'YeoJohnson', 'BoxCox', others
#' @param imputeType "medianImpute" (default), "knnImpute", may be others (see ?caret::preProcess)
#' @param freqCutoff minimum % of the most common value (simple tweak of caret function)
#' @param vifCutoff variance inflation factor cutoff level (default=10, which is low threshold)
#' @param colsToKeep vector of column names that should not be removed via VIF
#' @examples
#' data(iris)
#' doBasicDataClean(iris)
#' @export
getBasicCleanData <- function(df, cleanFnx = c("impute", "zeroVar", "vif", "transform"), 
		transType=c("center", "scale"), imputeType="medianImpute", freqCutoff=0.95, vifCutoff=10, colsToKeep=NULL) {
	require(caret)
	
	returnList = list()
	
	isDT = "data.table" %in% class(df) 
	
	## Need to convert to data.frame 
	if (isDT) df = df %>% data.frame()
	
	
	if ("zeroVar" %in% cleanFnx) {
		cat("Zero variance check of dataset\n")
		
		## This is rip of caret function: just looks at % of most frequent, not
		##  relative to the second most frequent
		freqRatio <- apply(df, 2, function(data) {
					t <- prop.table(table(data[!is.na(data)]))[1]
				})
		nms = names(freqRatio[freqRatio>freqCutoff])
		if (!is.null(colsToKeep)) nms = nms[which(!(nms %in% colsToKeep))] # remove some of the columns to remove
		if (length(nms)>0) df=df %>% select(-one_of(nms))  
	}
	
	
	if ("vif" %in% cleanFnx) {
		cat("VIF check of dataset\n")
		
		## Remove high multicollinearity via VIF variables (instead of correlations below)
		dftemp = removeHighVIF(df, vifCutoff)
		nms = unique(c(names(dftemp), colsToKeep))
		df= df %>% select(one_of(nms))
	}
	
	
	# Rescale them all after done removing all other filters
	if ("transform" %in% cleanFnx) {
		cat(paste("Transforming dataset using methods",transType, "\n"))
		datScale=preProcess(df[, !sapply(df, is.factor)], method=transType)
		df[, !sapply(df, is.factor)]=predict(datScale, df[, !sapply(df, is.factor)])
		returnList[["Scale"]] = datScale
	}
	
	
	if ("impute" %in% cleanFnx) {
		cat("Imputing dataset\n")
		### Impute the training data
		datImpute=preProcess(df[, !sapply(df, is.factor)], method=imputeType) #"knnImpute",
		df[, !sapply(df, is.factor)]=predict(datImpute, df[, !sapply(df, is.factor)])
		
		returnList[["Impute"]] = datImpute
	}
	
	
	if (isDT) df = df %>% data.table()
	returnList[["Data"]] = df
	
	returnList
}


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



#' Get's the basic summary stats for a data set, in data.frame format: 
#' seperate row for each column in a data.frame (factors/characters removed).
#' 
#' @param df data.frame or data.table
#' @export

getSummaryTable <- function(df) {
	require(dplyr)
	require(purrr)
	require(lubridate)
	
	df = df %>% dplyr::ungroup() %>% data.frame()
	
	charNms = names(df[, sapply(df, function(x) (is.character(x) | is.factor(x)) & any(complete.cases(x)) ), drop=FALSE])
	numNms =  names(df[, sapply(df, function(x) (is.integer(x) | is.numeric(x)) & any(complete.cases(x))), drop=FALSE])
	dateNms =  names(df[, sapply(df, function(x) (is.POSIXt(x) | is.Date(x)) & any(complete.cases(x))), drop=FALSE])
	
	
	
	datLs = list()
	
	####################################
	## Do the date variables
	####################################
	if (length(dateNms)>0) {
		
		dfDate = df %>% dplyr::select(one_of(dateNms)) 
		
		missingFnx = function(x) length(x[is.na(x)])
		numUniFnx = function(x) length(unique(x[!is.na(x)]))
		minFnx=function(x) as.character(min(x, na.rm=T))
		maxFnx=function(x) as.character(max(x, na.rm=T))
		
		summyDate = rbind(
				dfDate %>% dplyr::summarize_all(funs(length)),
				dfDate %>% dplyr::summarize_all(funs(numUniFnx)),
				dfDate %>% dplyr::summarize_all(funs(missingFnx)),
				dfDate %>% dplyr::summarize_all(funs(minFnx)),
				dfDate %>% dplyr::summarize_all(funs(maxFnx))
		)
		
		summyDate = data.frame(names(summyDate), t(summyDate), stringsAsFactors=FALSE)	
		names(summyDate)=c("Var","n", "NumUnique","NumNA","MinDate", "MaxDate")
		rownames(summyDate)=NULL
		
		datLs[["DateFields"]]=summyDate	
	}
	
	
	####################################
	## Do the character/factor variables
	####################################
	
	if (length(charNms)>0){
		
		dfChar = df %>% dplyr::select(one_of(charNms)) 
		
		
		####################
		## Get the top values
		####################
		
		
		sumFnx <- function(x) {
			tab=as_data_frame(prop.table(table(x))) %>% arrange(desc(n))
			res=paste0("(",round(tab$n*100,0),"%) ",tab$x)
			if (length(res)>25) res=res[1:25]
			res
		}
		
		padLists <- function(x, mv) {
			xlen = length(x)
			if (xlen < mv) x = c(x, rep("", mv-xlen))
			x
		}
		
		dfCharTop = dfChar %>% map(sumFnx) 
		maxVal = dfCharTop %>% dmap(length) %>% max
		dfCharTop = dfCharTop %>% dmap(function (x) padLists(x,maxVal))
		summyCharTop = data.frame(names(dfCharTop), t(dfCharTop), stringsAsFactors=F)	
		names(summyCharTop) = c("Var",paste0("Top",1:dim(dfCharTop)[1]))	
		rownames(summyCharTop)=NULL
		
		
		####################
		## Get unique and number missing
		####################
		
		missingFnx = function(x) length(x[is.na(x)])
		numUniFnx = function(x) length(unique(x[!is.na(x)]))
		
		
		summyChar = rbind(
				dfChar %>% dplyr::summarize_all(funs(length)),
				dfChar %>% dplyr::summarize_all(funs(numUniFnx)),
				dfChar %>% dplyr::summarize_all(funs(missingFnx))
		)
		
		summyChar = data.frame(names(summyChar), t(summyChar), stringsAsFactors=FALSE)	
		names(summyChar)=c("Var","n", "NumUnique","NumNA")
		rownames(summyChar)=NULL
		
		summyChar = summyChar %>% left_join(summyCharTop, by="Var")
		
		datLs[["CharFields"]]=summyChar	
		
	}
	
	
	####################################
	## Do numerics
	####################################
	
	if (length(numNms)>0){
		
		dfNum = df %>% dplyr::select(one_of(numNms))
		
		minFnx=function(x) min(x, na.rm=T)
		maxFnx=function(x) max(x, na.rm=T)
		meanFnx=function(x) mean(x, na.rm=T)
		quant1Fnx=function(x) quantile(x, probs=c(0.25), na.rm=T)
		medianFnx=function(x) median(x, na.rm=T)
		quant3Fnx=function(x) quantile(x, probs=c(0.75), na.rm=T)
		missingFnx = function(x) length(x[is.na(x)])
		numUniFnx = function(x) length(unique(x[!is.na(x)]))
		
		summyNum = rbind(
				dfNum %>% dplyr::summarize_all(funs(length)),
				dfNum %>% dplyr::summarize_all(funs(numUniFnx)),
				dfNum %>% dplyr::summarize_all(funs(missingFnx)),
				dfNum %>% dplyr::summarize_all(funs(minFnx)),
				dfNum %>% dplyr::summarize_all(funs(quant1Fnx)),
				dfNum %>% dplyr::summarize_all(funs(medianFnx)),
				dfNum %>% dplyr::summarize_all(funs(meanFnx)),
				dfNum %>% dplyr::summarize_all(funs(quant3Fnx)),
				dfNum %>% dplyr::summarize_all(funs(maxFnx))
		)
		
		summyNum = data.frame(names(summyNum), t(summyNum), stringsAsFactors=F)	
		names(summyNum)=c("Var","n", "NumUnique","NumNA","Min","LowQuart","Median","Mean","UpQuart","Max")
		rownames(summyNum)=NULL
		
		datLs[["NumFields"]]=summyNum	
		
	}
	
	datLs
}