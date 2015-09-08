#' Pulls confusion matrix metrics from predicted and observed labels,
#' works for both binomial and multinomial.  Calls caret confusionMatrix()
#' and then returns as a vector. 
#' 
#' @param pred vector of predicted labels
#' @param obs vector of observed labels
#' @export
getCMVals <- function (pred, obs) {
	require(stringr)
	require(caret)
	
	CM = confusionMatrix(pred, obs)
	# if it's multinomial, then get the specific
	if(length(CM$byClass)>8) {
		df=as.data.frame(CM$byClass)[,c(1,2,8),drop=F]; nms=str_replace(rownames(df),"Class: ","")
		
		df=unlist(df); names(df)=paste(substr(names(df), 1, nchar(names(df))-1),"_",nms,sep="")
		CMVals =c(CM$overall[1:2], df)
	} else {
		CMVals =c(CM$overall[1:2], CM$byClass[c(1:2,8)])
	}
	
	names(CMVals)=str_replace_all(names(CMVals), "Sensitivity","Sens")
	names(CMVals)=str_replace_all(names(CMVals), "Specificity","Spec")
	names(CMVals)=str_replace_all(names(CMVals), "Balanced Accuracy","BalAcc")
	CMVals
}


#' Pulls the AUC and Gini values from predicted probability and 
#' observed labels. 
#' 
#' @param prob predicted probability
#' @param obs observed labels
#' @export
getBinEvalVals <- function (prob, obs) {
	require(Hmisc)
	
	tvals=rcorr.cens(prob, obs)[1:2]
	names(tvals)=c("AUC","Gini")
	tvals
}



#' Get's the probability cutoff for a binomial classification 
#' model that minimizes the difference between sensitivity and
#' specificity. 
#' 
#' @param prob predicted probability
#' @param obs observed labels
#' @export
getBalancedProbCut <- function(prob, obs, posLabel, negLabel) {
	require(caret)
	
	probSeq = seq(0.001, 0.999, by=0.001)
	
	
	vals=sapply(probSeq, function(cut) {
				pred = ifelse(prob>cut, posLabel, negLabel)
				cm = confusionMatrix(pred, obs, positive = posLabel)
				c(cut=cut, diff=abs(as.numeric(cm$byClass[1])-as.numeric(cm$byClass[2])))
			})
	
	vals = as.data.frame(t(vals))
	(vals %>% arrange(diff) %>% head(1))$cut	
	
}



#' Get's evaluation metrics for a multiclass classification model.
#' Includes calculation of a custom balanced accuracy that is the product
#' of a specific labels balanced accuracy (mean of spec and sens) times 
#' the minimum of spec or sens.  
#'  
#' Designed to work as caret summaryFunction where data is a data.frame
#' with columns 'pred' and 'obs' for predicted and observed labels. 
#' 
#' @param data data.frame with columns 'pred' and 'obs' for predicted and observed labels
#' @param lev caret summaryFunction optional parameter
#' @param model caret summaryFunction optional parameter
#' @export
multiClassSummary <- function(data, lev = NULL, model = NULL)  {
	require(stringr)
	require(caret)
	
	
	CM = confusionMatrix(data[, "pred"], data[, "obs"])
	df=as.data.frame(CM$byClass)[,c(1,2,8),drop=F]; nms=str_replace(rownames(df),"Class: ","")
	
	# balanced accuracy is just average of spec/sens
	# this tweak here creates a new value that tries to balance spec and sens so similar
	# highest value would have the highest mean where numbers are closest together.
	# E.g., sens=1, spec=0 => balAcc=.5 | sens=0.6, spec=0.4 => balAcc=.5....2nd is likely preferable
	df$BalAccAdj=apply(df, 1, function(x) x[3]*min(x[1],x[2]))
	arithMeanBalAcc = mean(df$BalAccAdj)
	geoMeanBalAcc = prod(df$BalAccAdj)/length(df$BalAccAdj)
	
	df=unlist(df); names(df)=paste(substr(names(df), 1, nchar(names(df))-1),"_",nms,sep="")
	CMVals =c(CM$overall[1:2], df, meanBalAcc=arithMeanBalAcc, geoMeanBalAcc=geoMeanBalAcc)
	names(CMVals)=str_replace_all(names(CMVals), "Sensitivity","Sens")
	names(CMVals)=str_replace_all(names(CMVals), "Specificity","Spec")
	names(CMVals)=str_replace_all(names(CMVals), "Balanced Accuracy","BalAcc")
	CMVals
}