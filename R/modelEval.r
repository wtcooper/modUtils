

#' Compute the area under the ROC (AUC)
#'
#' This function computes the area under the receiver-operator
#' characteristic (AUC)
#'
#' @param actual binary vector
#' @param predicted real-valued vector that defines the ranking
#' @export
getAUC <- function(actual, predicted)
{
	# convert to binary vector if not
	if (all(sort(unique(actual))!=c(0,1))) actual=as.integer(as.factor(actual))-1
	
	r <- rank(predicted)
	n_pos <- as.numeric(sum(actual==1))
	n_neg <- as.numeric(length(actual) - n_pos)
	auc <- (sum(r[actual==1]) - n_pos*(n_pos+1)/2) / (n_pos*n_neg)
	names(auc) = "AUC"
	auc
}



#' Compute the Gini coefficient 
#'
#' This function computes the Gini coefficient as (2*(AUC-.5))
#'
#' @param actual binary vector
#' @param predicted real-valued vector that defines the ranking
#' @export
getGINI <- function(actual, predicted) {
	
	gini <- 2.*(getAUC(actual,predicted)-.5)
	names(gini) = "Gini"
	gini
}




#' Get's the probability cutoff for a binomial classification 
#' model that minimizes the difference between sensitivity and
#' specificity. 
#' 
#' @param prob predicted probability
#' @param obs observed labels
#' @export
getBalancedProbCut <- function(prob, obs, posLabel, negLabel, probSeq = seq(0.001, 0.999, by=0.001)) {
	
	
	
	vals=sapply(probSeq, function(cut) {
				pred = ifelse(prob>cut, posLabel, negLabel)
				cm = confusionMatrix(pred, obs, positive = posLabel)
				c(cut=cut, diff=abs(as.numeric(cm$byClass[1])-as.numeric(cm$byClass[2])))
			})
	
	vals = as.data.frame(t(vals))
	(vals %>% arrange(diff) %>% head(1))$cut	
	
}





#' Get's the balanced accuracy by choosing a probability
#' threshold value where Specificity == Sensitivity at the resolution
#' of the crit parameter.  
#' 
#' @param prob predicted probability
#' @param obs observed labels
#' @param posLabel the positive label (e.g. YES)
#' @param negLabel the negative label (e.g. NO)
#' @param crit criical value resolution for the difference in sensitivity==specificity
#' @param verbose boolean flag to print out the difference value
#' @export
getBalancedAcc <- function(prob, obs, posLabel, negLabel, crit=0.0001, verbose=F) {
	
	## Get the starting sequence for the probability threshold values 
	startMin = 0
	startMax = 1
	probSeq = seq(startMin, startMax, length.out=20 ) 
	
	## difVal will record what the different is for abs(sensitivity - specificity)
	## note: the loop will continue until this difference is < crit
	difVal = 1
	
	counter = 0 
	while ( (difVal > crit) & (counter < 10) ) {
		
		## Get a dataset of threshold cut value, difference, and the accuracy
		vals=sapply(probSeq, function(cut) {
					pred = ifelse(prob>cut, posLabel, negLabel)
					cm = suppressWarnings(confusionMatrix(pred, obs, positive = posLabel))
					c(cut=cut, diff=abs(as.numeric(cm$byClass[1])-as.numeric(cm$byClass[2])), acc=as.numeric(cm$overall[1]))
				})
		
		vals = as.data.frame(t(vals))
		
		## Get the mininum difference
		difVal = min(vals$diff)
		
		## Get new probSeq to test 
		idx = which(vals$diff == difVal)[1]
		startMin = ifelse(idx>1, vals$cut[idx-1], 0.0)
		startMax = ifelse(idx<length(vals$cut), vals$cut[idx+1], 1.0)
		
		## Here just do a length of 10 to scale down one order of magnitude
		probSeq = seq(startMin, startMax, length.out=10) 
		
		counter = counter + 1
		
		if(verbose) cat("Difference value: ", difVal, "\tCounter: ", counter, "\n")
		
	}
	
	## Return the final accuracy with the lowerst difference
	(vals %>% arrange(diff) %>% head(1))$acc	
}





#' Simple approach to balance the probabilities from an inbalanced
#' multiclass/multinomial model.  Divides the probability by the
#' class frequency used in the training dataset.   Note: this calculates
#' based on the named columns and the named frequencies matching so 
#' makes sure they match ok
#' 
#' @param probs a dataframe of the probabilities, each column with the class name in factor order
#' @param freqs named proportions from the training data as from prop.table(table(target))
#' @export
balanceMultiProbs <- function(probs, freqs) {
	for (probNm in names(probs)){
		freq = freqs[probNm]
		probs[,probNm] = probs[,probNm]/freq 
	}
	probs
}



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




#' Get's class-specific evaluation metrics for a multiclass classification model.
#' For each class, includes sensitivity, specificity, and balanced accuracy.
#' Also includes calculation of a custom balanced accuracy that is the product
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
multiClassMetrics <- function(data, lev = NULL, model = NULL)  {
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
	CMVals =c(df, meanBalAcc=arithMeanBalAcc, geoMeanBalAcc=geoMeanBalAcc)
	names(CMVals)=str_replace_all(names(CMVals), "Sensitivity","Sens")
	names(CMVals)=str_replace_all(names(CMVals), "Specificity","Spec")
	names(CMVals)=str_replace_all(names(CMVals), "Balanced Accuracy","BalAcc")
	CMVals
}


#' Get's overall evaluation metrics for a multiclass classification model.
#' Includes overall accuracy and kappa, and then the arithmetic mean of 
#' all classes for sensitivty, specificity, detection rate, and balanced accuracy.
#' Additionally includes one-vs-all calculations of AUC and log loss, which is
#' presented as the mean value across all one-vs-all calculations.  
#'  
#' Designed to work as caret summaryFunction where data is a data.frame
#' with columns 'pred' and 'obs' for predicted and observed labels, and the class
#' specific probabilities (columns as label names).  
#' 
#' Adapted from code by Zach Meyer: http://www.r-bloggers.com/error-metrics-for-multi-class-problems-in-r-beyond-accuracy-and-kappa/
#' 
#' @param data data.frame with columns 'pred', 'obs' for predicted and observed labels, and a column for each class probability. 
#' @param lev caret summaryFunction optional parameter
#' @param model caret summaryFunction optional parameter
#' @export
multiClassMetricsMeans <- function (data, lev = NULL, model = NULL){
	
	#Load Libraries
	require(caret)
	
	#Check data
	if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
		stop("levels of observed and predicted data do not match")
	
	
	
	## Get the 
	aucFnx <- function(actual, predicted)
	{
		# convert to binary vector if not
		if (all(sort(unique(actual))!=c(0,1))) actual=as.integer(as.factor(actual))-1
		
		r <- rank(predicted)
		n_pos <- as.numeric(sum(actual==1))
		n_neg <- as.numeric(length(actual) - n_pos)
		auc <- (sum(r[actual==1]) - n_pos*(n_pos+1)/2) / (n_pos*n_neg)
		auc
	}
	
	
	
	ll <- function(actual, predicted)
	{
		# convert to binary vector if not
		if (all(sort(unique(actual))!=c(0,1))) actual=as.integer(as.factor(actual))-1
		
		score <- -(actual*log(predicted) + (1-actual)*log(1-predicted))
		score[actual==predicted] <- 0
		score[is.nan(score)] <- Inf
		score
	}
	
	logLoss <- function(actual, predicted) mean(ll(actual, predicted))
	
	
	
	
	
	#Calculate custom one-vs-all stats for each class
	prob_stats <- lapply(levels(data[, "pred"]), function(class){
				
				#Grab one-vs-all data for the class
				pred <- ifelse(data[, "pred"] == class, 1, 0)
				obs  <- ifelse(data[,  "obs"] == class, 1, 0)
				prob <- data[,class]
				
				#Calculate one-vs-all AUC and logLoss and return
				cap_prob <- pmin(pmax(prob, .000001), .999999)
				prob_stats <- c(aucFnx(obs, prob), logLoss(obs, cap_prob))
				names(prob_stats) <- c('ROC', 'logLoss')
				return(prob_stats) 
			})
	prob_stats <- do.call(rbind, prob_stats)
	rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
	
	#Calculate confusion matrix-based statistics
	CM <- confusionMatrix(data[, "pred"], data[, "obs"])
	
	#Aggregate and average class-wise stats
	#Todo: add weights
	class_stats <- cbind(CM$byClass, prob_stats)
	class_stats <- colMeans(class_stats)
	
	#Aggregate overall stats
	overall_stats <- c(CM$overall)
	
	#Combine overall with class-wise stats and remove some stats we don't want 
	stats <- c(overall_stats, class_stats)
	stats <- stats[! names(stats) %in% 
					c('AccuracyLower', 'AccuracyUpper', 'AccuracyNull',
							'AccuracyPValue', 'McnemarPValue',
							'Prevalence', 'Detection Prevalence')]
	
	#Clean names and return
	names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
	return(stats)
	
}

