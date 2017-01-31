# modUtils
Collection of utils for model building and tuning.

## Overview
A small collection of some utility functions I use frequently for data pre-processing and model evaluations.

#### Preprossing functions include: <br />
getBasicCleanData(): Wrapper function to do many of the functions below sequentially. <br />
removeHighVIF(): Removes columns with high multicollinearity automatically based on VIF. <br />
removeZeroVar(): Rip of caret::nearZeroVar that just looks at % of most frequent, not relative to the second most frequent. <br />
convToBinary(): Converts select columns to binary. <br />
convToDummy(): Converts select factor/character columns to dummy coded columns using a data.table approach. <br />
balanceClasses(): Simple random class balancer for multinomial targets that does up- or down-sampling - you supply how many observations per class you want. <br />


#### Model evaluation functions include: <br />
getBalancedProbCut(): Returns a probability threshold that minimizes difference between sensitivity and specificity (balanced accuracy). <br />
getBalancedAcc(): Returns the balanced accuracy by choosing a probability threshold value where Specificity == Sensitivity. <br />
getAUC(): Returns AUC. <br />
getGINI(): Returns Gini. <br />
multiClassMetrics(): Custom caret:summaryFunction for multiclass targets (I think same as caret's multiClassSummary() function now). <br />
multiClassMetricsMeans(): Another custom caret:summaryFunction for multiclass targets.  This one returns one-vs-all calcs for AUC and log loss (mean across all one-vs-all). <br />
getCMVals(): Returns a subset of classification metrics for binary and multiclass classification.




## Installation

```R
library(devtools) 
devtools::install_github("wtcooper/modUtils")
```

