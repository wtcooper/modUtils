# modUtils
Collection of utils for model building and tuning.

## Overview
A small collection of some utility functions I use frequently for data pre-processing and model evaluations.
Some functions are small tweaks to  caret functions (e.g., dummy coder using a data.table approach; near zero variance
without comparison to second most abundanct level but uses overall frequency instead) or custom comparisons to 
use with caret (e.g., multiClassSummary. for multiclass models).  Also includes a VIF method to remove variables
with high multicollinearity. <br />

## Installation

```R
library(devtools) 
devtools::install_github("wtcooper/modUtils")
```

