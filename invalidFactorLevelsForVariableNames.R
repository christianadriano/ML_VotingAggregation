# StackOverflow questoin on the following Error:

# Error: At least one of the class levels is not a valid R variable name; This will cause errors when 
# class probabilities are generated because the variables names will be converted to  FALSE., TRUE. . 
# Please use factor levels that can be used as valid R variable names  (see ?make.names for help).

##Platform: RStudio, caret package

## Quick and dirty solution
# I tried replacing TRUE and FALSE for characters T and F. It did work, but I would like to
# know if there are any other alternatives.

##Question
# Is there a way to predict a boolean variable (True, False) using R caret? 

#Source code
#create a table with two columns. One column with False and True
#

