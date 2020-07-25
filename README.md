---
output:
  pdf_document: default
  html_document: default
---

This repo contains files which clean the dataset from the experiment of "Human Activity Recognition Using Smartphones Data Set" from researchers of the Smartlab and CETpD. For more detailed info check the following link: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.
The run_analysis.R file loads the required files from the dataset from the authors, to make a tidy data frame. The original zip files contain information on the variables, train data, test data separated files. So unifies all this data into one data frame that is easy to manipulate for researchers. Also, the new data frame contains measurements only from the standard deviation and mean of the features, and makes a second data frame with the mean of all variables by activities measured. Finally makes the variable names more readable. 
The CodeBook describes the transformations and the data more detailed.
