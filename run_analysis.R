library(tidyverse)
library(stringr)
library(data.table)


setwd("~/") ### Set my usual wd as wd
dest<- paste(getwd(), "/FinalProjectCD/FP.zip",sep="") ### Destination of the zip file

if (length(list.files(dest)) ==0 ){
    url<- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url,dest) 
    newdir<- paste(getwd(),"/FinalProjectCD",sep="" )
    a<- unzip(dest, exdir = newdir)
    file.remove(dest)
    setwd(paste( newdir, "/" ,list.files(newdir),sep="") )
} ### If new directory is empty, download the zip file, unzip it, and set the extracted carpet as wd.


### make a variable with the name of the test and train folders
train_dest<- paste(getwd(), "/train",sep="")
test_dest<- paste(getwd(), "/test",sep="")

### read file with features names, and levels of ".*y$" data.frames 
features_n <- fread(file= paste(getwd(),"/features.txt",sep="" ))[,2]
levels_y <- fread(file= paste(getwd(),"/activity_labels.txt",sep="" ) )[,2]


### read relevant txt with fread and
train_X <- fread(file=paste(train_dest, "/X_train.txt",sep=""), col.names = features_n$V2  )
train_y <- fread(file=paste(train_dest, "/y_train.txt",sep=""), col.names = "activity" )
test_X <- fread(file=paste(test_dest, "/X_test.txt",sep=""), col.names = features_n$V2  )
test_y <- fread(file=paste(test_dest, "/y_test.txt",sep=""), col.names = "activity" )



### merge columns (point 1)
all.equal(names(train_X),names(test_X)) ### check if features are in the same order.
X<- rbind(train_X,test_X)
y<- rbind(train_y,test_y)

### select measurements with SD or mean (point 2)
X<- X %>% select(contains("std"),contains("mean"))


### set levels of ".*y$" data.frames  (point 3)
train_y$activity<- factor(train_y$activity)
test_y$activity<- factor(test_y$activity)

levels(train_y$activity) <- levels_y$V2
levels(test_y$activity)<- levels_y$V2

### Make descriptive variable names
data<- cbind(X,y) ### making a single dataset
names(data)<- str_replace_all(names(data),pattern="\\(\\)",replace="")
names(data)<- str_replace_all(names(data),pattern="\\-",replace="_")


### New features with the summary by activity of each variable (point 5)
summ<- data  %>%
       group_by(activity) %>% 
       summarise(across(`tBodyAcc-std()-X`:`angle(Z,gravityMean)`,list(mean), .names= "mean_by_activity_{col}"))

data_grouped_means<- left_join(data,summ,by="activity")






