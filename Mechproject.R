# Machine learning Project file
# Read data from the url and store in training and test data sets
library(caret)
loadReadfile <- function(name,parameters){
  tempfile <- tempfile()
  download.file(name,tempfile)
  tempdata <- read.csv(tempfile, na.strings =parameters)
  unlink(tempfile)
  return(tempdata)
}


traindata <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainingdata <- loadReadfile (traindata,c("","NA","#DIV/0!"))
                                      
testdata <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testingdata <- loadReadfile(testdata,c("","NA","#DIV/0!"))
# create five measured instances of training data using classe variable
table (trainingdata$classe)

# divide the traing set 80 to 20 for validation and analysis

set.seed(196220)
iniTrain <- createDataPartition(trainingdata$classe,p=0.8,list=FALSE)
training <- trainingdata[iniTrain,]
validdate <- trainingdata[-iniTrain,]

# Cleaning and tiding the data set

## Exclude the near zero or zero variance predicators (one unique value)

zvarcolums <- nearZeroVar(training)
 
training <- training[,-zvarcolums]
# exclude the descriptive values columns and time stamps
descol <- c("X", "user_name", "raw_timestamp_part_1", 
            "raw_timestamp_part_2", "cvtd_timestamp", 
            "new_window", "num_window")
training <- training [,!names(training)%in% descol]

# exclude the columns with 40percentage and more missing values and NA
nacollength <- sapply(training,function(x){sum(!(is.na(x)|x==""))})
nullvaluecol <- names(nacollength[nacollength <0.6 *length(training$classe)])
training <- training[,!names(training)%in% nullvaluecol]

# since the data is large create a model with random Forest
fitMod <- randomForest(classe~.,data=training,importance=TRUE,ntrees=10)
# fit the model prediction in training data set
fitModPred <- predict(fitMod,training)
# check the accuracy of the predication
print(confusionMatrix(fitModPred,training$classe))

# validate with the validdate data

fitModVal <- predict(fitMod,validdate)

print(confusionMatrix(fitModVal,validdate$classe))

# test values prediction
fitModtest <- predict(fitMod,testingdata)

# Get the values for the 2nd part of the project with test data

answers <- as.vector(fitModtest)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
