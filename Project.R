setwd("c:/Users/AA/Desktop/Our Files/Avinash//2015/Coursera//Data Science")
setwd('Machine Learning//Code')

training = read.csv('pml-training.csv')
testing = read.csv('pml-testing.csv')
library(caret)


#Get rid of all names, timewindows etc.
training = training[,-c(1:7)]; testing= testing[,-c(1:7)]

# Eliminate those columns where > 50% are NA in the training set
# Starting at the end at moving towards the beginning
trainRows = nrow(training)
trainCols = ncol(training)
train1 = training
test1 = testing
for (i in 1:trainCols) {
    if (sum(is.na(train1[,(trainCols-i)]) / trainRows) > .5) {
        train1 = train1[,-(trainCols-i)]
        test1 = test1[,-(trainCols-i)]        
    }
}

# Do the same for test set, because if the predictors are not there in 90% of 
# test set, then there is no reason to consider them in training set
# So, this time, make the threshold .9
train2 = train1
test2 = test1
testRows = nrow(test2)
testCols = ncol(test2)
for (i in 1:testCols) {
    if (sum(is.na(test2[,(testCols-i)]) / testRows) > .9) {
        train2 = train2[,-(testCols-i)]
        test2 = test2[,-(testCols-i)]        
    }
}

# Eliminate those predictors that don't vary much
nearZeros = nearZeroVar(train2, saveMetrics=T)
toEliminate = which(nearZeros$nzv)
# Did not point to any near zero variables, so no adjustment needed here

# Align the names of the last columns
columns = dim(train2)[2]
names(test2)[columns]= names(train2)[columns]

# Just make sure that the types match; if they don't, coerce them to be same
# Should not really need this. But, doing it just to be sure.
for (i in 1:columns) {
    if (typeof(train2[2,i])=='integer') test2[,i]=as.integer(test2[,i])
    else test2[,i]=as.double(test2[,i])
}

#Diagnosis - start
# Just find out if there are any NA's left in the training data
trainRows = nrow(train2)
trainCols = ncol(train2)
for (i in 1:trainCols) {
    if (sum(is.na(train2[,(trainCols-i)]) > 0)) {
        print(trainCols-i)
        print("NA's Found")
    }
}
#Diagnosis - end

starter = 123
# Cross Validation - break into training and test, and do the exercise
for (k in 1:10 ) {
    starter = starter + 1; set.seed(starter)
    inTrain = createDataPartition(y=train2$classe, p=.75, list=F)
    trainTrain = train2[inTrain,]
    trainTest = train2[-inTrain,]
    
    
    # Try classification tree
    modCART = train(classe ~., method='rpart', data=trainTrain)
    modCART$finalModel
    
    # Impute missing values with KNN
    # Not needed because there are no missing values in the test data
    #preObjTrain <- preProcess(train2[,-columns],method="knnImpute")
    #preObjTest <- preProcess(test2[,-columns],method="knnImpute")
    
    predictCART = predict(modCART, trainTest)
    
    # Test for accuracy against training data
    a = table(trainTest$classe, predict(modCART, trainTest))
    total = 0; diag = 0
    for (i in 1:25) {
        total = total +a[i]
        if ((i %% 6) == 1) {diag = diag+a[i]}
    }
    print(diag/total)  # Gives only about 50%, not very good
}


# Try random forest
modRF <- train(classe ~ ., data=train2, method='rf', prox=T, ntree=10)

#Try GBM
modGBM <- train(classe ~ ., data=train2, method='gbm', verbose=F)


# Creating files for submission
answers = rep('A', 20)
pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
    }
}
setwd('Submissions')
pml_write_files(answers)
setwd('..')