set.seed(2685)
library(xgboost)


##G
# function definitions
getCategoricalVar = function(df){
  cat.var <- colnames(df)[
    which(sapply(df, function(x) mode(x)=="character"))]
  cat.var
}

createExpandedMatrix = function(df){
  catVar=getCategoricalVar(df)
  expandedM <- df[, !colnames(df) %in% catVar, 
                          drop=FALSE]
  n <- nrow(df)
  for(var in catVar){
    mylevels = sort(unique(df[, var]))
    m = length(mylevels)
    m = ifelse(m>2, m, 1)
    temp_level = matrix(0, n, m)
    col.names = NULL
    for(j in 1:m){
      temp_level[df[, var]==mylevels[j], j] <- 1
      col.names = c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(temp_level) = col.names
    expandedM = cbind(expandedM, temp_level)
  }
  expandedM
}

adjustMatrix = function (sourceMatrix,colnames){
  
  expandedMatrix=createExpandedMatrix(sourceMatrix)
  sourcenames=colnames(expandedMatrix)
  n=nrow(sourceMatrix)
  finalMatrix=data.frame(row.names=1:n)  
  for (colname in colnames){
    if (colname %in% sourcenames){
      finalMatrix=cbind(finalMatrix,expandedMatrix[,colname,drop=FALSE])
    }
    else{
      zero_level=matrix(0,n,1)
      colnames(zero_level)=colname
      finalMatrix=cbind(finalMatrix,zero_level)
    }
  }
  finalMatrix
}

#Loading  train data
train <- read.csv("train.csv")

# Removing columns
train.x =train[-1]
train.x =train.x[-82]
# Extracting training y
train.y=log(train[,"Sale_Price"])

# Create train Matrix
trainM=createExpandedMatrix(train.x)
# Get list of columns
ctrainM=colnames(trainM)


# train Model
params <- list(
  eta = 0.5,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.5,
  gamma=0,
  lambda=0,
  alpha=1
)
xgb.model <- xgboost(data = as.matrix(trainM), 
                     label = train.y,
                     params = params,
                     nrounds=500,
                     verbose = FALSE)


# Load test data
test <- read.csv("test.csv")
# Removing columns
test.x=test[-1]

testM=adjustMatrix(test.x,ctrainM)

pred=exp(predict(xgb.model,as.matrix(testM)))

pred=cbind(test["PID"],pred)

names(pred)[2]="Sale_Price"

write.csv(pred,"mysubmission1.txt",row.names = FALSE)


##
