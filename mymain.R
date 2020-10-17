set.seed(2685)
library(xgboost)


train <- read.csv("train.csv")
test <- read.csv("test.csv")

train.x =train[-1]
train.x =train.x[-82]

train.y=log(train[,"Sale_Price"])

test.x=test[-1]

total=rbind(train.x,test.x)

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

expandMatrix = function (sourceMatrix,colnames){
  sourcenames=colnames(sourceMatrix)
  n=nrow(sourceMatrix)
  expandedmatrix=data.frame(row.names=1:n)  
  for (colname in colnames){
    if (colname %in% sourcenames){
      expandedmatrix=cbind(expandedmatrix,sourceMatrix[,colname,drop=FALSE])
    }
    else{
      zero_level=matrix(0,n,1)
      colnames(zero_level)=colname
      expandedmatrix=cbind(expandedmatrix,zero_level)
    }
  }
  expandedmatrix
}


finalM=createExpandedMatrix(total)

trainM=createExpandedMatrix(train.x)
testM=createExpandedMatrix(test.x)

cfinalM=colnames(finalM)

trainExp=expandMatrix(trainM,cfinalM)
testExp=expandMatrix(testM,cfinalM)

start.time <- Sys.time()
xgb.model <- xgboost(data = as.matrix(trainExp), 
                     label = train.y, max_depth = 6,
                     eta = 0.05, nrounds = 1000,
                     subsample = 0.3,
                     verbose = FALSE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

pred=exp(predict(xgb.model,as.matrix(testExp)))

pred=cbind(test["PID"],pred)

names(pred)[2]="Sale_Price"

write.csv(pred,"mysubmission1.txt",row.names = FALSE)
