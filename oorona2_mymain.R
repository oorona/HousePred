set.seed(2685)
library(xgboost)
library(glmnet)
library(Matrix)


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

#Data Prep gbm
# Removing columns
train.x =train[-1]
train.x =train.x[-82]
# Extracting training y
train.y=log(train[,"Sale_Price"])
# Create train Matrix
trainM=createExpandedMatrix(train.x)
ctrainM=colnames(trainM)


#Data prep Linear Regression

# List of irrelevant columns
quan.value = 0.95        
alpha=0.2
dropped_cols = c("BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF","Second_Flr_SF", 'First_Flr_SF')
winsor.vars <- c("Lot_Frontage", "Lot_Area","Mas_Vnr_Area", "Gr_Liv_Area", "Garage_Area")

train <- read.csv("train.csv")
# Eliminate columns
train.x2 =train[-1]
train.x2 =train.x2[-82]
train.x2=train.x2[ , !(names(train.x2) %in% dropped_cols)]
# Fix data Issues
train.x2$Garage_Yr_Blt[is.na(train.x2$Garage_Yr_Blt)] = 0
train.x2$Garage_Yr_Blt[train.x2$Garage_Yr_Blt>2010]=2007

for(var in winsor.vars){
  tmp <- train.x2[, var]
  myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
  tmp[tmp > myquan] <- myquan
  train.x2[, var] <- tmp
}

# Create train Matrix
trainM2=createExpandedMatrix(train.x2)
ctrainM2=colnames(trainM2)


# train Model gbm
params <- list(
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.5,
  gamma=0,
  lambda=0.1,
  alpha=0
)
xgb.model <- xgboost(data = as.matrix(trainM), 
                     label = train.y,
                     params = params,
                     nrounds=532,
                     verbose = FALSE)


# Train Model linear
cv.out <- cv.glmnet(as.matrix(trainM2), train.y, alpha = alpha)



# Load test data
test <- read.csv("test.csv")

# Removing columns
test.x=test[-1]
test.x2=test.x[ , !(names(test.x) %in% dropped_cols)]
test.x2$Garage_Yr_Blt[is.na(test.x2$Garage_Yr_Blt)] = 0
test.x2$Garage_Yr_Blt[test.x2$Garage_Yr_Blt>2010]=2007

for(var in winsor.vars){
  tmp <- test.x2[, var]
  myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
  tmp[tmp > myquan] <- myquan
  test.x2[, var] <- tmp
}


testM=adjustMatrix(test.x,ctrainM)
testM2=adjustMatrix(test.x2,ctrainM2)

# Prediction gbm
pred=exp(predict(xgb.model,as.matrix(testM)))
pred=cbind(test["PID"],pred)
names(pred)[2]="Sale_Price"
write.csv(pred,"mysubmission1.txt",row.names = FALSE)


# Prediction linear


pred2 <-exp(predict(cv.out, s = cv.out$lambda.min, newx = as.matrix(testM2)))


pred2=cbind(test["PID"],pred2)
names(pred2)[2]="Sale_Price"
write.csv(pred2,"mysubmission2.txt",row.names = FALSE)

