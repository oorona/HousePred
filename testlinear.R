library(glmnet)
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



alpha=1
dropped_cols = c()#c('Street', 'Utilities',  'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude')
quan.value <- 0.95             
winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area") 
train <- read.csv("train.csv")
train.y=log(train[,"Sale_Price"])
train.x2 =train[-1]
train.x2 =train.x2[-82]
train.x2=train.x2[ , !(names(train.x2) %in% dropped_cols)]
train.x2$Garage_Yr_Blt[is.na(train.x2$Garage_Yr_Blt)] = 0
train.x2$Garage_Yr_Blt[train.x2$Garage_Yr_Blt>2010]=2007

for(var in winsor.vars){
  tmp <- train.x2[, var]
  myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
  tmp[tmp > myquan] <- myquan
  train.x2[, var] <- tmp
}
trainM2=createExpandedMatrix(train.x2)
ctrainM2=colnames(trainM2)


#lfit=glmnet(as.matrix(trainM2),train.y)

#plot(lfit,xvar="lambda",label=TRUE)



#plot(cv.res)

test <- read.csv("test.csv")

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


testM2=adjustMatrix(test.x2,ctrainM2)

test_y <- read.csv("test_y.csv")
test.y=log(test_y[,"Sale_Price"])


#lfit=glmnet(as.matrix(trainM2),train.y,alpha=alpha)
#pred=predict(lfit,as.matrix(testM2))
#rmse=sqrt(apply((test.y-pred)^2,2,mean))

#plot(log(lfit$lambda),rmse,type='b',xlab="Log(lambda)")

#blambda=lfit$lambda[order(rmse)[1]]

#blambda




cv.res=cv.glmnet(as.matrix(trainM2),train.y,alpha=1)
sel.vars <- predict(cv.res, type="nonzero", s = cv.res$lambda.1se)$X1
#sel.vars

alphas=seq(0.2,0.2,0.1)
res=rep(0,length(alphas))

for(i in (1:length(alphas))) {
  cv.res <- cv.glmnet(as.matrix(trainM2[, sel.vars]), 
                    train.y, alpha = 0)
  pred <-predict(cv.res, s = cv.res$lambda.1se, 
              newx = as.matrix(testM2[, sel.vars]))
  res[i]=sqrt(mean((test.y-pred)^2))
}

cat("rmse=",min(res),"alpha=",alphas[which.min(res)],"\n")

for(i in (1:length(alphas))) {
  cv.out <- cv.glmnet(as.matrix(trainM2), train.y, alpha = alphas[i])
  pred <-predict(cv.out, s = cv.out$lambda.min, newx = as.matrix(testM2))

  res[i]=sqrt(mean((test.y-pred)^2))
}

cat("rmse=",min(res),"alpha=",alphas[which.min(res)],"\n")

#colnames(trainM2) == colnames(testM2)
#pred=predict(lfit,as.matrix(testM2),s=cv.reso$lambda.min,alpha=alpha)
#sqrt(mean((test.y-pred)^2))

#pred=predict(lfit,as.matrix(testM2),s=cv.res$lambda.1se)
#sqrt(mean((test.y-pred)^2))
#pred=predict(lfit,as.matrix(testM2),s=cv.res$lambda.min)
#sqrt(mean((test.y-pred)^2))