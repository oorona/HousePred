test.y <- read.csv("test_y.csv")
names(test.y)[2] <- "True_Sale_Price"
pred <- read.csv("mysubmission1.txt")
pred <- merge(pred, test.y, by="PID")
round(sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2)),3)
pred <- read.csv("mysubmission2.txt")
pred <- merge(pred, test.y, by="PID")
round(sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2)),3)
