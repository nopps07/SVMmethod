#Support Vector Machine

sample. <- 1:25
sugar_content <- c(10.9, 10.9, 10.6, 10, 8.0, 8.2, 8.6, 10.9, 10.7, 8.0, 7.7, 7.8, 8.4, 11.5, 11.2, 8.9, 8.7, 7.4, 10.9, 10.0, 11.4, 10.8, 8.5, 8.2, 10.6)

df <- data.frame(sample.,
                 sugar_content,
                 stringsAsFactors =  FALSE)

library(ggplot2)

#plot sugar content along the x-axis
plot_df <- ggplot(df, aes(x=sugar_content, y =0)) +
  geom_point() +
  geom_text(aes(label = sugar_content), size = 2.5, vjust = 2, hjust = 0.5)

#display
plot_df

#The maximal margin separator is at the midpoint of the two extreme points in each cluster.
mm_separator <- (8.9 + 10)/2

#create data frame containing the maximum margin separator
separator <- data.frame(sep = mm_separator)

#add separator to sugar content scatterplot
plot_sep <- plot_df + geom_point(data = separator, aes(x = mm_separator, y = 0), color = "blue", size = 4)

#display plot
plot_sep


##Generate a 2d uniformly distributed dataset.
set.seed(42)

#set number of data points. 
n <- 600

#Generate data frame with two uniformly distributed predictors lying between 0 and 1.
df2 <- data.frame(x1 = runif(n), 
                 x2 = runif(n))

#classify data points depending on location
df2$y <- factor(ifelse(df2$x2 - 1.4*df2$x1 < 0, -1, 1), 
               levels = c(-1, 1))

head(df2)


#set margin
delta <- 0.07

#retain only those points that lie outside the margin
df1 <- df2[abs(1.4*df2$x1 - df2$x2) > delta, ]
head(df1)

#build a plot
plot_margins <- ggplot(data = df1, aes(x = x1, y = x2, color = y)) + geom_point() + 
  scale_color_manual(values = c("red", "blue")) + 
  geom_abline(slope = 1.4, intercept = 0)+
  geom_abline(slope = 1.4, intercept = delta, linetype = "dashed") +
  geom_abline(slope = 1.4, intercept = -delta, linetype = "dashed")

#display
plot_margins


###Ch2
#split train and test data in an 80/20 proportion
df2[, "train"] <- ifelse(runif(nrow(df2))<0.8, 1, 0)

#assign training rows to data frame trainset
trainset <- df2[df2$train == 1, ]

#assign test rows to data frame testset
testset <- df2[df2$train == 0, ]

#find index of "train" column
trainColNum <- grep("train", names(df2))

#remove "train" column from train and test dataset
trainset <- trainset[, -trainColNum]
testset <- testset[, -trainColNum]


#
library(e1071)

#build svm model, setting required parameters
svm_model<- svm(y ~ ., 
                data = trainset, 
                type = "C-classification", 
                kernel = "linear", 
                scale = FALSE)

#list components of model
names(svm_model)

#list values of the SV, index and rho
svm_model$SV
svm_model$index
svm_model$rho

#compute training accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)

#compute test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#personal note
head(df2)
head(trainset)
head(testset)

#
scatter_plot <- ggplot(data = trainset, aes(x = x1, y = x2, color = y)) + 
  geom_point() + 
  scale_color_manual(values = c("red", "blue"))

#add plot layer marking out the support vectors 
layered_plot <- 
  scatter_plot + geom_point(data = trainset[svm_model$index, ], aes(x = x1, y = x2), color = "purple", size = 4, alpha = 0.5)

#display plot
layered_plot


w <- data.frame(x1 = 6.55241,
                x2 = -4.73278)
w <- as.matrix(w)
factor(w)

#calculate slope and intercept of decision boundary from weight vector and svm model
slope_1 <- -w[1]/w[2]
intercept_1 <- svm_model$rho/w[2]

#build scatter plot of training dataset
scatter_plot <- ggplot(data = trainset, aes(x = x1, y = x2, color = y)) + 
  geom_point() + scale_color_manual(values = c("red", "blue"))
#add decision boundary
plot_decision <- scatter_plot + geom_abline(slope = slope_1, intercept = intercept_1) 
#add margin boundaries
plot_margins <- plot_decision + 
  geom_abline(slope = slope_1, intercept = intercept_1 - 1/w[2], linetype = "dashed")+
  geom_abline(slope = slope_1, intercept = intercept_1 + 1/w[2], linetype = "dashed")
#display plot
plot_margins

#plot decision boundaries and support vectors for the training data
plot(x = svm_model, data = trainset)
