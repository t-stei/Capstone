#Installing required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(ordinalForest)) install.packages("ordinalForest", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

#Download URL
urlfile="https://raw.githubusercontent.com/t-stei/Capstone/master/column_2C_weka.csv"
#Create Data
patients<-read_csv(url(urlfile))

#Setting the seed
set.seed(1, sample.kind = "Rounding")
#Choosing test index
test_index <- createDataPartition(patients$class, times = 1, p = 0.2, list = FALSE)
test_index <- as.vector(test_index)
#Creating test_set
test_set <- patients[test_index, ]
#Creating train set
train_set <- patients[-test_index, ]

##1. RF Model

#Create a df with best tuneGrid options
rf_values <- data.frame(mtry = c(2), splitrule = c("extratrees"),
                        min.node.size = c(8))
#Setting the seed
set.seed(1, sample.kind = "Rounding")
#Train Model with best variables
rf_fit <- train(class ~ .,
                method = "ranger",
                data = train_set,
                tuneGrid = rf_values)
#Predict y_hat
y_hat_rf_final <- predict(rf_fit, test_set, type = "raw")
#Creating Confusion Matrix
cm <- confusionMatrix(y_hat_rf_final,factor(test_set$class))
#Storing ensemble accuracy
acc_final <- cm$overall["Accuracy"]
#Storing Ensemble Sensitivity and Specificity
ss_final <- cm$byClass[1:2]

#Creating Tibble
final_df <- tibble(model = c("Final Model"), Accuracy = c(acc_final), Sensitivity = c(ss_final[1]), Specificity = c(ss_final[2]))
#Printing Tibble
final_df %>% knitr::kable()
