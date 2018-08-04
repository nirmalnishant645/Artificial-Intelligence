#Importing Dataset
dataset = read.csv('dataset.csv')

library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset$news))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

#Bag of words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset1 = as.data.frame(as.matrix(dtm))
dataset1$headline_type = dataset$type

#splitting data into training and test data
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset1$headline_type, SplitRatio = 0.75)
test_set = subset(dataset1, split == TRUE)
training_set = subset(dataset1, split == FALSE)


#Fitting Naive Bayes to the training set
#install.packages('e1071')
library(e1071)
classifier = naiveBayes(x = training_set[-8894],
                        y = training_set$headline_type)


#Predicting the Test set Results
y_pred = predict(classifier, newdata = test_set[-8894])


#Making the COnfusion Matrix
cm = table(test_set[, 8894], y_pred)

classifier$apriori

#Finding Accuracy
accuracy = (cm[1,1] + cm[2,2] + cm[3,3] + cm[4,4] + cm[5,5]) / (cm[1,1]+cm[1,2]+cm[1,3]+cm[1,4]+cm[1,5]+cm[2,1]+cm[2,2]+cm[2,3]+cm[2,4]+cm[2,5]+cm[3,1]+cm[3,2]+cm[3,3]+cm[3,4]+cm[3,5]+cm[4,1]+cm[4,2]+cm[4,3]+cm[4,4]+cm[4,5]+cm[5,1]+cm[5,2]+cm[5,3]+cm[5,4]+cm[5,5])
print(accuracy)

#category <- c('sport','entertainment','politics', 'business','tech')
library(plotly)
library(ggplot2)
Sys.setenv("plotly_username"="nirmalnishant645")
Sys.setenv("plotly_api_key"="getA9Z6WMnkagrae3y1i")
trace1 <- list(x = c("sport", "entertainment", "politics", "business", "tech"), 
               y = c("sport", "entertainment", "politics", "business", "tech"), 
               z = cm, colorscale = "Jet", type = "heatmap")
data <- list(trace1)
layout <- list(barmode = "overlay", title = "Confusion Matrix", 
               xaxis = list(title = "Predicted value", titlefont = list(color = "#7f7f7f", size = 18)), 
               yaxis = list(title = "True Value", titlefont = list(color = "#7f7f7f", size = 18)))
p <- plot_ly()
p <- add_trace(p, x=trace1$x, y=trace1$y, z=trace1$z, colorscale=trace1$colorscale, type=trace1$type)
p <- layout(p, barmode=layout$barmode, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
api_create(p, filename = "Naive-Bayes")