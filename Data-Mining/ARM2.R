library(arules) #To implement algorithm
library(arulesViz) #Visualization
library(datasets) #For loading dataset in RStudio

data(Groceries) #Dataset is loaded

itemFrequencyPlot(Groceries,topN=20,type="absolute") #To get top 20 items

#To get rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2) #To control the number of significant digits
rules <- inspect(sort(rules[1:5], by = "confidence")) #To display top 5 values 

summary(rules) #Summary of rules

#Targetting elements
#Below is an example of knowing what products will lead costumers to buy whole milk too.
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
#Likewise, we can set the left hand side to be "whole milk" and find its antecedents.
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#Plotting graph
plot(rules,method="graph",interactive=TRUE,shading=NA)