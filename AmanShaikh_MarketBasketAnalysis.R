# Reading Cosmetic.csv file from local storage
# You have to change path according to your local machine.
mydata<-read.csv("C:\\Users\\amans\\Desktop\\7thSem\\4CP31_Project1\\MarketBasketAnalyzer\\Cosmetics.csv",header=T, colClasses = "factor")

# Finding association rules
library(arules)
rules <- apriori(mydata)

#Summary of data
summary(mydata)

# Rules with specified parameter values
rules <- apriori(mydata,parameter = list(minlen=2, maxlen=10,supp=.7, conf=.8))

# This is to find out rule what is the probability of females buying foundation if they buy bag and blush.
rules <- apriori(mydata,parameter = list(minlen=2, 
                                         maxlen=3,
                                         supp=.01, 
                                         conf=.7)
                 ,appearance=list(rhs=c("Foundation=Yes"),
                                  lhs=c("Bag=Yes", "Blush=Yes"),
                                  default="lhs"))


# Generalizing buy finding probability of females buying foundation if they any item from dataset.
rules <- apriori(mydata,parameter = list(minlen=2, 
                                         maxlen=5,
                                         supp=.1, 
                                         conf=.5),
                 appearance=list(rhs=c("Foundation=Yes"),
                                 lhs=c("Bag=Yes", "Blush=Yes", "Nail.Polish=Yes", "Brushes=Yes", "Concealer=Yes", "Eyebrow.Pencils=Yes", "Bronzer=Yes", "Lip.liner=Yes", "Mascara=Yes", "Eye.shadow=Yes","Lip.Gloss=Yes", "Lipstick=Yes", "Eyeliner=Yes"),
                                 default="none"))


inspect(rules)


# Finding redundancy
redundant <- is.redundant(rules, measure="confidence")
which(redundant)
newrules <- rules[!redundant]
newrules <- sort(newrules, by="lift")
inspect(newrules)

# Graphs and Charts
library(arulesViz)
#plot(rules)
#plot(rules,method="grouped")
plot(rules,method="graph")