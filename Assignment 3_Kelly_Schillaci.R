library(readr)
bankdata <- read_csv("IST 707/bankdata_csv_all (1).csv")
library(plyr)
library(dplyr)
library(arules)
str(bankdata)
#Create bins for ages
bankdata$age <- cut(bankdata$age, breaks = c(0,10,20,30,40,50,60,Inf),labels=c("child","teens","twenties","thirties","fourties","fifties","old"))
#Create bins for Income
min_income <- min(bankdata$income)
max_income <- max(bankdata$income)
bins = 3 
width=(max_income - min_income)/bins;
bankdata$income = cut(bankdata$income, breaks=seq(min_income, max_income, width))
#Change character variable to a factor variable
bankdata$children=factor(bankdata$children)
bankdata$id=factor(bankdata$id)
bankdata$sex=factor(bankdata$sex)
bankdata$region=factor(bankdata$region)
bankdata$married=factor(bankdata$married)
bankdata$car=factor(bankdata$car)
bankdata$save_act=factor(bankdata$save_act)
bankdata$current_act=factor(bankdata$current_act)
bankdata$mortgage=factor(bankdata$mortgage)
bankdata$pep=factor(bankdata$pep)
str(bankdata)
#Change "YES" to "[variable_name]=YES"
bankdata$married=dplyr::recode(bankdata$married, YES="married=YES", NO="married=NO")
bankdata$car=dplyr::recode(bankdata$car, YES="car=YES", NO="car=NO")
bankdata$save_act=dplyr::recode(bankdata$save_act, YES="save_act=YES", NO="save_act=NO")
bankdata$current_act=dplyr::recode(bankdata$current_act, YES="current_act=YES", NO="current_act=NO")
bankdata$mortgage=dplyr::recode(bankdata$mortgage, YES="mortgage=YES", NO="mortgage=NO")
bankdata$pep=dplyr::recode(bankdata$pep, YES="pep=YES", NO="pep=NO")
#Remove ID
bankdata <- bankdata[,-(1), drop=FALSE]
#Load into apriori
myRules = apriori(bankdata, parameter = list(supp = 0.001, conf = 0.9, maxlen = 3))
inspect(myRules[1:30])
SortedRulesC<- sort(myRules, by="confidence", decreasing = TRUE)
inspect(SortedRulesC[1:30])
SortedRulesL <- sort(myRules, by="lift", decreasing = TRUE)
inspect(SortedRulesL[1:30])
SortedRulesS<-sort(myRules, by="support", decreasing = TRUE)
inspect(SortedRulesS[1:30])
library(arulesViz)
plot(SortedRulesS[1:20], method="graph", engine='interactive', shading="confidence")
#Set pep to lhs

peprules = apriori(bankdata, parameter=list(supp=0.001,conf = 0.15,minlen=2), appearance = list(default="lhs",rhs=c("pep=pep=YES","pep=pep=NO")),control = list(verbose=F))
peprulesC<-sort(peprules, decreasing=TRUE,by="confidence")
inspect(peprulesC[1:100])
peprulesL<-sort(peprules, decreasing=TRUE,by="lift")
inspect(peprulesL[1:100])
peprulesS<-sort(peprules, decreasing=TRUE,by="support")
inspect(peprulesS[1:100])
plot(peprulesC[1:20], method="graph", engine='interactive', shading="confidence")
plot(peprulesL[1:20], method="graph", engine='interactive', shading="confidence")
plot(peprulesS[1:20], method="graph", engine='interactive', shading="confidence")
