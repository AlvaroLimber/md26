rm(list=ls())
library(tidyverse)
library(arulesViz)
library(arules)
#dataset
data("Groceries")
bdt <- Groceries
#Apriori
reglas <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5))
reglas
top20 <- sort(reglas, by = "lift")[1:20]
plot(top20)
plot(top20, interactive=T)
plot(top20, method = "graph")
plot(top20, method = "graph", engine = "htmlwidget")
plot(top20, method = "matrix")
plot(top20, method = "grouped matrix")
ruleExplorer(top20)
ruleExplorer(reglas)
ruleExplorer(ma3)
inspect(top20)

soda_l=apriori(bdt, parameter=list(supp=0.001, 
                                   conf =0.1, minlen=2), 
               appearance = list(default="rhs",lhs="soda"))

soda_r=apriori(bdt, parameter=list(supp=0.001, 
                                   conf =0.1, minlen=2), 
               appearance = list(default="lhs",rhs="soda"))
soda_l
soda_r
soda_r<-soda_r[!is.redundant(soda_r)]
ruleExplorer(soda_r)

subset(reglas, lhs %in% c("soda", "yogurt"))
subset(reglas, lhs %ain% c("soda", "sausage"))
subset(reglas, rhs %in% c("sausage"))

###ECLAT
res_eclat <- eclat(bdt, 
                   parameter = list(supp = 0.001, minlen = 2))
res_eclat
inspect(sort(res_eclat, by = "support")[1:5])
plot(res_eclat)

reglas_eclat<-ruleInduction(res_eclat, 
                            transactions=bdt,
                            confidence=0.5)
reglas_eclat
