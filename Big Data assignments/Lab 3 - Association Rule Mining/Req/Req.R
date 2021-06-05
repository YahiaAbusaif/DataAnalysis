rm(list=ls())

setwd("F:\\CMP 2020\\Big Data\\Labs\\Lab 3 - Association Rule Mining\\Lab 3 - Association Rule Mining\\Req")

#(2) Load libraries
library(arules)
library(arulesViz)

#(3)The file contains no header line
tr = read.transactions('AssociationRules.csv', format='basket', sep=' ')


#(4)Display first 100
inspect(head(tr, 100), linebreak=FALSE)

#(5) Frequency
freq = summary(tr)@itemSummary
# The 1st most frequent item and its frequency
freq[1]
# The 2nd most frequent item and its frequency
freq[2]

#(6) Plot the top 5 items frequencies
freq[1:5]
itemFrequencyPlot(tr, type="absolute", topN=5, main="Absolute Frequency")

#(7) Rules by Apriori algorithm
rules = apriori(tr, parameter = list(supp = 0.01, conf = 0.5, minlen=2))
summary(rules)


n = 6
#(8)
rules_by_sup = sort(rules, by='support')
inspect(head(rules_by_sup, n))

#(9)
rules_by_conf = sort(rules, by='confidence')
inspect(head(rules_by_conf, n))

#(10)
rules_by_lift = sort(rules, by='lift')
inspect(head(rules_by_lift, n))

#(11)
plot(rules, measure='support', shading='lift')
## The most interesting rules are those with high lift since high lift (say >> 1)
# means that the iteams are dependent and not just coincidentally happening together