library(tm)
library(tidyverse)
library(slam)
library(proxy)
library(arules)
library(arulesViz)

readerPlain = function(fname) {
  readPlain(elem=list(content=readLines(fname)), 
            id = fname, language = 'en')
}

grocery_raw = readerPlain('./groceries.txt')


df = as.data.frame(grocery_raw$content)

df$customer = seq.int(nrow(df))

names(df)[1] = 'grocery_basket'
df$customer = factor(df$customer)

df = separate_rows(df,grocery_basket,1,sep=',')

grocery_basket = split(x=df$grocery_basket, f = df$customer)
grocery_basket = lapply(grocery_basket, unique)

grocery = as(grocery_basket, 'transactions')

grocery_rules = apriori(grocery, parameter = list(support=.005, confidence=.01, maxlen=2))
summary(grocery_rules)
arules::inspect(grocery_rules)
arules::inspect(subset(grocery_rules, support > 0.05))
arules::inspect(subset(grocery_rules, confidence > 0.1))

sub1 = subset(grocery_rules, subset=confidence > 0.18 & support > 0.014)
plot(sub1, method='graph')
plot(head(sub1, 100, by='lift'), method='graph')
