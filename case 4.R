rm(list=ls())

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(grf)

d = read.csv("/Users/kouyukihare/Desktop/design/hm4/test_data_1904.csv")

##################part a#################
dt = data.table(d)
dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),seOpen = sd(open)/sqrt(.N), seClick=sd(click)/sqrt(.N), sePurch = sd(purch)/sqrt(.N),.N),by = .(group)]
dagg

dodge = position_dodge(width=1)
ggplot(aes(x=group,y=purch,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
    geom_bar(position=dodge,stat="identity",col=2:3,fill=2:3) + 
    geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")

summary(lm(purch~group,data=d))
d$email = (d$group != "ctrl")

######################part b###################
##recent purchase
hist(d$last_purch, 
     xlab="Days Since Last Purchase", ylab="Customers", 
     main="Histogram of Days Since Last Purchase")
d$recentPurch = (d$last_purch < 60)
dt = data.table(d)

dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),sePurch = sd(purch)/sqrt(.N),.N),by = .(group,recentPurch)]
dagg

dodge = position_dodge(width=1)
ggplot(aes(fill=group,y=purch,x=recentPurch,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
    geom_bar(position=dodge,stat="identity") + 
    geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")

##past-purchase
hist(d$past_purch, 
     xlab="Past Purchase", ylab="Customers", 
     main="Histogram of how much the customer avg spend in past visits")
d$buysomething = (d$past_purch > 0)
dt = data.table(d)

dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),sePurch = sd(purch)/sqrt(.N),.N),by = .(group,buysomething)]
dagg

dodge = position_dodge(width=1)
ggplot(aes(fill=group,y=purch,x=buysomething,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
    geom_bar(position=dodge,stat="identity") + 
    geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")

##frequent visitors
hist(d$visits, 
     xlab="Past Purchase", ylab="Customers", 
     main="Histogram of how much the customer avg spend in past visits")
d$frequentvisitor = (d$visits > 5)
dt = data.table(d)

dagg = dt[,.(open = mean(open), click=mean(click), purch = mean(purch),sePurch = sd(purch)/sqrt(.N),.N),by = .(group,frequentvisitor)]
dagg

dodge = position_dodge(width=1); ##to form constant dimensions
ggplot(aes(fill=group,y=purch,x=frequentvisitor,ymax=purch+sePurch,ymin=purch-sePurch),data=dagg)+
    geom_bar(position=dodge,stat="identity") + 
    geom_errorbar(position=dodge)
labs(x="Group",y="Purchases")

##interaction test
summary(lm(purch~recentPurch+group:recentPurch, data = d))
summary(lm(purch~buysomething+group:buysomething, data = d))
summary(lm(purch~frequentvisitor+group:frequentvisitor, data = d))

d$profit = d$purch*0.3-0.1

##cluster
seg1 = d[d$recentPurch == TRUE & d$visits >5 & d$buysomething == TRUE, ]
seg1$segmentation = 1
seg2 = d[d$recentPurch ==FALSE & d$visits >5 & d$buysomething == TRUE, ]
seg2$segmentation = 2
seg3 = d[d$recentPurch == TRUE & d$visits <=5 & d$buysomething == TRUE, ]
seg3$segmentation = 3
seg4 = d[d$recentPurch == FALSE & d$visits <=5 & d$buysomething == TRUE,]
seg4$segmentation = 4
seg5 = d[d$recentPurch == TRUE & d$visits >5 & d$buysomething ==FALSE, ]
seg5$segmentation = 5
seg6 = d[d$recentPurch ==FALSE & d$visits >5 & d$buysomething == FALSE, ]
seg6$segmentation = 6
seg7 = d[d$recentPurch == TRUE & d$visits <=5 & d$buysomething == FALSE, ]
seg7$segmentation = 7
seg8 = d[d$recentPurch == FALSE & d$visits <=5 & d$buysomething == FALSE,]
seg8$segmentation = 8
df = rbind(seg1,seg2,seg3,seg4,seg5,seg6,seg7,seg8)

tapply(seg1$profit, seg1$group, sum)
tapply(seg2$profit, seg2$group, sum)
tapply(seg3$profit, seg3$group, sum)
tapply(seg4$profit, seg4$group, sum)
tapply(seg5$profit, seg5$group, sum)
tapply(seg6$profit, seg6$group, sum)
tapply(seg7$profit, seg7$group, sum)
tapply(seg8$profit, seg8$group, sum)

########part c #########
###### causal forest #####
cf_size <- nrow(d) 
cf_set = sample(nrow(d),cf_size)
treat <- d$email[cf_set]
response <- d$purch[cf_set]
colnames(d)

baseline <- d[cf_set, c("last_purch", "past_purch","visits", "chard", "sav_blanc", "syrah", "cab")]
cf <- causal_forest(baseline, response, treat)
print(cf)
average_treatment_effect(cf, method="AIPW")

new_cust = d[,c("last_purch", "past_purch","visits", "chard", "sav_blanc", "syrah", "cab")]
pre = predict(cf, new_cust, estimate.variance = TRUE)

hist(predict(cf)$predictions, 
     main="Histogram of Purchase Lift", 
     xlab="Purchase Lift for Email", ylab="Customers")

new_cust$score = pre$predictions*0.3-0.1
new_cust$target = d$score>0
summary(new_cust[new_cust$target == TRUE,])
summary(new_cust[new_cust$target == FALSE,])

d$score =  pre$predictions*0.3-0.1
d$target = d$score>0
write.csv(d,'target.csv')


