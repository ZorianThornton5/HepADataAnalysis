library(ggplot2)
library(dplyr)
library("arules")
library("arulesViz")

setwd("C:/Users/Zorian Thornton/Dropbox/Data Analytics Competition/Dataset Summaries")
data <- read.csv('factored.csv')
category <- data[, c(5:7,11)]

# Make factors
citizen.levels <- c('citizen', 'not a citizen', 'refused', 'dont know')
category$Citizenship.status <- factor(citizen.levels)

marriage.levels <- c('married', 'widowed', 'divorced', 'separtated', 'never married', 'living with partner', 'refused', 'dont know')
category$Marital.status <- factor(marriage.levels)

service.levels <- c('Served at Home','Refused to Answer', 'Served Overseas')
category$Served.active.duty.in.US.Armed.Forces.1 <- factor(service.levels)

# Apriori Rules
rules <- apriori(category, parameter = list(supp=0.01, conf=0.6, minlen = 2, target = 'rules'))
length(rules)
hepA <- subset( rules, subset = rhs %pin% "HepA.antibody=positive")
plot(hepA, method = "paracoord", control=list(alpha=.5, reorder=TRUE,  main = "Positive HepA Association Rules"))
hepAdf <- as(hepA, 'data.frame')

rules2 <- apriori(category, parameter = list(supp=0.01, conf=0.6, minlen = 2, target = 'rules'))
length(rules2)
hepA2 <- subset( rules2, subset = rhs %pin% "HepA.antibody=negative")
plot(hepA2, method = "paracoord", control=list(alpha=.5, reorder=TRUE, main = "Negative HepA Association Rules"))
hepA2df <- as(hepA2, 'data.frame')
# HepA test Results

# Create a subset of the data only concerning Mexicans.
cat2 <- subset(category, category$Race.Hispanic.origin.w..NH.Asian == 'mex')


# Stacked barplot for race
dat <- data[, c(5,11)]
dat <- data.frame(table(data$Race.Hispanic.origin.w..NH.Asian, data$HepA.antibody))
names(dat) <- c("Race", "Results", "Count")
ggplot(data = dat, aes(x = Race, y = Count, fill = Results)) + geom_bar(stat = "identity") + labs(xlab = "Race", ylab = "Count", title = "HepA Results by Race") + scale_fill_manual(values=c("orange","maroon")) + theme_bw()

# Stacked barplot for Citizenship
dat2 <- data.frame(table(cat2$Citizenship.status, cat2$HepA.antibody))
names(dat2) <- c("CitizenshipStatus", "Results", "Count")
ggplot(dat2, aes(x = CitizenshipStatus, y = Count, fill = Results )) + geom_bar(stat = "identity") + labs(xlab = "Citizenship Status", ylab = "Count", title = "HepA Results for Mexicans by Citizenship Status")+ scale_fill_manual(values=c("orange","maroon")) + theme_bw()

# Stacked barplot for marital status
dat3 <- data.frame(table(cat2$Marital.status, cat2$HepA.antibody))
names(dat3) <- c("MaritalStatus", "Results", "Count")
ggplot(dat3, aes(x = MaritalStatus, y = Count, fill = Results )) + geom_bar(stat = "identity") + labs(xlab = "Marital Status", ylab = "Count", title = "HepA Results for Mexicans by Marital Status") + scale_fill_manual(values=c("orange","maroon")) + theme_bw()

# Stacked barplot for military service.
cat3 <- subset(cat2, cat2$Served.active.duty.in.US.Armed.Forces.1 != 'Refused to Answer')
dat4 <- data.frame(table(cat3$Served.active.duty.in.US.Armed.Forces.1, cat3$HepA.antibody))
dat4 <- dat4[-c(1,4),]
names(dat4) <- c("Served", "Results", "Count")
ggplot(dat4, aes(x = Served, y = Count, fill = Results)) + geom_bar(stat = "identity") + labs(xlab = "Served in Armed Forces Overseas?", ylab = "Count", title = "HepA Results for Mexicans by Military Service")  + scale_fill_manual(values=c("orange","maroon")) + theme_bw()


