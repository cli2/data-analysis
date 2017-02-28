country <- read.table("countrydata_withregion.tsv",head=TRUE,sep = "\t",quote = "", fileEncoding="windows-1252")
head(country,n=15)

logArea <- log(country$area)
logPop <- log(country$population)
library(ggplot2)
qplot(logArea,logPop, xlab="log(area)",ylab="log(population)")

aggArea <- aggregate(country$area,by=list(region = country$region), sum)
aggPop <- aggregate(as.numeric(country$population),by=list(region = country$region), sum)
pie(aggArea$x,labels=aggArea$region,main="Area of Regions")
pie(aggPop$x,labels=aggPop$region,main="Population of Regions")

popPerKm <- aggPop$x / aggArea$x
newFrame <- data.frame(popPerKm,region=aggPop$region)
newFrame <- newFrame[order(newFrame$popPerKm,decreasing = TRUE),]
qplot(data=newFrame,x=factor(region), y=factor(popPerKm), geom="bar",stat = 'identity',xlab="region",ylab="Population per sq km of Regions") +theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggplot(newFrame, aes(x=region,y = popPerKm)) +geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x="region",y="Population per sq km of Regions")