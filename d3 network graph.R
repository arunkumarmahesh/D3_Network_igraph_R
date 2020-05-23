

library(RCurl)
library(d3Network)


############# network graph ##############
bcl <- read.xlsx("QS_GVKBIO_Diabetes_71 papers_14thJuly2015.xlsx",startRow =2)
bcl <- subset(bcl,
  select=c(trial,drug1, drug2))

bcl$drug1 <- gsub("control","placebo",bcl$drug1)

bcl$drug3 <- ifelse(!is.na(bcl$drug2),paste(bcl$drug1,bcl$drug2,sep="+"),bcl$drug1)

bcl$drug1 <- bcl$drug2 <- NULL


bcl <- unique(bcl)

bcl$group <- cumsum(!duplicated(bcl$trial))

bcl1$value <- rep(1:15,length=157)

bcl$target<-NA

x <- data.frame(x=table(bcl$drug3))

for(i in unique(bcl$drug3)){
  bcl$target[bcl$drug3==i] <- replace(bcl$target[bcl$drug3==i],
    is.na(bcl$target[bcl$drug3==i]),x$x.Freq[x$x.Var1==i])
}



bcl1 <- data.frame(source=bcl$trial,target=bcl$drug3)

bcl$drug3<-bcl$value<-bcl$target <-NULL

bcl$drug <- bcl1$target




d3ForceNetwork(bcl1,bcl,Source = "source",Target = "target",NodeID = "trial",Group = "group",Value = "value",
   width = 1000, height = 1000,fontsize = 7,
               opacity = 0.9,file = "Force.html")

NetworkData <- bcl

d3SimpleNetwork(NetworkData, width = 1000, height = 1000,fontsize = 7,file = "ExampleGraph.html",
  linkColour = "silver",textColour = "black", opacity = 0.9,nodeColour = "red",
  nodeClickColour="red")


