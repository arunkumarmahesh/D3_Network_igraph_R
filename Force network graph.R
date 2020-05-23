############# network graph ##############
bcl <- read.xlsx("QS_GVKBIO_Diabetes_71 papers_14thJuly2015.xlsx",startRow =2)
bcl <- subset(bcl,
  select=c(trial,drug1, drug2))

bcl$drug1 <- gsub("control","placebo",bcl$drug1)

bcl$drug3 <- ifelse(!is.na(bcl$drug2),paste(bcl$drug1,bcl$drug2,sep="+"),bcl$drug1)

bcl$drug1 <- bcl$drug2 <- NULL


bcl <- unique(bcl)

bcl$group <- cumsum(!duplicated(bcl$trial))

bcl$group1 <- ifelse(bcl$group==1,0,"")


for(i in unique(bcl$trial)){
for(j in 3:length(bcl$group[bcl$trial==i])){
  bcl$group1[bcl$trial==i & j] <- bcl$group[bcl$trial==i][j]-bcl$group[1]
  }
}


unique(bcl$group1)

df <- data.frame(trial = unique(bcl$drug3),stringsAsFactors = F)
df$group <- 71+1:nrow(df)

bcl$source <- NA

for(i in unique(bcl$drug3)){
  bcl$source[bcl$drug3==i] <- replace(bcl$source[bcl$drug3==i],is.na(bcl$source[bcl$drug3==i]),
    df$group[df$trial==i])
}

#x <- data.frame(x=table(bcl$drug3))

#for(i in unique(bcl$drug3)){
  bcl$target[bcl$drug3==i] <- replace(bcl$target[bcl$drug3==i],
    is.na(bcl$target[bcl$drug3==i]),x$x.Freq[x$x.Var1==i])
}



#bcl1 <- data.frame(source=bcl$group1,target=bcl$source)

x<-unique(bcl$trial)

bcl1 <- data.frame(trial=c(x),group=c(0:71))

bcl1 <- rbind(bcl1,df)


#bcl1 <- data.frame(trial=c(x,bcl$drug3),group=c(0:71,bcl$source))

bcl$trial <- bcl$drug3 <-bcl$group <- NULL
bcl[,1:2]<- apply(bcl[,1:2],2,as.numeric)
bcl1$trial <- as.character(bcl1$trial)

d3ForceNetwork(bcl,bcl1,Source = "group1",Target = "source",NodeID = "trial",Group = "group",
   width = 1000, height = 1000,fontsize = 7,
               opacity = 0.9,file = "Force.html")
