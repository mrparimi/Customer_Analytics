wcd <- read.csv(file = "data/wcd.csv",header = TRUE)
summary(wcd)
library(dplyr)
library(ggplot2)
#sgementation based region
wcd_1 <- wcd %>% filter(Region==1)
#segment based on channel in region 1
wcd_1_1 <- wcd_1%>%filter(wcd_1$Channel==1)
x=vector()
for (i in 1:10) {
  wcd_1_1_loop <- kmeans(wcd_1_1[,3:8],i,nstart = 20)
  x[i]=wcd_1_1_loop$tot.withinss
  i=i+1
}
k=1:10
plot(k,x)
wcd_1_1_segmentation <- kmeans(wcd_1_1[,3:8],2,nstart = 20)
wcd_1_1_clustered <- data.frame(wcd_1_1,wcd_1_1_segmentation$cluster)
wcd_1_1_group <- wcd_1_1_clustered%>%group_by(wcd_1_1_segmentation.cluster)%>%
  summarise(Milk=sum(Milk),Grocery=sum(Grocery),Frozen=sum(Frozen),
  Detergents_Paper=sum(Detergents_Paper),Delicassen=sum(Delicassen))
means_1_1<-wcd_1_1_group[,1:length(wcd_1_1_group)]
library(reshape2)
means.long<-melt(means_1_1,id.vars="wcd_1_1_segmentation.cluster")

ggplot(means.long,aes(x=variable,y=value,fill=factor(wcd_1_1_segmentation.cluster)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="cluster",
                      breaks=c(1, 2),
                      labels=c("1", "2"))+
  xlab("Department")+ylab("total sales")
