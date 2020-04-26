#Lara Clasen
#DSC 680 Term Project 3

#DATA EXPLORATION

library(RColorBrewer)

customerData = read.csv("Mall_Customers.csv")

str(customerData)

names(customerData)

head(customerData)

summary(customerData$Age)

sd(customerData$Age)

summary(customerData$Annual.Income..k..)

sd(customerData$Annual.Income..k..)

sd(customerData$Spending.Score..1.100.)

#CUSTOMER GENDER VISUALIZATION

a=table(customerData$Gender)
barplot(a,main="Gender DIstribution",
        ylab="Count",
        xlab="Gender",
        col=brewer.pal(n = 3, name = 'Set2'),
        legend=rownames(a))

pct=round(a/sum(a)*100)

lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")

library(plotrix)

pie3D(a,labels=lbs,
      main="Ratio of Female and Male Customers",
      col=brewer.pal(n = 3, name = 'Set2'))

#VISUALIZATION OF AGE DISTRIBUTION

summary(customerData$Age)

hist(customerData$Age,
     main="Customer Age Distribution",
     col=brewer.pal(n = 3, name = 'Set2'),
     xlab="Age",
     ylab="Frequency",
     labels=TRUE)

boxplot(customerData$Age,
        col=brewer.pal(n = 3, name = 'Set2'),
        main="Descriptive Analysis of Age")

#ANALYSIS OF CUSTOMER ANNUAL INCOME

summary(customerData$Annual.Income..k..)

hist(customerData$Annual.Income..k..,
     col=brewer.pal(n = 3, name = 'Set2'),
     main="Annual Income",
     xlab="Annual Income",
     ylab="Frequency",
     labels=TRUE)

plot(density(customerData$Annual.Income..k..),
     col="blue",
     main="Annual Income Density Plot",
     xlab="Annual Income",
     ylab="Density")


polygon(density(customerData$Annual.Income..k..),
        col=brewer.pal(n = 3, name = 'Set2'))

#ANALYZING CUSTOMER SPENDING SCORES

summary(customerData$Spending.Score..1.100.)

boxplot(customerData$Spending.Score..1.100.,
        horizontal=TRUE,
        col=brewer.pal(n = 3, name = 'Set2'),
        main="Spending Score Boxplot")

hist(customerData$Spending.Score..1.100.,
     main="Spending Score Histogram",
     xlab="Spending Score",
     ylab="Frequency",
     col=brewer.pal(n = 3, name = 'Set2'),
     labels=TRUE)

#K-MEANS ALGORITHM

library(purrr)

set.seed(123)

#Calculate the within cluster sum of squares
iss <- function(k) {
  kmeans(customerData[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Within Cluster Sum of Squares")

#AVERAGE SILHOUETTE METHOD

library(cluster)
library(gridExtra)
library(grid)

k2<-kmeans(customerData[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")

s2<-plot(silhouette(k2$cluster,dist(customerData[,3:5],"euclidean")))

k3<-kmeans(customerData[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")

s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customerData[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")

s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customerData[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")

s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customerData[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")

s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customerData[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")

s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customerData[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")

s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customerData[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")

s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customerData[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")

s10<-plot(silhouette(k10$cluster,dist(customerData[,3:5],"euclidean")))


library(NbClust)
library(factoextra)

fviz_nbclust(customerData[,3:5], kmeans, method = "silhouette")

#GAP STATISTIC METHOD

set.seed(125)

stat_gap <- clusGap(customerData[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

k6<-kmeans(customerData[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

k6$cluster
k6$centers
k6$totss
k6$tot.withinss
k6$betweenss

#VISUALIZING CLUSTER RESULTS

set.seed(1)

ggplot(customerData, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

ggplot(customerData, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


###



