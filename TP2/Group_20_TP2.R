rm(list=ls())
####################################################
### Functions
####################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c('dplyr', 'ggplot2', 'tidyverse', 'cluster','factoextra', 'prophet', 'magrittr', 'taRifx', 'Metrics', 'e1071')  
installIfAbsentAndLoad(needed)
#'readr'

df_air <- read.csv('air_visit_data.csv')
df_air_store <- read.csv('air_store_info.csv')


####plots total restaurant visitors accross all stores in training data
par(mfrow=c(2,1), cex=0.7)
df_air %>% 
  group_by(visit_date) %>% 
  summarize(visitors = sum(visitors)) %>% 
  plot(type='l', main='Overall Visitors')

#annual visitors by store_id
total_visits<- df_air %>% group_by(air_store_id) %>% summarize(visitors = sum(visitors))

####combines store visit info with store location info into one dataframe
air_merged <- total_visits %>% 
  dplyr::left_join(df_air_store, by='air_store_id', how='left')

air_merged2<- air_merged[, c(1,2)]

########cluster analysis#######################

#######Gowers Method with 5 Clusters######
#creates cluster of our data with num of visitors included
FundDist <- as.matrix(daisy(air_merged, metric = 'gower', stand = TRUE))

#tells us the optimal number of clusters to use
fviz_nbclust(FundDist, pam, method = "silhouette")+
  theme_classic()

#the output above indicates that 10 is the optimal number of clusters. We could also use 7 or even 5
# After running several simulations we found 5 clusters achieves the best results. 
pam.res <- pam(FundDist, 5) # put the number you select in here


air_merged <- cbind(air_merged, cluster = pam.res$cluster) # add the cluster numbers to  a new column in the data set

# 3 lines below create dataframe containing all store daily visitors and cluster IDs
air_merged<- air_merged[, -2] #drops annual visitors 
all_store_info<- df_air %>%
  dplyr::left_join(air_merged, by='air_store_id', how='left')

all_store_info$visit_date <- as.Date(all_store_info$visit_date, origin = '1899-12-30') #format date for prophet funciton


########cluster dataframes#######

cluster_1<- all_store_info[all_store_info$cluster == 1,]
cluster_1_stores<- unique(cluster_1$air_store_id)
cluster_2<- all_store_info[all_store_info$cluster == 2,]
cluster_2_stores<- unique(cluster_2$air_store_id)
cluster_3<- all_store_info[all_store_info$cluster == 3,]
cluster_3_stores<- unique(cluster_3$air_store_id)
cluster_4<- all_store_info[all_store_info$cluster == 4,]
cluster_4_stores<- unique(cluster_4$air_store_id)
cluster_5<- all_store_info[all_store_info$cluster == 5,]
cluster_5_stores<- unique(cluster_5$air_store_id)


#Blocks of code below contains function that runs 42 day forecast based on avg demand per cluster. This is an improvement upon 
#Konrad Banachewicz's score (kaggle Notebook author) because our forecast is based off of the average demand of each
#cluster rather than an average of the enture data set. This is essential because we only want to base our forecasts off 
#of restaurants of similar revenue structure. It wouldn't make sense to forecast the performance of a 5 star restaurant based on the demand 
#of the local coffee shop down the street. In short, we want to compare apples to apples.

#######Author's Forecast########
group_mean<- aggregate(visitors~visit_date, data = all_store_info, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]
kaggle_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}

store_names<- unique(all_store_info$air_store_id)
kaggle_RMSE<- c()
for (i in store_names){
  temp_store_rmse<-kaggle_forecast(i)
  kaggle_RMSE<- c(kaggle_RMSE, temp_store_rmse)
}
kaggle_RMSE<- data.frame(kaggle_RMSE) #one store doesn't cover the timeline so it was removed
kaggle_RMSE<- na.omit(kaggle_RMSE)
kaggle_RMSE<-mean(kaggle_RMSE$kaggle_RMSE)



##########cluster 1 forecast##########
group_mean<- aggregate(visitors~visit_date, data = cluster_1, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]
cluster_1_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}
cluster_1_RMSE<- c()
for (i in cluster_1_stores){
  temp_store_rmse<-cluster_1_forecast(i)
  cluster_1_RMSE<- c(cluster_1_RMSE, temp_store_rmse)
}
#(cluster_1_RMSE<-mean(cluster_1_RMSE))
cluster_1_RMSE<- data.frame(cluster_1_RMSE) #one store doesn't cover the timeline so it was removed
cluster_1_RMSE<- na.omit(cluster_1_RMSE)
cluster_1_RMSE<-mean(cluster_1_RMSE$cluster_1_RMSE)


##########cluster 2 forecast##########
group_mean<- aggregate(visitors~visit_date, data = cluster_2, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]
cluster_2_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}
cluster_2_RMSE<- c()
for (i in cluster_2_stores){
  temp2_store_rmse<-cluster_2_forecast(i)
  cluster_2_RMSE<- c(cluster_2_RMSE, temp2_store_rmse)
}
cluster_2_RMSE<- data.frame(cluster_2_RMSE) #one store doesn't cover the timeline so it was removed
cluster_2_RMSE<- na.omit(cluster_2_RMSE)
cluster_2_RMSE<-mean(cluster_2_RMSE$cluster_2_RMSE)


###############cluster 3 forecast######
group_mean<- aggregate(visitors~visit_date, data = cluster_3, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]
cluster_3_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}
cluster_3_RMSE<- c()
for (i in cluster_3_stores){
  temp_store_rmse<-cluster_3_forecast(i)
  cluster_3_RMSE<- c(cluster_3_RMSE, temp_store_rmse)
}

cluster_3_RMSE<- mean(cluster_3_RMSE)


###############cluster 4 forecast#######
group_mean<- aggregate(visitors~visit_date, data = cluster_4, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]
cluster_4_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}
cluster_4_RMSE<- c()
for (i in cluster_4_stores){
  temp_store_rmse<-cluster_4_forecast(i)
  cluster_4_RMSE<- c(cluster_4_RMSE, temp_store_rmse)
}
#cluster_4_RMSE<-mean(cluster_4_RMSE)
cluster_4_RMSE<- data.frame(cluster_4_RMSE) #one store doesn't cover the timeline so it was removed
cluster_4_RMSE<- na.omit(cluster_4_RMSE)
cluster_4_RMSE<-mean(cluster_4_RMSE$cluster_4_RMSE)


###############cluster 5 forecast#######
group_mean<- aggregate(visitors~visit_date, data = cluster_5, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]
cluster_5_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}
cluster_5_RMSE<- c()
for (i in cluster_5_stores){
  temp_store_rmse<-cluster_5_forecast(i)
  cluster_5_RMSE<- c(cluster_5_RMSE, temp_store_rmse)
}
cluster_5_RMSE<- data.frame(cluster_5_RMSE) #one store doesn't cover the timeline so it was removed
cluster_5_RMSE<- na.omit(cluster_5_RMSE)
cluster_5_RMSE<-mean(cluster_5_RMSE$cluster_5_RMSE)


######## RMSE 5 Cluster Output######

(five_cluster_RMSE<- sum(cluster_2_RMSE, cluster_1_RMSE, cluster_3_RMSE, cluster_4_RMSE, cluster_5_RMSE)/5)
#actual RMSE

(five_cluster_customer_variance<- sqrt(exp(five_cluster_RMSE))) #reverses the log and then takes the square root in order to 
# make our results more interpretable 

#On average our forecast will over/under estimate daily customers by 1.5

########################Gowers Method with 2 Clusters############

#run same script as above except we create clusters using only aggregated annual customers.
# We used this approach in order to compare companies based exclusively on customer size

air_merged <- total_visits %>% 
  dplyr::left_join(df_air_store, by='air_store_id', how='left')

air_merged2<- air_merged[, c(1,2)] #grabs store name and total customers 
########cluster analysis#######################

#######Gowers######
#creates cluster of our data with num of visitors included
FundDist2 <- as.matrix(daisy(air_merged2, metric = 'gower', stand = TRUE))

#tells us the optimal number of clusters to use
fviz_nbclust(FundDist2, pam, method = "silhouette")+
  theme_classic()


pam.res <- pam(FundDist2, 2) # put the number you select in here

air_merged <- cbind(air_merged, cluster = pam.res$cluster) # add the cluster numbers to  a new column in the data set

# 3 lines below create dataframe containing all store daily visitors and cluster IDs
air_merged<- air_merged[, -2] #drops annual visitors 
all_store_info<- df_air %>%
  dplyr::left_join(air_merged, by='air_store_id', how='left')

all_store_info$visit_date <- as.Date(all_store_info$visit_date, origin = '1899-12-30') #format date for prophet funciton


cluster_1<- all_store_info[all_store_info$cluster == 1,]
cluster_1_stores<- unique(cluster_1$air_store_id)
cluster_2<- all_store_info[all_store_info$cluster == 2,]
cluster_2_stores<- unique(cluster_2$air_store_id)

##########cluster 1 forecast 2c##########
group_mean<- aggregate(visitors~visit_date, data = cluster_1, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]
cluster_1_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}
cluster_1_RMSE_c2<- c()
for (i in cluster_1_stores){
  temp_store_rmse<-cluster_1_forecast(i)
  cluster_1_RMSE_c2<- c(cluster_1_RMSE_c2, temp_store_rmse)
}
cluster_1_RMSE_c2<- data.frame(cluster_1_RMSE_c2) #one store doesn't cover the timeline so it was removed
cluster_1_RMSE_c2<- na.omit(cluster_1_RMSE_c2)
cluster_1_RMSE_c2<-mean(cluster_1_RMSE_c2$cluster_1_RMSE_c2)

##########cluster 2 forecast 2c##########
group_mean<- aggregate(visitors~visit_date, data = cluster_2, FUN = mean)
train<- group_mean %>% filter(visit_date < '2017-03-12')
names(train)<- c('ds', 'y')#need to rename cols for prohpet packacge
train$y<- log(train$y) #log our data for more accurate results
m<- prophet(train)
future<- make_future_dataframe(m, periods = 42)
forecast<- predict(m, future)
results<- forecast %>% filter(ds >= '2017-03-11')
results<- results[, c(1, 16)]

cluster_2_forecast<- function(i){
  store1<- all_store_info[all_store_info$air_store_id == i,]
  group_result<- store1 %>% filter(visit_date >= '2017-03-12')
  group_result$visit_date<- as.Date(group_result$visit_date, origin = '1899-12-30')
  group_result<- group_result[, c(2,3)]
  names(results)<- c('visit_date', 'y')
  results$visit_date<- as.Date(results$visit_date, origin = '1899-12-30')
  store_forecast<-group_result %>%
    dplyr::left_join(results, by = 'visit_date', how='left')
  output <-rmse(log(store_forecast$visitors), store_forecast$y)
  return(output)
}
cluster_2_RMSE_c2<- c()
for (i in cluster_2_stores){
  temp2_store_rmse<-cluster_2_forecast(i)
  cluster_2_RMSE_c2<- c(cluster_2_RMSE_c2, temp2_store_rmse)
}
cluster_2_RMSE_c2<- data.frame(cluster_2_RMSE_c2) #one store doesn't cover the timeline so it was removed
cluster_2_RMSE_c2<- na.omit(cluster_2_RMSE_c2)
cluster_2_RMSE_c2<-mean(cluster_2_RMSE_c2$cluster_2_RMSE_c2)

######## RMSE 2 Cluster Output######

(two_cluster_RMSE<- sum(cluster_1_RMSE_c2, cluster_2_RMSE_c2)/2)
#actual RMSE

(two_cluster_customer_variance<- sqrt(exp(two_cluster_RMSE)))

#On average our forecast will over/under estimate daily customers by 1.4

######Final Results######
'Kaggle Author RMSE'
kaggle_RMSE

"Group 20 best RMSE"
two_cluster_RMSE

