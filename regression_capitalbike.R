library(ggplot2)
library(tree)
library(kknn)
library(MASS)
# install.packages('Hmisc')
library(Hmisc)
library(gtable)
library(gridExtra)
# Let's read the trip data CSV -

trip_data_2011_2012 <- read.csv('data/hour.csv')

# Let's factorize all required columns first

trip_data_2011_2012$season <- as.factor(trip_data_2011_2012$season)
trip_data_2011_2012$yr <- as.factor(trip_data_2011_2012$yr)
trip_data_2011_2012$mnth <- as.factor(trip_data_2011_2012$mnth)
trip_data_2011_2012$hr <- as.factor(trip_data_2011_2012$hr)
trip_data_2011_2012$holiday <- as.factor(trip_data_2011_2012$holiday)
trip_data_2011_2012$weekday <- as.factor(trip_data_2011_2012$weekday)
trip_data_2011_2012$workingday <- as.factor(trip_data_2011_2012$workingday)
trip_data_2011_2012$weathersit <- as.factor(trip_data_2011_2012$weathersit)


# Splitting data into year 2011 and 2012 datasets (used in Exploratory Data Analysis)
trip_data_2011 <- trip_data_2011_2012[trip_data_2011_2012$yr==0,]
trip_data_2012 <- trip_data_2011_2012[trip_data_2011_2012$yr==1,]


#Let's look at a summary
Hmisc::describe(trip_data_2011_2012)

# From this summary, we can see no values are missing in any of the columns

head(trip_data_2011_2012)


get_rmse <- function(pred, test){
  return(sqrt(sum((pred-test)^2)/length(test)))
}


get_rsq <- function(pred, test){
  rss = sum((pred - test) ^ 2)  ## residual sum of squares
  tss <- sum((test - mean(test)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  return (rsq)
}

# Let's first look at distribution of rides per season per type ]of user

# Calculate means to be displayed on boxplot
means_agg_2011 <- aggregate(registered ~  season, trip_data_2011, mean)
means_cas_2011 <- aggregate(casual ~  season, trip_data_2011, mean)

means_agg_2012 <- aggregate(registered ~  season, trip_data_2012, mean)
means_cas_2012 <- aggregate(casual ~  season, trip_data_2012, mean)
# Round off the means
means_agg_2011$registered <- round(means_agg_2011$registered, 2)
means_cas_2011$casual <- round(means_cas_2011$casual, 2)

means_agg_2012$registered <- round(means_agg_2012$registered, 2)
means_cas_2012$casual <- round(means_cas_2012$casual, 2)



# For 2011

plt_2011_season_reg <- ggplot(data = trip_data_2011, aes(x=season, y=registered, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE) +
  geom_text(data = means_agg_2011, aes(label = registered)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"),labels=c('Winter','Spring','Summer','Fall'))

plt_2011_season_reg


plt_2011_season_cas <- ggplot(data = trip_data_2011, aes(x=season, y=casual, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE)+
  geom_text(data = means_cas_2011, aes(label = casual)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"),labels=c('Winter','Spring','Summer','Fall'))

plt_2011_season_cas



# For 2012


plt_2012_season_reg <- ggplot(data = trip_data_2012, aes(x=season, y=registered, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE) +
  geom_text(data = means_agg_2012, aes(label = registered)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"),labels=c('Winter','Spring','Summer','Fall'))
plt_2012_season_reg


plt_2012_season_cas <- ggplot(data = trip_data_2012, aes(x=season, y=casual, fill=season)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE)+
  geom_text(data = means_cas_2012, aes(label = casual)) +
  scale_fill_manual(values=c("#FEF8FA", "#D6F1C6","#C9F1FD","#F9CC87"),labels=c('Winter','Spring','Summer','Fall'))

plt_2012_season_cas


grid.arrange(plt_2011_season_reg, plt_2011_season_cas, plt_2012_season_reg, plt_2012_season_cas, nrow=2, ncol=2)



#ggplot defines an outlier by default as something that's > 1.5*IQR from the borders of the box.


# Let's look at the day of the week wise distribution of rides of casuals and registered 
# users over different seasons

#get only season, day of week and no. of rides

season_1 <- trip_data_2011_2012[trip_data_2011_2012$season=='1',][c('yr','season', 'weekday', 'casual','registered')]
season_2 <- trip_data_2011_2012[trip_data_2011_2012$season=='2',][c('yr','season', 'weekday', 'casual','registered')]
season_3 <- trip_data_2011_2012[trip_data_2011_2012$season=='3',][c('yr','season', 'weekday', 'casual','registered')]
season_4 <- trip_data_2011_2012[trip_data_2011_2012$season=='4',][c('yr','season', 'weekday', 'casual','registered')]




# Winter 

s1_reg <- ggplot(data=aggregate(registered ~  weekday, season_1, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s1_reg


s1_cas <- ggplot(data=aggregate(casual ~  weekday, season_1, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s1_cas



# Spring
s2_reg <- ggplot(data=aggregate(registered ~  weekday, season_2, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s2_reg


s2_cas <- ggplot(data=aggregate(casual ~  weekday, season_2, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s2_cas



# Summer
s3_reg <- ggplot(data=aggregate(registered ~  weekday, season_3, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s3_reg


s3_cas <- ggplot(data=aggregate(casual ~  weekday, season_3, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s3_cas



# Fall
s4_reg <- ggplot(data=aggregate(registered ~  weekday, season_4, mean), aes(x=weekday, y=registered)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s4_reg


s4_cas <- ggplot(data=aggregate(casual ~  weekday, season_4, mean), aes(x=weekday, y=casual)) +
  geom_point(stat='identity') +
  geom_line(aes(group='weekday'),size=1, linetype='dotdash') +
  scale_x_discrete(labels= c("Sun","Mon","Tue",'Wed',"Thurs","Fri", "Sat"))
#s4_cas

grid.arrange(s1_reg, s1_cas, s2_reg, s2_cas, s3_reg, s3_cas, s4_reg, s4_cas, nrow=4, ncol=2)


# Reset plot grid
par(mfrow=c(1, 1))

# Enough variation to consider predicting them separately


# Let's do a simple lin reg on predicting reg rides based on casual rides




regCas <- lm(registered~casual, data = trip_data_2011_2012)
summary(regCas)

plot(trip_data_2011_2012$casual,trip_data_2011_2012$registered)
abline(regCas)




# One hot encoding
# We need to encode all categorical predictors (One Hot Encoding) so that we can pass them as 
# inputs to our regression models
# install.packages(c("mltools","data.table"))

library(mltools)
library(data.table)

# Encoding all predictors but removing the first two columns - 
# 1) instant - This is just a serial no. counter so not needed
# 2) dteday - The date of the observation. This is not needed as we are already considering yr, 
# month, weekday and hour as predictors


# Let's remove months since we have seasons which contains relatively the same information
newdata <- one_hot(as.data.table(trip_data_2011_2012[c(-1,-2,-4,-8)]))


# Let's look a summary of this data
Hmisc::describe(newdata)

# It has 64 predictors since each unique value of our categorical variables was encoded as a separate
# column (predictor)



# Let's split the dataset for registered users and casual users
# We remove columns 'casual' and 'cnt' because we are currently only concerned with predicting
# rides by registered users and we don't need the total count (rides by registered+rides by casuals)


# Removing singularities from our encoded dataset
#reg = newdata[,-c('casual','cnt','season_4','yr_1', 'mnth_12','hr_23','holiday_1', 'weekday_6','workingday_0', 'workingday_1', 'weathersit_4')]

reg = newdata[,-c('casual','cnt','season_4','hr_23', 'holiday_1', 'workingday_1','weathersit_4','mnth_12')]
#reg = newdata[,-c('casual','cnt')]



# Let's first split the dataset into training

set.seed(123)

# Let's take 15000 out of ~17k samples for training 

n = dim(reg)[1]
tr = sample(1:n,15000)

newdata_tr = reg[tr]
newdata_test = reg[-tr]


# Now, let's run a multiple linear regression with all predictors
reg_mlr <- lm(registered~., data=newdata_tr)

summary(reg_mlr)



# We can see 9 co-efficients were not selected because of singularities.
# That's okay. It simply means that those co-efficients were a linear combination of other co-efficients
# and were effectively not providing any new information, hence they were ignored.


plot(reg_mlr)


# We see a more or less random spread of residuals and we get a good adjusted r square value as well.
# Let's try to predict the values on our test set now

reg_pred_mlr <- predict(reg_mlr, newdata=newdata_test)

plot(newdata_test$registered, reg_pred_mlr, xlab = "Test values", ylab ="Fitted Values" )
abline(1,1,col='red',lwd=2)


#get RMSE and RSQ value for test set
rmse.mlr <- get_rmse(reg_pred_mlr,newdata_test$registered)
r2.mlr <- get_rsq(reg_pred_mlr,newdata_test$registered)

# Okay we have around 68% accuracy :D
# That means we have room to improve yay!
# The residuals form a pattern that looks like a parabola meaning we are making systematic errors.


# Let's predict for cube root of Y (maybe polynomial)


root_pow = 2:6
rmse_lm_root = c()
rsq_lm_root = c()



reg_mlr_root <- lm(registered^(1/6)~., data=newdata_tr)
summary(reg_mlr_root)
plot(reg_mlr_root)

par(mfrow=c(1,1))
reg_pred_mlr_root <- predict(reg_mlr_root, newdata=newdata_test)
plot(newdata_test$registered, reg_pred_mlr_root^6,xlab = "Test values", ylab ="Fitted Values")
abline(1,1,col='red',lwd=2)

rmse_lm_root <- get_rmse(reg_pred_mlr_root^6, newdata_test$registered)
rsq_lm_root <- get_rsq(reg_pred_mlr_root^6, newdata_test$registered)

# R Sq for y^2 is the highest and increasing the powers only negatively impacts the R sq.







# Let's plot for log

reg_mlr_log <- lm(log(registered+1)~., data=newdata_tr)
summary(reg_mlr_log)

# Improvement, we got 0.816 RMSE

par(mfrow=c(1,1))
reg_pred_mlr_log <- predict(reg_mlr_log, newdata=newdata_test)
plot(newdata_test$registered, exp(reg_pred_mlr_log))
abline(1,1,col='red',lwd=2)

# RMSE'
rmse_mlr_log <- get_rmse(exp(reg_pred_mlr_log), newdata_test$registered)
rsq_mlr_log <- get_rsq(exp(reg_pred_mlr_log), newdata_test$registered)
plot(reg_mlr_log)




reg_mlr_log_ext <- lm(log(registered+1)~.+log(atemp+0.0001)+log(hum+0.0001)+log(windspeed+0.0001), data=newdata_tr)
summary(reg_mlr_log_ext)

par(mfrow=c(1,1))
reg_pred_mlr_log_ext <- predict(reg_mlr_log_ext, newdata=newdata_test)
plot(newdata_test$registered, exp(reg_pred_mlr_log_ext))
abline(1,1)

# RMSE'
rmse_mlr_log_ext <- get_rmse(exp(reg_pred_mlr_log_ext), newdata_test$registered)
rsq_mlr_log_ext <- get_rsq(exp(reg_pred_mlr_log_ext), newdata_test$registered)
plot(reg_mlr_log_ext)




# Let's see if only weather data helps us predict better

trip_data_subset = trip_data_2011_2012[,c('temp','atemp','hum','windspeed','registered')]

# Train test split
newdata_tr_sub = trip_data_subset[tr,]
newdata_test_sub = trip_data_subset[-tr,]

lm.sub.mlr <- lm(registered~., newdata_tr_sub)
summary(lm.sub.mlr)



# No, not enough data



# Let's see if we can train a less complicated model using the step forward method -

null = lm(registered~1, data=reg[tr,])
#regForward = step(null, scope=formula(reg_mlr), direction="forward", k=log(length(tr))) 

#regBack = step(reg_mlr, direction="backward", k=log(length(tr)))




# Let's try running a random forest model for a baseline 

library(tree)

library(randomForest)

#bag.reg <- randomForest(registered ~ ., data = reg,
 #                          subset = tr, mtry = 7, importance = TRUE)


trip_data_2011_2012_sub <- trip_data_2011_2012[sample(1:nrow(trip_data_2011_2012), 5000),]


tree.reg <- tree(registered~.-instant-dteday-casual-cnt, trip_data_2011_2012_sub)
summary(tree.reg)

train = sample(1:nrow(trip_data_2011_2012_sub),4000)
random.reg <- randomForest(registered~.-instant-dteday-casual-cnt, data = trip_data_2011_2012_sub,
                           subset = train, mtry = 4, importance = TRUE)

random.reg
importance(random.reg)
varImpPlot(random.reg)


yhat.reg <- predict(random.reg, newdata = trip_data_2011_2012_sub[-train, ])
plot(yhat.reg, trip_data_2011_2012_sub[-train, ]$registered)
abline(1,1)

# Get RMSE and RSq
rmse.tree.rand = get_rmse(yhat.reg,trip_data_2011_2012_sub[-train, ]$registered)
rsq.tree.rand = get_rsq(yhat.reg,trip_data_2011_2012_sub[-train, ]$registered)
plot(yhat.reg, residuals)





# Random forest with diff max trees


set.seed(99)
trip_data_2011_2012_sub_test <- trip_data_2011_2012_sub[-train,]
n = nrow(trip_data_2011_2012_sub_test)
ntreev = c(10,500,1000,5000)
nset = length(ntreev)
fmat = matrix(0,n,nset)

for(i in 1:nset) {
  cat('doing Boston rf: ',i,'\n')
  rffit = randomForest(registered~.,data=trip_data_2011_2012_sub[train,],ntree=ntreev[i], maxnodes=20)
  fmat[,i] = predict(rffit, newdata=trip_data_2011_2012_sub_test)
}


par(mfrow=c(1,1))
plot(rffit)

# Plotting fits
par(mfrow=c(1,4))
plot(trip_data_2011_2012_sub[-train,]$registered, fmat[,1])
plot(trip_data_2011_2012_sub[-train,]$registered, fmat[,2])
plot(trip_data_2011_2012_sub[-train,]$registered, fmat[,3])
plot(trip_data_2011_2012_sub[-train,]$registered, fmat[,4])


mse.reg.random = c(1,2,3,4)
rsq.reg.random = c(1,2,3,4)
for (i in 1:4) {
  #mse.reg.random[i] = sqrt(mean((fmat[,i]-trip_data_2011_2012_sub[-train,]$registered)^2)/(1000-1))
  mse.reg.random[i] = get_rmse(fmat[,i],trip_data_2011_2012_sub[-train,]$registered)
  rsq.reg.random[i] = get_rsq(fmat[,i],trip_data_2011_2012_sub[-train,]$registered)
}




# We can see ideal no. of trees is around 1000

rffit_ideal = randomForest(registered~.,data=trip_data_2011_2012_sub[train,],ntree=1000, maxnodes=20)

rffit_ideal_pred <- predict(rffit_ideal, newdata=trip_data_2011_2012_sub[-train,])

rmse.rand.ideal = get_rmse(rffit_ideal_pred,trip_data_2011_2012_sub[-train,]$registered)
rsq.ran.ideal = get_rsq(rffit_ideal_pred,trip_data_2011_2012_sub[-train,]$registered)


# Let's use boosting










# Let's recalculate best no. of neighbours (optimal k)
#### Initializing variables ########
tr = sample(1:n,5000)




newdata_tr = reg[tr]
n = dim(newdata_tr)[1]

newdata_test = reg[-tr]


kcv = 10   # No. of folds
n0 = round(n/kcv,0) #no. of observations per group

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n
##################################

# Run K-Fold validation K times for each set
for(j in 1:kcv){
  
  # Basically, if no of observations per group is more than total observations, just set
  # no. of obs per group to entire set
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  # Take sets of 'val' no. of values from train and put it in test
  train_i = newdata_tr[-val,]
  print(train_i)
  test_i = newdata_tr[val,]
  print(test_i)
  
  for(i in 1:100){
    
    # for 'j' no. of folds (between 1 and kcv) calculate 100 MSE values
    # for 1 to 100 nearest neighbours and store in out_MSE matrix
    near = kknn(registered~.,train_i,test_i,k=i,kernel = "rectangular")
    print((test_i$registered-near$fitted)^2)
    aux = mean((test_i$registered-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}

# Calculate mean MSE for all values in columns in out_MSE (i.e calc. mean MSE for 
# 1 to 100 nearest neighbours calculated for K fold cross validation)
mMSE = apply(out_MSE,2,mean)

# Plot KCV RMSE result for 1 to 100 nearest neighbours to determine optimal no. of neighbours
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="Registered Users per hour (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")



tst = sample(1:dim(newdata_test)[1],8000)
# Calculate KNN on all predictors i.e Lat, Long and Population
near = kknn(registered~.,newdata_test[tst],newdata_test[-tst],k=2,kernel = "rectangular")

# Calc. residuals
res = newdata_test[-tst]$registered - near$fitted

#Let's store value to compare later
res_Pop = res
RMSE_Pop = sqrt(mean(res^2))
rsq_reg = get_rsq(near$fitted, newdata_test[-tst]$registered)



















#### Initializing variables ########
tr = sample(1:n,5000)



newdata_tr_cas = cas[tr]
n = dim(newdata_tr_cas)[1]

newdata_test_cas = cas[-tr]


kcv = 10   # No. of folds
n0 = round(n/kcv,0) #no. of observations per group

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n
##################################

# Run K-Fold validation K times for each set
for(j in 1:kcv){
  
  # Basically, if no of observations per group is more than total observations, just set
  # no. of obs per group to entire set
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  # Take sets of 'val' no. of values from train and put it in test
  train_i = newdata_tr_cas[-val,]
  print(train_i)
  test_i = newdata_tr_cas[val,]
  print(test_i)
  
  for(i in 1:100){
    
    # for 'j' no. of folds (between 1 and kcv) calculate 100 MSE values
    # for 1 to 100 nearest neighbours and store in out_MSE matrix
    near = kknn(casual~.,train_i,test_i,k=i,kernel = "rectangular")
    print((test_i$casual-near$fitted)^2)
    aux = mean((test_i$casual-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}

# Calculate mean MSE for all values in columns in out_MSE (i.e calc. mean MSE for 
# 1 to 100 nearest neighbours calculated for K fold cross validation)
mMSE = apply(out_MSE,2,mean)

# Plot KCV RMSE result for 1 to 100 nearest neighbours to determine optimal no. of neighbours
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="Casual Users per hour (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")



tst = sample(1:dim(newdata_test)[1],8000)
# Calculate KNN on all predictors i.e Lat, Long and Population
near = kknn(casual~.,newdata_test_cas[tst],newdata_test_cas[-tst],k=best,kernel = "rectangular")

# Calc. residuals
res = newdata_test_cas[-tst]$casual - near$fitted

#Let's store value to compare later
res_Pop = res
RMSE_Pop = sqrt(mean(res^2))

rsq_cas = get_rsq(near$fitted, newdata_test_cas[-tst]$casual)








