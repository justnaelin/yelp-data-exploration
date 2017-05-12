library(rpart)
library(rpart.plot)
library(maptree)
library(class)
library(FNN)
library(kknn)
library(ElemStatLearn)
source("~/Documents/spring17/cst463/project/yelp-data-exploration/process-data.R")
source("~/Documents/spring17/cst463/code/lin-regr-util.R")


fnames = c('business','checkin','review','tip','user')
jfile = paste0('~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_',fnames,'.json')

user_dat = user_data(jfile[5])
############################################################################################
# DATA EXPLORATION
rv = c("review_count", "fans", "num_friends", "num_times_elite", "useful", "funny", "cool")
plot(user_dat[,rv], pch=20, col="red4")

one = user_dat[round(user_dat$average_stars)==1,]
two = user_dat[round(user_dat$average_stars)==2,]
three = user_dat[round(user_dat$average_stars)==3,]
four = user_dat[round(user_dat$average_stars)==4,]
five = user_dat[round(user_dat$average_stars)==5,]

# scatter plots
plot(num_times_elite ~ fans, data=one, ylim=c(0,12), xlim=c(0,1321), col="blue", pch=20)
points(num_times_elite ~ fans, data=two, col="green4", pch=20)
points(num_times_elite ~ fans, data=three, col="yellow", pch=20)
points(num_times_elite ~ fans, data=four, col="orange2", pch=20)
points(num_times_elite ~ fans, data=five, col="red", pch=20)

# Histograms
hist(one$num_times_elite, col="red4", main="Rounded Average Stars: 1", 
     xlab="Number of Times Elite")
hist(two$num_times_elite, col="red4", main="Rounded Average Stars: 2", 
     xlab="Number of Times Elite")
hist(three$num_times_elite, col="red4", main="Rounded Average Stars: 3", 
     xlab="Number of Times Elite")
hist(four$num_times_elite, col="red4", main="Rounded Average Stars: 4", 
     xlab="Number of Times Elite")
hist(five$num_times_elite, col="red4", main="Rounded Average Stars: 5", 
     xlab="Number of Times Elite")

hist(one$fans, col="red4", main="Rounded Average Stars: 1", 
     xlab="Number of Fans")
hist(two$fans, col="red4", main="Rounded Average Stars: 2", 
     xlab="Number of Fans")
hist(three$fans, col="red4", main="Rounded Average Stars: 3", 
     xlab="Number of Fans")
hist(four$fans, col="red4", main="Rounded Average Stars: 4", 
     xlab="Number of Fans")
hist(five$fans, col="red4", main="Rounded Average Stars: 5", 
     xlab="Number of Fans")

hist(one$review_count, col="red4", main="Rounded Average Stars: 1", 
     xlab="Review Count")
hist(two$review_count, col="red4", main="Rounded Average Stars: 2", 
     xlab="Review Count")
hist(three$review_count, col="red4", main="Rounded Average Stars: 3", 
     xlab="Review Count")
hist(four$review_count, col="red4", main="Rounded Average Stars: 4", 
     xlab="Review Count")
hist(five$review_count, col="red4", main="Rounded Average Stars: 5", 
     xlab="Review Count")

hist(one$num_friends, col="red4", main="Rounded Average Stars: 1", 
     xlab="Number of Friends")
hist(two$num_friends, col="red4", main="Rounded Average Stars: 2", 
     xlab="Number of Friends")
hist(three$num_friends, col="red4", main="Rounded Average Stars: 3", 
     xlab="Number of Friends")
hist(four$num_friends, col="red4", main="Rounded Average Stars: 4", 
     xlab="Number of Friends")
hist(five$num_friends, col="red4", main="Rounded Average Stars: 5", 
     xlab="Number of Friends")

hist(one$funny, col="red4", main="Rounded Average Stars: 1", 
     xlab="Funny Votes")
hist(two$funny, col="red4", main="Rounded Average Stars: 2", 
     xlab="Funny Votes")
hist(three$funny, col="red4", main="Rounded Average Stars: 3", 
     xlab="Funny Votes")
hist(four$funny, col="red4", main="Rounded Average Stars: 4", 
     xlab="Funny Votes")
hist(five$funny, col="red4", main="Rounded Average Stars: 5", 
     xlab="Funny Votes")

hist(one$useful, col="red4", main="Rounded Average Stars: 1", 
     xlab="Useful Votes")
hist(two$useful, col="red4", main="Rounded Average Stars: 2", 
     xlab="Useful Votes")
hist(three$useful, col="red4", main="Rounded Average Stars: 3", 
     xlab="Useful Votes")
hist(four$useful, col="red4", main="Rounded Average Stars: 4", 
     xlab="Useful Votes")
hist(five$useful, col="red4", main="Rounded Average Stars: 5", 
     xlab="Useful Votes")

hist(one$cool, col="red4", main="Rounded Average Stars: 1", 
     xlab="Cool Votes")
hist(two$cool, col="red4", main="Rounded Average Stars: 2", 
     xlab="Cool Votes")
hist(three$cool, col="red4", main="Rounded Average Stars: 3", 
     xlab="Cool Votes")
hist(four$cool, col="red4", main="Rounded Average Stars: 4", 
     xlab="Cool Votes")
hist(five$cool, col="red4", main="Rounded Average Stars: 5", 
     xlab="Cool Votes")


# Density plots
plot(density(one$num_times_elite), col="red4", main="Rounded Average Stars: 1", 
     xlab="Number of Times Elite")
plot(density(two$num_times_elite), col="red4", main="Rounded Average Stars: 2", 
     xlab="Number of Times Elite")
plot(density(three$num_times_elite), col="red4", main="Rounded Average Stars: 3", 
     xlab="Number of Times Elite")
plot(density(four$num_times_elite), col="red4", main="Rounded Average Stars: 4", 
     xlab="Number of Times Elite")
plot(density(five$num_times_elite), col="red4", main="Rounded Average Stars: 5", 
     xlab="Number of Times Elite")

plot(density(one$fans), col="red4", main="Rounded Average Stars: 1", 
     xlab="Number of Fans")
plot(density(two$fans), col="red4", main="Rounded Average Stars: 2", 
     xlab="Number of Fans")
plot(density(three$fans), col="red4", main="Rounded Average Stars: 3", 
     xlab="Number of Fans")
plot(density(four$fans), col="red4", main="Rounded Average Stars: 4", 
     xlab="Number of Fans")
plot(density(five$fans), col="red4", main="Rounded Average Stars: 5", 
     xlab="Number of Fans")

plot(density(one$review_count), col="red4", main="Rounded Average Stars: 1", 
     xlab="Review Count")
plot(density(two$review_count), col="red4", main="Rounded Average Stars: 2", 
     xlab="Review Count")
plot(density(three$review_count), col="red4", main="Rounded Average Stars: 3", 
     xlab="Review Count")
plot(density(four$review_count), col="red4", main="Rounded Average Stars: 4", 
     xlab="Review Count")
plot(density(five$review_count), col="red4", main="Rounded Average Stars: 5", 
     xlab="Review Count")

plot(density(one$num_friends), col="red4", main="Rounded Average Stars: 1", 
     xlab="Number of Friends")
plot(density(two$num_friends), col="red4", main="Rounded Average Stars: 2", 
     xlab="Number of Friends")
plot(density(three$num_friends), col="red4", main="Rounded Average Stars: 3", 
     xlab="Number of Friends")
plot(density(four$num_friends), col="red4", main="Rounded Average Stars: 4", 
     xlab="Number of Friends")
plot(density(five$num_friends), col="red4", main="Rounded Average Stars: 5", 
     xlab="Number of Friends")

plot(density(one$funny), col="red4", main="Rounded Average Stars: 1", 
     xlab="Funny Votes")
plot(density(two$funny), col="red4", main="Rounded Average Stars: 2", 
     xlab="Funny Votes")
plot(density(three$funny), col="red4", main="Rounded Average Stars: 3", 
     xlab="Funny Votes")
plot(density(four$funny), col="red4", main="Rounded Average Stars: 4", 
     xlab="Funny Votes")
plot(density(five$funny), col="red4", main="Rounded Average Stars: 5", 
     xlab="Funny Votes")

plot(density(one$useful), col="red4", main="Rounded Average Stars: 1", 
     xlab="Useful Votes")
plot(density(two$useful), col="red4", main="Rounded Average Stars: 2", 
     xlab="Useful Votes")
plot(density(three$useful), col="red4", main="Rounded Average Stars: 3", 
     xlab="Useful Votes")
plot(density(four$useful), col="red4", main="Rounded Average Stars: 4", 
     xlab="Useful Votes")
plot(density(five$useful), col="red4", main="Rounded Average Stars: 5", 
     xlab="Useful Votes")

plot(density(one$cool), col="red4", main="Rounded Average Stars: 1", 
     xlab="Cool Votes")
plot(density(two$cool), col="red4", main="Rounded Average Stars: 2", 
     xlab="Cool Votes")
plot(density(three$cool), col="red4", main="Rounded Average Stars: 3", 
     xlab="Cool Votes")
plot(density(four$cool), col="red4", main="Rounded Average Stars: 4", 
     xlab="Cool Votes")
plot(density(five$cool), col="red4", main="Rounded Average Stars: 5", 
     xlab="Cool Votes")
############################################################################################



############################################################################################
# LOGISTIC REGRESSION
set.seed(123)
splits = split_data(user_dat, frac=c(3,1))
tr_dat = splits[[1]]
te_dat = splits[[2]]

log_fit = glm(above_three ~ useful + funny + cool + review_count + fans + num_friends + num_times_elite, 
           data=tr_dat, family=binomial)
summary(log_fit)

y = predict(log_fit, newdata=te_dat, type="response")
predicts = as.numeric(y > 0.5)
actuals = te_dat$above_three
conf_mtx = table(actuals, predicts)
conf_mtx
############################################################################################



############################################################################################
# TREE CLASSIFICATION
tree_fit1 = rpart(above_three ~ useful + funny + cool + review_count + fans + num_friends + num_times_elite, 
            data=tr_dat, method="class")
prp(tree_fit1, extra=1, varlen=-10,main="regression tree for yelpers rating", box.col="tan")
summary(tree_fit1)
predicted = predict(tree_fit1, newdata=te_dat, type="class")
actuals = te_dat$above_three
conf_mtx = table(actuals, predicted)
conf_mtx
############################################################################################


############################################################################################
# KNN CLASSIFICATION
knn_user_dat = user_dat[sample(1:nrow(user_dat)),]

pca = prcomp(knn_user_dat[,rv], center=TRUE, scale.=TRUE)
plot(pca, type="l")
summary(pca)

predict(pca, newdata=knn_user_dat[,rv])

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              groups = knn_user_dat$above_four, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


# number of training examples
tr_rows = 1:floor(nrow(knn_user_dat)*.70)
# features to be used
features = c("useful", "funny", "fans", "review_count")
# feature vectors training and testing data
fvs = knn_user_dat[,features]
tr_dat = fvs[tr_rows,]
te_dat = fvs[-tr_rows,]
# labels for training and test data
labels = knn_user_dat[,"above_four"]
tr_labels = labels[tr_rows]
te_labels = labels[-tr_rows]

actuals = te_labels
predicts = knn(tr_dat, te_dat, tr_labels, k=3, prob=TRUE)

table(actuals, predicts)

prob <- attr(predicts, "prob")
prob <- ifelse(predicts==1, prob, 1-prob)
px1 <- knn_user_dat$useful
px2 <- knn_user_dat$review_count
prob11 <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob11, levels=0.5, labels="", xlab="", ylab="", main="11-nearest neighbour")
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()
############################################################################################


############################################################################################
# KNN REGRESSION
average_stars = knn_user_dat[,"average_stars"]
tr_average_stars = average_stars[tr_rows]
te_average_stars = average_stars[-tr_rows]

actuals = te_average_stars
predicts = knn.reg(tr_dat, te_dat, tr_average_stars, k=11)
predicts$pred = round(predicts$pred)

table(actuals, predicts$pred)
############################################################################################


