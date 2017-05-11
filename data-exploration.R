source("/Users/naelin/Projects/YelpDataProject/yelp-data-exploration/process-data.R")
library("rjson")

## Preprocess Yelp user data
user_json_file = "/Users/naelin/Downloads/yelp_dataset_challenge_round9/yelp_academic_dataset_user.json"
user_lines = readLines(user_json_file, n=50000)
user_dat = lapply(user_lines, fromJSON)
user_dat = do.call("rbind", user_dat)
user_dat = data.frame(user_dat)
user_dat$yelping_since = unlist(user_dat$yelping_since)
user_dat$yelping_since = unlist(as.POSIXlt(user_dat$yelping_since, tz="US/Pacific", "%Y-%m-%d"))
user_dat$user_id = unlist(user_dat$user_id)
user_dat$name = unlist(user_dat$name)
user_dat$review_count = unlist(user_dat$review_count)
user_dat$useful = unlist(user_dat$useful)
user_dat$funny = unlist(user_dat$funny)
user_dat$cool = unlist(user_dat$cool)
user_dat$fans = unlist(user_dat$fans)
user_dat$average_stars = unlist(user_dat$average_stars)
user_dat$compliment_hot = unlist(user_dat$compliment_hot)
user_dat$compliment_more = unlist(user_dat$compliment_more)
user_dat$compliment_profile = unlist(user_dat$compliment_profile)
user_dat$compliment_cute = unlist(user_dat$compliment_cute)
user_dat$compliment_list = unlist(user_dat$compliment_list)
user_dat$compliment_note = unlist(user_dat$compliment_note)
user_dat$compliment_photos = unlist(user_dat$compliment_photos)
user_dat$compliment_plain = unlist(user_dat$compliment_plain)
user_dat$compliment_cool = unlist(user_dat$compliment_cool)
user_dat$compliment_funny = unlist(user_dat$compliment_funny)
user_dat$compliment_writer = unlist(user_dat$compliment_writer)

## Data exploration
feat = c("funny", "useful", "cool", "fans", "average_stars")
plot(user_dat[,])
cor(user_dat[,feat])

plot(average_stars ~ review_count + review_count, data=user_dat, pch=20, col="red", main="Star rating by review count")

one = user_dat[round(user_dat$average_stars)==1,]
two = user_dat[round(user_dat$average_stars)==2,]
three = user_dat[round(user_dat$average_stars)==3,]
four = user_dat[round(user_dat$average_stars)==4,]
five = user_dat[round(user_dat$average_stars)==5,]

points(two$review_count, two$average_stars, pch=16, col="orange")
points(three$review_count, three$average_stars, pch=16, col="yellow")
points(four$review_count, four$average_stars, pch=16, col="blue")
points(five$review_count, five$average_stars, pch=16, col="purple")

legend("bottomright", c("1-star", "2-star", "3-star", "4-star", "5-star"), inset=0.05, pch=16, col=c("red", "orange", "yellow", "blue", "purple"))

source("/Users/naelin/Downloads/lin-regr-util.R")
set.seed(123)
splits = split_data(user_dat, frac=c(3,1))
tr_dat = splits[[1]]
te_dat = splits[[2]]
library(e1071)

#fit = naiveBayes(average_stars ~ review_count + useful + funny, data=tr_dat)

library(rpart)
library(rpart.plot)
fit = rpart(useful ~, data=tr_dat)
# fit = lm(stars ~ city + review_count, data=tr_dat)

summary(fit)

prp(fit, extra=1, varlen=-10,
    main="Regression tree for Yelp users",
    box.col="tan")

plot(user_dat$compliment_cool, user_dat$average_stars, xlab="Fans", ylab="Average Stars", main="Average star rating by number of fans")
#abline(fit, lty=2)
predicts = predict(fit, te_dat)
#conf_mtx = table(round(predicts), round(te_dat$average_stars))
#conf_mtx
#succ_rate = mean(predicts == te_dat$average_stars)
#paste0("The success rate is about ", round(succ_rate, 2) * 100, "%")