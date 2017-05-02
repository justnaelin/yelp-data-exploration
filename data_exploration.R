library(rjson)
print("hi")
# user data
user_json_file = "~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_user.json"
dat = readLines(user_json_file, n=10000)
user_json_data = lapply(dat, fromJSON)
str(user_json_data[[1]])

# business data
business_json_file = "~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_business.json"
dat = readLines(business_json_file, n=10000)
business_json_data = lapply(dat, fromJSON)
str(business_json_data[[2]])
