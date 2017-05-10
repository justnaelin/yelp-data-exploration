library(rjson)

# pre-process user data
user_json_file = "~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_user.json"
user_lines = readLines(user_json_file, n=25)
user_dat = lapply(user_lines, fromJSON)
user_dat = do.call("rbind", user_dat)
user_dat = data.frame(user_dat)
user_dat$yelping_since = unlist(user_dat$yelping_since)
user_dat$yelping_since = as.POSIXlt(user_dat$yelping_since, tz="US/Pacific", "%Y-%m-%d")
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

# pre-process business data
business_json_file = "~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_business.json"
business_lines = readLines(business_json_file, n=25)
business_dat = lapply(business_lines, fromJSON)
business_dat = do.call("rbind", business_dat)
business_dat = data.frame(business_dat)
business_dat$business_id = unlist(business_dat$business_id)
business_dat$name = unlist(business_dat$name)
business_dat$stars = unlist(business_dat$stars)
business_dat$review_count = unlist(business_dat$review_count)

# pre-process checkin data
checkin_json_file = "~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_checkin.json"
checkin_lines = readLines(checkin_json_file, n=25)
checkin_dat = lapply(checkin_lines, fromJSON)
checkin_dat = do.call("rbind", checkin_dat)
checkin_dat = data.frame(checkin_dat)
checkin_dat$business_id = unlist(checkin_dat$business_id)
temp_times = list()
for (i in 1:length(checkin_dat$time)) {
  temp = unlist(checkin_dat$time[i])
  mon = grep("Mon", temp)
  mon_hours = temp[mon]
  mon_hours = sub("Mon-", "", mon_hours)
  
  tue = grep("Tue", temp)
  tue_hours = temp[tue]
  tue_hours = sub("Tue-", "", tue_hours)
  
  wed = grep("Wed", temp)
  wed_hours = temp[wed]
  wed_hours = sub("Wed-", "", wed_hours)
  
  thu = grep("Thu", temp)
  thu_hours = temp[thu]
  thu_hours = sub("Thu-", "", thu_hours)
  
  fri = grep("Fri", temp)
  fri_hours = temp[fri]
  fri_hours = sub("Fri-", "", fri_hours)
  
  sat = grep("Sat", temp)
  sat_hours = temp[sat]
  sat_hours = sub("Sat-", "", sat_hours)
  
  sun = grep("Sun", temp)
  sun_hours = temp[sun]
  sun_hours = sub("Sun-", "", sun_hours)
  
  temp_times[[length(temp_times) + 1]] = list(mon=mon_hours, tue=tue_hours, wed=wed_hours, 
                    thu=thu_hours, fri=fri_hours, sat=sat_hours, sun=sun_hours)
  
}
checkin_dat$time = temp_times

# pre-process review data
review_json_file = "~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_review.json"
review_lines = readLines(review_json_file, n=25)
review_dat = lapply(review_lines, fromJSON)
review_dat = do.call("rbind", review_dat)
review_dat = data.frame(review_dat)
review_dat$user_id = unlist(review_dat$user_id)
review_dat$business_id = unlist(review_dat$business_id)
review_dat$stars = unlist(review_dat$stars)
review_dat$text = unlist(review_dat$text)
review_dat$useful = unlist(review_dat$useful)
review_dat$funny = unlist(review_dat$funny)
review_dat$cool = unlist(review_dat$cool)
review_dat$date = unlist(review_dat$date)
review_dat$date = as.POSIXlt(review_dat$date, tz="US/Pacific", "%Y-%m-%d")

# tip data
tip_json_file = "~/Documents/spring17/cst463/project/yelp_dataset_challenge_round9/yelp_academic_dataset_tip.json"
tip_lines = readLines(tip_json_file, n=25)
tip_dat = lapply(tip_lines, fromJSON)
tip_dat = do.call("rbind", tip_dat)
tip_dat = data.frame(tip_dat)
tip_dat$text = unlist(tip_dat$text)
tip_dat$likes = unlist(tip_dat$likes)
tip_dat$business_id = unlist(tip_dat$business_id)
tip_dat$user_id = unlist(tip_dat$user_id)
tip_dat$date = unlist(tip_dat$date)
tip_dat$date = as.POSIXlt(tip_dat$date, tz="US/Pacific", "%Y-%m-%d")
