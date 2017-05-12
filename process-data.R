library(jsonlite)

# pre-process business data
business_data = function(business_json_file) {
  business_dat = stream_in(file(business_json_file), pagesize=100000)
  return (business_dat)
}

# pre-process checkin data
checkin_data = function(checkin_json_file) {
  checkin_dat = stream_in(file(checkin_json_file), pagesize=100000)
  temp_times = list()
  for (i in 1:length(checkin_dat$time)) {
    temp = unlist(checkin_dat$time[i])
    mon = grep("Mon", temp)
    mon_hours = temp[mon]
    mon_hours = sub("Mon-", "", mon_hours)
    mon_hours = as.numeric(gsub("([0-9]+).*$", "\\1", mon_hours))
    
    tue = grep("Tue", temp)
    tue_hours = temp[tue]
    tue_hours = sub("Tue-", "", tue_hours)
    tue_hours = as.numeric(gsub("([0-9]+).*$", "\\1", tue_hours))
    
    wed = grep("Wed", temp)
    wed_hours = temp[wed]
    wed_hours = sub("Wed-", "", wed_hours)
    wed_hours = as.numeric(gsub("([0-9]+).*$", "\\1", wed_hours))
    
    thu = grep("Thu", temp)
    thu_hours = temp[thu]
    thu_hours = sub("Thu-", "", thu_hours)
    thu_hours = as.numeric(gsub("([0-9]+).*$", "\\1", thu_hours))
    
    fri = grep("Fri", temp)
    fri_hours = temp[fri]
    fri_hours = sub("Fri-", "", fri_hours)
    fri_hours = as.numeric(gsub("([0-9]+).*$", "\\1", fri_hours))
    
    sat = grep("Sat", temp)
    sat_hours = temp[sat]
    sat_hours = sub("Sat-", "", sat_hours)
    sat_hours = as.numeric(gsub("([0-9]+).*$", "\\1", sat_hours))
    
    sun = grep("Sun", temp)
    sun_hours = temp[sun]
    sun_hours = sub("Sun-", "", sun_hours)
    sun_hours = as.numeric(gsub("([0-9]+).*$", "\\1", sun_hours))
    
    temp_times[[length(temp_times) + 1]] = list(mon=mon_hours, tue=tue_hours, wed=wed_hours, 
                      thu=thu_hours, fri=fri_hours, sat=sat_hours, sun=sun_hours)
  }
  checkin_dat$time = temp_times
  return (checkin_dat)
}

# pre-process review data
review_data = function(review_json_file) {
  review_dat = stream_in(file(review_json_file), pagesize=1000000)
  review_dat$date = as.POSIXlt(review_dat$date, tz="US/Pacific", "%Y-%m-%d")

  return (review_dat)
}

# tip data
tip_data = function(tip_json_file) {
  tip_dat = stream_in(file(tip_json_file), pagesize=100000)
  tip_dat$date = as.POSIXlt(tip_dat$date, tz="US/Pacific", "%Y-%m-%d")
  
  return (tip_dat)
}

# pre-process user data
user_data = function(user_json_file) {
  user_dat = stream_in(file(user_json_file), pagesize=50000)
  user_dat$yelping_since = as.POSIXlt(user_dat$yelping_since, tz="US/Pacific", "%Y-%m-%d")
  user_dat$yelping_since = as.Date(user_dat$yelping_since)
  user_dat$elite = unlist(lapply(user_dat$elite, length))
  user_dat$elite = ifelse(user_dat$elite == 1, 0, user_dat$elite)
  user_dat = user_dat[sample(nrow(user_dat), 30000), ]
  user_dat$above_four = ifelse(round(user_dat$average_stars) > 4, 1, 0)
  return (user_dat)
}


