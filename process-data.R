library(rjson)

# pre-process business data
business_data = function(business_json_file) {
  business_dat = stream_in(file(business_json_file), pagesize=10000)
  return (business_dat)
}

# pre-process checkin data
checkin_data = function(checkin_json_file) {
  checkin_dat = stream_in(file(checkin_json_file), pagesize=10000)
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
  
  return (checkin_dat)
}

# pre-process review data
review_data = function(review_json_file) {
  review_dat = stream_in(file(review_json_file), pagesize=10000)
  review_dat$date = as.POSIXlt(review_dat$date, tz="US/Pacific", "%Y-%m-%d")

  return (review_dat)
}

# tip data
tip_data = function(tip_json_file) {
  tip_dat = stream_in(file(tip_json_file), pagesize=10000)
  tip_dat$date = as.POSIXlt(tip_dat$date, tz="US/Pacific", "%Y-%m-%d")
  
  return (tip_dat)
}

# pre-process user data
user_data = function(user_json_file) {
  user_dat = stream_in(file(user_json_file), pagesize=10000)
  user_dat$yelping_since = as.POSIXlt(user_dat$yelping_since, tz="US/Pacific", "%Y-%m-%d")
  return (user_dat)
}


