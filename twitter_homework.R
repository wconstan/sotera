#####################################################################################
# Install and load required packages

# Define local library location and CRAN respository
lib <- .Library
repos <- 'https://cran.rstudio.com'

# Define list of required packages and install them if missing
required_packages <- c("streamR", "RCurl", "ROAuth", "RJSONIO",'ggplot2','maps','grid',
                       'data.table','httr','twitteR','wordcloud','tm','geosphere','randomForest','stats')
installed_packages <- .packages(all.available = TRUE, lib.loc = lib)
missing_packages <- setdiff(required_packages, installed_packages)
if (length(missing_packages) > 0L) {
  install.packages(pkgs = missing_packages, lib = lib, repos = repos)
}

# Load the required packages
invisible(sapply(required_packages, function(pkg) stopifnot(require(pkg, character.only = TRUE))))

#####################################################################################
# Tap into Twitter Streaming API to obtain high-volume users with geotagged Tweets

# Define local functions
get_bounding_box <- function(map_data) {
  lat_long_range <- map_data$range
  longitudes <- lat_long_range[1:2]
  latitudes <- lat_long_range[3:4]
  sw_corner <- c(min(longitudes), min(latitudes))
  ne_corner <- c(max(longitudes), max(latitudes))
  return(c(sw_corner, ne_corner))
}

get_bounds <- function(database = 'county', state = 'washington', regions = NULL, plot = FALSE) {
  regions <- paste(state, regions, sep = ',')
  return(get_bounding_box(map(database = database, regions = regions, plot = plot)))
}

get_geo_tagged_data <- function(data) {
  stopifnot(is.data.table(data))
  stopifnot(all(c('latitude','longitude') %in% names(data)))
  data <- data[!is.na(latitude) & !is.na(longitude)]
  data[, latitude := as.numeric(latitude)]
  data[, longitude := as.numeric(longitude)]
  return(data)
}

# Load credentials (previously saved)
if (file.exists('credentials.RData')) {
  load('credentials.RData')
} else {
  stop('Run authorization scipt')
}

# Get lat-lon bounding box of various counties in Washington state
state <- 'washington'
regions <- c('Snohomish','King','Pierce','Kitsap','Thurston','Mason')
locations <- get_bounds(state = state, regions = regions)

# Get tweets within bounding box for five minutes via stream filtering
tweets <- data.table(parseTweets(filterStream("", locations = locations, timeout = 300, oauth = credentials), verbose = FALSE))
stream_sample <- tweets[1:10, list(text = paste(substr(text, 1, 40), '...'), screen_name, statuses_count, lat, lon)]
save(stream_sample, file='stream_sample.RData')

# Obtain only the Tweets that have geotagged tweets and 
# filter out descriptions that appear to be job related
tweets_geo_tagged <- tweets[!is.na(lat) & !is.na(lon), ]
filtered_tweets <- tweets_geo_tagged[!grepl('geo-targeted|jobs|careers', description, ignore.case = TRUE)]

# order by the status_count
filtered_tweets <- filtered_tweets[order(-statuses_count)]
sample_filtered_tweets <- filtered_tweets[1:10, list(text = paste(substr(text, 1, 40), '...'), screen_name, statuses_count, lat, lon)]
save(sample_filtered_tweets, file='sample_filtered_tweets.RData')

#####################################################################################
# Obtain Tweet history for targeted high-volume user

# NOTE: We are using the twitteR package here, which uses a slightly different way
# of establishing OAUTH security. So, we must get our credentials working for this effort.

# Load security keys as desginated by my faux application
load('oauth_keys.RData') # previously stored
setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret, 
                    access_token = access_token, access_secret = access_secret)


# Identify the target user
high_volume_user <- 'ArianeAstraea'

# Use the twitteR::userTimeline function to obtain the Tweet history.
# We are limited to 3200 Tweets. Convert the output to a data.table object.
tweet_history_user = userTimeline(high_volume_user, n = 3200)
tweet_history_user <- data.table(twListToDF(tweet_history_user))
save(tweet_history_user, file = 'tweet_history.RData')

# Form initial training data by filtering the history for geotagged Tweets
# NOTE: We will be doing a lot of data.table manipulation, so let's save the
# variable to 'DT', which is short and sweet.
if (!exists('tweet_history_user')) load(file = 'tweet_history.RData')
DT <- get_geo_tagged_data(tweet_history_user)

#####################################################################################
# Add travel distance as a feauture
#
# People may limit their travel based on the distance, e.g., why go 50 miles
# for ice cream when you go a few miles from home to get it? Our user, however,
# traveled to the Philippines this last year and so we need to define the
# distances he traveled relative to some center reference point, which we define
# based on a simple kmeans clustering.

# Display the lat-lon clusters
save(DT, file = 'training_pre_cluster.RData')
DT[, plot(latitude, longitude, pch = 19)] # obviously 2 clusters

# Perform K-Means clustering to obtain cluster centers for distance calculations
cluster <- kmeans(DT[, list(latitude, longitude)], centers = 2)
regions <- c('PACNW', 'PHILIPPINES')
if (cluster$centers[1L,1L] < 20) regions <- rev(regions) 
centers <- data.table(center_latitude = cluster$centers[,1L], 
                      center_longitude = cluster$centers[,2L], 
                      cluster_color = c('red','blue'),
                      region = regions,
                      key = 'region')
print(centers)
DT[, region := ifelse(cluster$cluster == 1L, regions[1L], regions[2L])]

# Merge the center data with the training data
# Note: I likely would not do this in practice as it adds a lot of repetitive data.
# However, the data set is small and it is convenient for subsequent distance calculations
# and plotting
setkey(DT, region)
DT <- DT[centers]
save(DT, cluster, centers, regions, file = 'clusters.RData')

# Define fancy plot function to better visualize clusters
plot_cluster <- function(data) {
  stopifnot(is.data.table(data))
  stopifnot(data[, length(unique(region))] == 1L)
  
  color <- data[, unique(cluster_color)]
  region <- data[, unique(region)]
  centers <- data[, list(latitude = unique(center_latitude), longitude = unique(center_longitude))]
  
  if (region == 'PHILIPPINES'){
    regional_map <- map_data('world', regions = 'Philippines')
    capital <- data.frame(latitude = 14.5995, longitude = 120.9842, name = 'Manila, Philippines')
  } else {
    regional_map <- map_data('county', region = c('washington'))
    capital <- data.frame(latitude = 47.0379, longitude = -122.9007, name = 'Olympia, WA')
  }
  
  g <- ggplot(regional_map) + 
    geom_map(aes(map_id = region), map = regional_map, fill = "white", color = "grey20", size = 0.25) +
    expand_limits(x = regional_map$long, y = regional_map$lat) +
    geom_point(data = data, aes(x = longitude, y = latitude), size = 3, alpha = 2/5, color = color) +
    geom_point(data = centers, 
               aes(x = longitude, y = latitude), size = 2, stroke = 2, alpha = 1, color = 'black', fill = 'white', shape = 21) +
    geom_point(data = capital, aes(x = longitude, y = latitude), size = 3, alpha = 1, 
               shape = 24, color = 'black', fill = 'black') +
    geom_point(data = capital, aes(x = longitude, y = latitude), size = 3, alpha = 1, 
               shape = 25, color = 'black', fill = 'black') +
    geom_text(data = capital, aes(x = longitude, y = latitude, label = name, hjust = -0.1)) +
    xlab('longitude') + ylab('latitude')
  
  print(g)
  
  return(invisible(g))
}

# fancy plots of tweets about cluster centers
plot_cluster(DT[region == 'PACNW'])
plot_cluster(DT[region == 'PHILIPPINES'])

# Create distance from clsuter centers
vincenty_distance <- function(p1, p2, r = 3963.190592) {
  # Vincenty elipsoid great circle distance between two points in miles
  miles_per_meter <- 0.000621371
  distVincentyEllipsoid(p1, p2) * miles_per_meter
}

DT[, dist_from_center := vincenty_distance(cbind(center_longitude, center_latitude), 
                                           cbind(longitude, latitude)), 
   by = region]
ix <- 10:13
sample_distance <- DT[, list(text = paste(substr(text[ix], 1, 50), '...'), 
                             dist_from_center = dist_from_center[ix], 
                             latitude = latitude[ix], 
                             longitude = longitude[ix]), 
                      by = region]
save(sample_distance, file='sample_distance.RData')

#####################################################################################
# Add various features centered around time

# Convert the Tweet date to a day of the week (Monday, Tuesday, etc.) and merge into training data
DT[, day := factor(weekdays(created))]
DT <- DT[order(created)]
day_rank <- DT[, .N, by = day][order(N, decreasing = TRUE)] # what day is the user tweeting the most?
day_rank[, day_rank := seq.int(.N)]
DT <- merge(DT, day_rank, by = 'day')

# Categorize what time of day the Tweet occurred and add to training data
daytime_category <-function(date) {
  
  cut(as.numeric(format(date, "%H")),
      breaks = c(00, 04, 08, 12, 16, 20, 24),
      label = c("late night", "early morning",
                "morning", "afternoon",
                "evening", "early night"),
      include.lowest = TRUE)
}

DT[, day_time := factor(daytime_category(created))]

# Add season feature
date_to_season <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

DT[, season := factor(date_to_season(created))]

# Create sample for demonstration
ix <- 9:11
sample_time <- DT[, list(text = paste(substr(text[ix], 1, 40), '...'), 
                         day = day[ix], 
                         day_time = day_time[ix], 
                         season = season[ix]), 
                  by = region]
save(sample_time, file='sample_time.RData')


#####################################################################################
# Add weather data, since that likely affects when and where people travel
# NOTE: data obtained by rquest from http://www.ncdc.noaa.gov/cdo-web
# and stored in a local file weather.csv
weather <- data.table(read.csv('weather.csv'))
weather <- weather[grepl('Seattle|Manila', STATION_NAME, ignore.case = TRUE) & TAVG > 0]
weather[, region := ifelse(grepl('Seattle', STATION_NAME, ignore.case = TRUE), 'PACNW', 'PHILIPPINES')]
weather <- weather[, list(region, DATE, PRCP, TAVG, TMAX, TMIN)]
DT[, DATE := as.integer(format(created, '%Y%m%d'))]
DT <- merge(DT, weather, by = c('region', 'DATE'), all.x = TRUE)

# fix missing values
DT[PRCP < 0, PRCP := 0.0]

# Form sample for demonstration
sample_weather <- DT[PRCP > 0, list(region,
                                    text = paste(substr(text, 1, 40), '...'), 
                                    DATE, 
                                    PRCP,
                                    TAVG)][c(1:3, 40:46)]
save(sample_weather, file='sample_weather.RData')

#####################################################################################
# Parse Tweet text for location information

# Tweets seem to have a lot of location identifiers like:
#   "I'm at Bank Of America in Seattle, WA https://t.co/HhkT9UDBx0"
# The bulk of these have an '@' or an 'at' word followed by a location
# where each word of the location is capiltalized. 
text_location_example <- DT[205:214, text]
save(text_location_example, file = 'text_location_example.RData')

# Define local functions to parse Tweet text
first_letter_is_capitalized <- function(word){
  word_letters <- strsplit(word, '')[[1L]]
  first_letter <- word_letters[1L]
  is_capitalized <- first_letter == toupper(first_letter)
  return(is_capitalized)
}

parse_tweet_location <- function(tweet) {
  
  remove_url <- function(x) gsub("http[^[:space:]]*", "", x)
  
  tweet <- remove_url(tweet)
  tweet <- gsub('^.*\\bat\\b', 'at', tweet)
  tweet <- gsub('[@]', ' at ', tweet)
  tweet <- gsub('(at\\s+at)+','at', tweet)
  tweet <- gsub('[[:punct:]]', '', tweet)
  tweet <- gsub('[[:cntrl:]]', '', tweet)
  tweet <- gsub("[^[:alnum:]///' ]", "", tweet)
  
  pattern <- '\\bat\\b'
  if (!grepl(pattern = pattern, x = tweet)) return('')
  
  # split with 'at'
  after_at <- strsplit(x = tweet, split = pattern)[[1L]][2L]
  
  # split into words
  words <- strsplit(after_at, split = '\\s+')[[1L]]
  words <- words[nzchar(words)]
  
  # find the words with capital letters
  cap_words <- sapply(words, first_letter_is_capitalized)
  
  # Find the range of words that have their first letter capitalized
  first_word <- which(cap_words)[1L]
  if (is.na(first_word)) first_word <- 1L
  last_word <- max(which(!cap_words)[1L] - 1L, 1L)
  which_in <- grep('in', words)
  if (length(which_in) > 0L){
    diff_in <- which_in - last_word
    good <- diff_in[diff_in %between% c(1,5)]
    if (length(good) > 0L){
      last_word <- last_word + good[1L] - 1L
    }
  }
  
  if (is.na(last_word)) last_word <- length(words)
  
  # return the location, lower cased and collapsed
  tolower(paste(words[first_word:last_word], collapse = '_'))
} 

parsed_locations <- data.table(loc = sapply(DT[, text], parse_tweet_location))

# save examples for demonstration
original_tweets <- text_location_example[3:5]
parsed_tweet_locations <- parsed_locations[207:209]
save(original_tweets, parsed_tweet_locations, file = 'parsed_locations.RData')

# Cannot have more than 53 categories or else randomForest dies
# take the most popular
popular_locations <- parsed_locations[,.N, by=loc][N > 2][order(-N)]
save(popular_locations, file='popular_locations.RData')

most_popular <- popular_locations[, loc]
parsed_locations[!loc %in% most_popular, loc := '']
parsed_locations[!nzchar(loc), loc := 'missing']
DT[, parsed_location := factor(parsed_locations$loc)]

#####################################################################################
# Divide regional space by rounding the latitude and longitude to the nearest hundredth
# This will form regional categories that we can use in a classification model
DT[, lat_long := factor(paste(round(latitude, 2L), round(longitude, 2L), sep = ':'))]

# provide sample of training data
sample_training <- rbind(DT[1:3], DT[parsed_location != 'missing' & region == 'PHILIPPINES'][1:3])
training_data_sample <- sample_training[, list(lat_long, day, day_time, season, TAVG, TMIN, TMAX, PRCP, dist_from_center, parsed_location, region)]
save(training_data_sample, file = 'training_data_sample.RData')

#####################################################################################
# Fit the classification model
DT[, region := factor(region)]
form <- formula(lat_long ~ day + day_time + TAVG + TMIN + TMAX + dist_from_center + PRCP + season + region + parsed_location)

# check for missing independent variables
ind_vars <- attr(terms(form), 'term.labels')
rows_with_missing_data <- which(apply(DT[, ind_vars, with = FALSE], MARGIN = 1, function(x) any(is.na(x))))
any_missing <- length(rows_with_missing_data) > 0L
if (any_missing) {
  cat('\nRows with missing data:\n');flush.console()
  missing_data <- DT[rows_with_missing_data, ind_vars, with = FALSE]
  print(missing_data)
  save(missing_data, file = 'missing_data.RData')
}

# Impute missing data to form the full training data set
training_data <- data.table(rfImpute(form, data = DT))

if (any_missing) {
  cat('\nRows that HAD missing data:\n');flush.console()
  missing_data_imputed <- training_data[rows_with_missing_data, ind_vars, with = FALSE]
  print(missing_data_imputed)
  save(missing_data_imputed, file = 'missing_data_imputed.RData')
}

# Build the random forest model
rf_model <- randomForest(form, data=training_data, importance=TRUE, proximity=TRUE)
save(rf_model, file = 'rf_model.RData')

# Plot variable importance
varImpPlot(rf_model, type = 1)

#####################################################################################
# Form predictions

# Define a function to obtain prediction probability matrix: M x C
# for M observations (tweets) and C classes (lat_long regions)
# For each observation, find the maximum probabliity and the associated class.
# Round the probabliities to the nearest hundredth and return as a data.table object
rf_model_predict <- function(rf_model) {
  
  pred_mat <- predict(rf_model, type='prob')
  classes <- data.table(do.call(rbind.data.frame, strsplit(colnames(pred_mat), split = ':')))
  setnames(classes, c('lat','lon'))
  
  best_prob <- function(probs){
    ix <- which.max(probs)
    cbind(classes[ix], 
          data.table(percentage = round(probs[ix] * 100, 2L)))
  }
  
  rbindlist(apply(pred_mat, MARGIN = 1, best_prob))
}

# calculate the best class probability for each tweet
probs <- rf_model_predict(rf_model)

# multiple probabilities will exist for each class as 
# multiple tweets may be assigned to the same lat-long class:
# form the average to reduce to a single probablity per location
mean_probs <- probs[, list(percentage = round(mean(percentage), 2L)), by = c('lat', 'lon')][order(lat, lon)]
save(mean_probs, file = 'mean_probs.RData')

# output to a file
write.csv(mean_probs, file = 'twitter_homework.csv', row.names = FALSE)

