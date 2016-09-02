# Install and Activate Packages
lib <- .Library
repos <- 'https://cran.rstudio.com'

required_packages <- c("streamR", "RCurl", "ROAuth", "RJSONIO",'ggplot2','maps','grid',
                       'data.table','httr','twitteR','wordcloud','tm','geosphere','randomForest')
installed_packages <- .packages(all.available = TRUE, lib.loc = lib)
missing_packages <- setdiff(required_packages, installed_packages)
if (length(missing_packages) > 0L) {
  install.packages(pkgs = missing_packages, lib = lib, repos = repos)
}

invisible(sapply(required_packages, function(pkg) stopifnot(require(pkg, character.only = TRUE))))

get_bounding_box <- function(map_data) {
  lat_long_range <- map_data$range
  longitudes <- lat_long_range[1:2]
  latitudes <- lat_long_range[3:4]
  sw_corner <- c(min(longitudes), min(latitudes))
  ne_corner <- c(max(longitudes), max(latitudes))
  return(c(sw_corner, ne_corner))
}

clean_words <- function(text) {
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  text <- gsub("[^[:alnum:]///' ]", "", text)
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  text <- removeURL(text)
  text <- removeNumPunct(text)
  word_list <- stringr::str_split(text, '\\s+')
  words <- unlist(word_list)
  return(words[nzchar(words)])
}

# load credentials
if (file.exists('credentials.RData')) {
  load('credentials.RData')
} else {
  stop('Run authorization scipt')
}

get_bounds <- function(database = 'county', state = 'washington', regions = NULL) {
  regions <- paste(state, regions, sep = ',')
  return(get_bounding_box(map(database = database, regions = regions, plot = FALSE)))
}

get_geo_tagged_data <- function(data) {
  stopifnot(is.data.table(data))
  stopifnot(all(c('latitude','longitude') %in% names(data)))
  return(data[!is.na(latitude) & !is.na(longitude)])
}

# WA/King and WA/Pierce county bounding box (approx)
locations <- get_bounds(state = 'washington', regions = NULL)

# get tweets within bounding box: stream API filtering via bounding box
tweets <- data.table(parseTweets(filterStream("", locations = locations, timeout = 300, oauth = credentials), verbose = FALSE))

# add got_geo column
tweets[, got_geo := FALSE]
tweets[!is.na(lat) & !is.na(lon), got_geo := TRUE]

# get ids of users that have geo_tags
tweets_geo_tagged <- tweets[!is.na(lat) & !is.na(lon), ]
DT <- tweets_geo_tagged[!grepl('geo-targeted|jobs|careers', description, ignore.case = TRUE)]

users_with_geo_tags <- tweets[!is.na(lat) & !is.na(lon), unique(user_id_str)]

regional_map <- data.table(map_data("county"))[region == 'washington']

points <- tweets[, list(x = as.numeric(lon), y = as.numeric(lat))]
ggplot(regional_map) + 
  geom_map(aes(map_id = region), map = regional_map, fill = "white", 
           color = "grey20", size = 0.25) + expand_limits(x = regional_map$long, y = regional_map$lat) + 
  geom_point(data = points, aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue") + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(), plot.background = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))


# tweet history for a particular user
consumer_key <- '591cC2qtKdNJi0hpv2FoEhaTA'
consumer_secret <- 'N9n08szDBwC9RoVodc1d9SQbhGzy6n01yQ68Gb6dF8QllQ9wq0'
access_token <- '770382304636182528-aHXQBlH4wi439uOv3FFWEbhYN1RCJwq'
access_secret <-'RXKXr9UCyrA6tpzEPq6LJorujMS9Vnp8lzeiuR8regIxB'

# registerTwitterOAuth(credentials)
setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret, access_token = access_token, access_secret = access_secret)

tweet_history_user = userTimeline('ArianeAstraea', n = 3200)
tweet_history_user <- data.table(twListToDF(tweet_history_user))
DT <- get_geo_tagged_data(tweet_history_user)

# Feature extraction
DT[, day := weekdays(created)]
DT <- DT[order(created)]
day_rank <- DT[, .N, by = day][order(N, decreasing = TRUE)] # what day is she tweeting the most?
day_rank[, day_rank := seq.int(.N)]

DT <- merge(DT, day_rank, by = 'day')


daytime_category <-function(date) {
  
  cut(as.numeric(format(date, "%H")),
      breaks = c(00, 04, 08, 12, 16, 20, 24),
      label = c("late night", "early morning",
                "morning", "afternoon",
                "evening", "early night"),
      include.lowest = TRUE)
}

DT[, day_time := daytime_category(created)]

# add temp data
mean_seattle_temps <- as.matrix(read.delim('noaa_seattle_daily_temp_history.txt', 
                                 row.names = 1L,stringsAsFactors = FALSE, 
                                 header = FALSE, 
                                 sep = '', 
                                 col.names = c('Day', month.abb)))

day_month_index <- DT[, cbind(as.integer(format(created, '%d')), as.integer(format(created, '%m')))]
DT[, mean_temp := mean_seattle_temps[day_month_index]]
DT[, plot(created, mean_temp, xlab = 'Tweet Date', ylab = 'Mean Temp (F)')]


# add season
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

DT[, season := date_to_season(created)]

# form lat-long target category
DT[, lat_long := paste(round(as.numeric(latitude), 2L), round(as.numeric(longitude), 2L), sep = ':')]

# search_string <- "#mls"
# num_tweets <- 100
# tweets <- searchTwitter(search_string, n=num_tweets, lang="en")

# wordcloud
words <- DT[, clean_words(text)]
word_table <- table(words)
wordcloud(words = names(word_table), freq = word_table, min.freq = 3L)


# define center to be the median tweet location on lat-long coordinates
center <- DT[, c(median(as.numeric(latitude), na.rm = TRUE), median(as.numeric(longitude), na.rm = TRUE))]

haverstine_distance <- function(p1, p2, r = 3963.190592) {
  # Vicenty elipsoid great circle distance between two points in miles
  miles_per_meter <- 0.000621371
  distVincentyEllipsoid(p1, p2) * miles_per_meter
}

DT[, miles_from_center := haverstine_distance(rev(center), cbind(as.numeric(longitude), as.numeric(latitude)))]
