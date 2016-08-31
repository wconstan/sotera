stopifnot(require(ROAuth))

# Define Twitter App URLs
# App-only authentication	https://api.twitter.com/oauth2/token
request_url <- 'https://api.twitter.com/oauth/request_token'
authorize_url <- 'https://api.twitter.com/oauth/authorize'
access_url <- 'https://api.twitter.com/oauth/access_token'

# Define keys
consumer_key <- '591cC2qtKdNJi0hpv2FoEhaTA'
consumer_secret_key <- 'N9n08szDBwC9RoVodc1d9SQbhGzy6n01yQ68Gb6dF8QllQ9wq0'
# user <- 'sotto_voce_vita'
# user_id	<- 770382304636182528

# Establish authorization
credentials <- OAuthFactory$new(consumerKey = consumer_key,
                                consumerSecret = consumer_secret_key, 
                                requestURL = request_url, 
                                accessURL = access_url, 
                                authURL = authorize_url,
                                needsVerifier = TRUE)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
credentials$handshake(cainfo="cacert.pem")

oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"), browseUrl = FALSE)
save(credentials, file='credentials.RData')

pin <- 5920961
save(oauth, file = "oauth.Rdata")


#function to actually scrape Twitter
filterStream(file.name="tweets.json", track="twitter", tweets=1000, oauth=credentials, timeout=10, lang='en')
tweets.df <- parseTweets("tweets.json", simplify = TRUE)

load("my_oauth.Rdata")
filterStream("tweets.json", track = c("Clinton", "Trump"), timeout = 120, oauth = credentials)
stopifnot(require(data.table))
tweets <- data.table(parseTweets("tweets.json", simplify = TRUE))

