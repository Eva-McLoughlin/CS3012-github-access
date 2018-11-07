install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)

oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "API_request_assignment",
                   key = "8effa90261308b04b02a",
                   secret = "3471fe07abad672dcdee77d293a38aa353962a25")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
req

# lines 8-20 sourced from: https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

stop_for_status(req)

# Extract content from a request
json1 = content(req)
json1

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]


data <- fromJSON("https://api.github.com/users/Eva-McLoughlin")
data$followers
data$public_repos #number of public repositories i have

data$id
data$url
data$followers_url
data$following_url
data$name
data$company
data$following
data$name
data$name
data$created_at
data$updated_at
data$bio
data$email
data$type
data$site_admin
data$public_gists
data$public_repos
data$hireable
data$login
data$blog
data$location
data$repos_url
