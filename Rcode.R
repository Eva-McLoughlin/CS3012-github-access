#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)


oauth_endpoints("github")

# Change based on application
myapp <- oauth_app(appname = "API_request_assignment",
                   key = "8effa90261308b04b02a",
                   secret = "3471fe07abad672dcdee77d293a38aa353962a25")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# lines 8-20 sourced from: https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]

#-------------------------------------------------------------------------------------------------

# Extract data from my profile:

data <- fromJSON("https://api.github.com/users/Eva-McLoughlin")
data$followers
data$public_repos #number of public repositories i have


#Information on repos
repositories <- fromJSON("https://api.github.com/users/Eva-McLoughlin/repos")
repositories$name #names of all public repositories
repositories$created_at # when various repos were created
ass1repos <- fromJSON("https://api.github.com/repos/Eva-McLoughlin/software-engineering/commits")
ass1repos$commit$message #display the messages attached to each commit for this repos

# information on followers 
followers <- fromJSON("https://api.github.com/users/Eva-McLoughlin/followers")
followers$login #usernames of all followers
length <- length(followers$login) #number of people who follow me
length

# view this data as JSON
dataJSon = toJSON(data, pretty = TRUE)
dataJSon

# Seeing as I have no followers, my data is not very interesting.
# Want to interrogtate a user with more interesting data -> turanct (selected randomly from github)

dataT <- fromJSON("https://api.github.com/users/turanct")
dataT$followers
dataT$public_repos

#---------------------------------------------------------------------------------------------------

# PROCESSING THE DATA FROM GITHUB

#Gather information about the number of followers cassidke's followers have (again, randomly selected a profile from github)

followersNames <- fromJSON("https://api.github.com/users/cassidke/followers")
followersNames$login #shown previously, gets the user names of my followers

a <- "https://api.github.com/users/"
b <- followersNames$login[4]
b
c <- "/followers"

test <- sprintf("%s%s%s", a,b,c) #this method amalgamates a, b and c into one string 
test                              #called test 

#Now have access to stephenoquigley's followers as he is the user at login[4]

# Run this in a loop to access all of cassidke's followers:

numberOfFollowers <- c() 
namesOfFollowers <- c()
for (i in 1:length(followersNames$login)) {
  followers <- followersNames$login[i] #loops through each of her followers, using i as the index
  jsonLink <- sprintf("%s%s%s", a, followers, c) #creates link for the ith follower
  followData <- fromJSON(jsonLink) #stores the followers of my ith follower
  numberOfFollowers[i] = length(followData$login) #number of followers the ith follower has
  namesOfFollowers[i] = followers #names of all users following the ith follower
  
}
numberOfFollowers
namesOfFollowers
finalData <- data.frame(numberOfFollowers, namesOfFollowers) #stores two vectors as one
#data frame
finalData$namesOfFollowers
finalData$numberOfFollowers

