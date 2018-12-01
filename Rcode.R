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

#-----------------------------------------------------------------------------------------------------

# LANGUAGES 

findLanguages <- function(username)
{
  i=1
  x=1
  languageVector=c()
  RepoNameVector=c()
  languageDF = data_frame()
  while(x!=0)
  {
    
    
    
    repositoryDF = GET( paste0("https://api.github.com/users/", username, "/repos?per_page=100&page=", i),myToken)
    repoContent = content(repositoryDF)
    x = length(repoContent) 
    print(x)
    if (x==0)
    {
      break
    }
    for ( j in 1:length(repoContent))
    {
      repoLanguage=repoContent[[j]]$language
      if(is.null(repoLanguage))
      {
        RepoNameVector[j] = repoContent[[j]]$name
        languageVector[j] = ""
      }else
      {
        languageVector[j] =repoContent[[j]]$language
        RepoNameVector[j] = repoContent[[j]]$name
      }
    }
    currentLanguageDF <- data_frame(repo =  RepoNameVector, language = languageVector)
    languageDF <- rbind(languageDF, currentLanguageDF)
    
    i = i+1
    
  }
  
  return (languageDF)
}
#Returns a dataframe with the language used in each of the users repos
getLanguages <- function(username)
{
  
  reposDF <- GET( paste0("https://api.github.com/users/", username, "/repos?per_page=100"),myToken)
  repoContent <- content(reposDF)
  i <- 1
  languageDF <- data_frame()
  numberOfRepos <- length(repoContent)
  for(i in 1:numberOfRepos)
  {
    repoLanguage <- repoContent[[i]]$language
    repoName <- repoContent[[i]]$name
    if(repoLanguage=="")
    {
      currentLanguageDF <- data_frame(repo = repoName, language = "No language specified")
    }else
    {
      currentLanguageDF <- data_frame(repo = repoName, language = repoLanguage)
    }
    i <- i+1
    languageDF <- rbind(languageDF, currentLanguageDF)
  }
  return (languageDF)
}

languages= findLanguages("phadej")
languages
languages$repo
languages$language

l = unique(languages$language)
n = rep(0, length(unique(languages$language)))
df <- cbind(l,n)

for(i in 1:length(languages$repo)){
  
  lang = languages$language[i]
  #print(lang)
  N = df[l ==lang,2]
  df[l == lang,2] = as.numeric(N)+1
}

length(languages$repo)
df[,2] = as.numeric(df[,2])/length(languages$repo)
l = df[,1]
n = as.numeric(df[,2])*100
frame = data.frame(l,n)
frame


#-----------------------------------------------------------------------------------------------------
# VISUALIZATION

#install.packages("devtools")
#install.packages("Rcpp")
library(devtools)
library(Rcpp)
#install_github('ramnathv/rCharts', force= TRUE)
require(rCharts)

plot1 <- nPlot(numberOfFollowers ~ namesOfFollowers, data = finalData, type = "multiBarChart")
# Other types: pieChart
plot1
plot1$save("plot1.html") #this saves a html file of the plot to my documents. 

#Plot 1 shows that kennyc11 and endam1234 have the most followers out of 
#cassidke's followers -> they are the most influential developers following her

plot2 <- nPlot(n ~ l, data=frame, type="pieChart", main = "Languages")
plot2
plot2$save("plot2.html")

# Plot 2 shows the precentage breakdown of languages used in Phadej's repositories.

#-----------------------------------------------------------------------------------------------------

