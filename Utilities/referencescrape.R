# scraping of hockey-reference to get all NHL Games
# 2013-14

rm(list=ls())  # want to clear all the variables
library(RODBC) # load the odbc library 
library(XML)   # load the xml library

# set the working directory
workingdir <- ''  # NEED TO ADD YOUR WORKING DIRECTORY
setwd(workingdir)

teammapping <- read.csv('teamabbr.csv', header=FALSE)

year <- c(2013, 2013, 2013, 2014, 2014, 2014, 2014) # all the years for scraping mapped with month
month = c(10,11,12,1,2,3,4) # month mapped with years
day = 1:31 # full range of dates

index <- 0
  for (m in month) # iterate through all the months
  {
    index <- index + 1  # ensure to iterate through all years
    y <- year[index] # gather the year
    for (d in day) # iterate through the days
    {

      # create the directory based on the data
      dir.create(file.path(workingdir,gsub(" ","",paste(y,m,d))))
      weburl <- "http://www.hockey-reference.com/boxscores/index.cgi?month="
      weburl <- paste(weburl, m, "&day=",d,"&year=", y)
      weburl <- gsub(" ", "", weburl)   # website to scrape the scores
      
      # want to gather the scores on that date
      scores <- readHTMLTable(weburl)
      setwd(file.path(workingdir,gsub(" ","",paste(y,m,d))))
      write.table(scores$stats,file="scores.csv",sep=",",col.names=FALSE)
      
      # gather the home teams based on the scores
      hometeams <- as.data.frame(scores$stats$Home)
      awayteams <- as.data.frame(scores$stats$Visitor)

      # gather all the box score links on the page to be parsed
      doc <- htmlParse(weburl)
      links <- xpathSApply(doc,"//a/@href")
      links <- grep(glob2rx("*boxscores/2*"),links,value=TRUE)
      links <- links[grepl("*boxscores/2*",links)]
      
      if (length(links) > 0) # make sure there are links to parse
      {
         linkindex <- 1
         for (link in links)
         {
           gameurl = paste('http://www.hockey-reference.com/',link)           
           gameurl <- gsub(" ", "", gameurl)        
           print(gameurl)  # create the new URL to gather the table data
           tables <- readHTMLTable(gameurl)
           
           # use the mapping to gather the ABR for the current team
           # and hence be able to shortcut to the table we want
           currentHomeTeam <- toString(teammapping[teammapping[2] == toString(hometeams[linkindex,1]),1])
           currentAwayTeam <- toString(teammapping[teammapping[2] == toString(awayteams[linkindex,1]),1])
           
           # sanity check
           if (currentHomeTeam == "") { print("NO HOME TEAM")}
           if (currentAwayTeam == "") { print("NO AWAY TEAM")}
           
           gamename <- substring(link,12,23) # used for saving data
           ssummary <- as.data.frame(tables$scoring) # gather the scoring summary
           write.table(ssummary,file=gsub(" ","",paste(gamename,"_ssummary.csv")),sep=",",col.names=FALSE)
           
           psummary <- as.data.frame(tables$penalty) # gather the penalty summary
           write.table(psummary,file=gsub(" ","",paste(gamename,"_psummary.csv")),sep=",",col.names=FALSE)
           
           awskate <- as.data.frame(tables[gsub(" ","",paste(currentAwayTeam,"_skaters"))]) # gather the away skater stats
           write.table(awskate,file=gsub(" ","",paste(gamename,"_awskate.csv")),sep=",",col.names=FALSE)
           
           awgoal <- as.data.frame(tables[gsub(" ","",paste(currentAwayTeam,"_goalies"))]) # gather the away goalie stats
           write.table(awgoal,file=gsub(" ","",paste(gamename,"_awgoal.csv")),sep=",",col.names=FALSE)
           
           hmskate <- as.data.frame(tables[gsub(" ","",paste(currentHomeTeam,"_skaters"))]) # gather the home skater stats
           write.table(hmskate,file=gsub(" ","",paste(gamename,"_hmskate.csv")),sep=",",col.names=FALSE)
           
           hmgoal <- as.data.frame(tables[gsub(" ","",paste(currentHomeTeam,"_goalies"))]) # gather the home goalie stats
           write.table(hmgoal,file=gsub(" ","",paste(gamename,"_hmgoal.csv")),sep=",",col.names=FALSE)

           linkindex <- linkindex+1

          }
       }
 
       free(doc)
    
  }

}
