
# Extract MLB Stats
# Extraction of data from baseball-reference.com
# Version: 0.3
# Last Update: 2016.10.27

# Testing data-mining and data-analysis with R. Extraction of MLB baseball
# statistics from www.baseball-reference.com and later on computing advanced
# stats and metrics.
# based on:
#             blog.yhat.com/posts/replicating-five-thirty-eight-in-r.html

##########################
# Block on auto-run
##########################

# set working directory:
setwd("D:/Users/YCHEN/Desktop/RWorkspace/MLB Sandbox")  # for windows
#setwd("~/Desktop/RWorkspace/MLB Sandbox")               # for macosx

# Set of packages, versions and scripts used:
library(XML)                    # 3.98-1.4
library(plyr)                   # 1.8.4
library(dplyr)                  # 0.5.0
library(stringr)                # 1.1.0
library(tidyverse)              # 1.0.0,uploads:ggplot2,dplyr,tidyr,readr,purrr,tibble
#library(scales)                 # 0.4.0
#library(ggplot2)                # 2.1.0
# may need to install the following package from github as it has a template for plotting graphs:
#install.packages("devtools")
#library("devtools")
#devtools::install_github("hadley/scales", force=TRUE)
#devtools::install_github("hadley/ggplot2", force=TRUE)
#devtools::install_github(c("hadley/ggplot2", "jrnold/ggthemes", force=TRUE))
#install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)               # 3.2.0

#insert lines for curl opts to bypass firewall
#username <- Sys.getenv("USERNAME")
#pass <- scan(paste("D:/Users/",username,"/pass.txt",sep=""),what="character",quiet=TRUE)
#opts <- list(
#              proxy           = "proxy.inf.bndes.net"
#              proxyport       = 8080
#              proxyusername   = paste(Sys.getenv("USERDOMAIN"), "\\",username, sep="")
#              proxypassword   = pass,
#              ssl.veripeer    = FALSE
#)
#set_config(use_proxy(url="proxy.inf.bndes.net",8080,username,pass))
#rm(pass)

##########################
# End of auto-run
##########################

#_________________________

##########################
# Start of script
##########################


################################################################################
# DATA MINING AND WRANGLING                                                    #
################################################################################

# reading a sample data for attendance
url <- "http://www.baseball-reference.com/leagues/MLB/1990-misc.shtml"
data <- readHTMLTable(url, stringsAsFactors = FALSE)
data[[1]] # select the first table

# creating function to fetch attendance data
fetch_attendance <- function(year) {
  url <- paste0("http://www.baseball-reference.com/leagues/MLB/", year, "-misc.shtml")
  data <- readHTMLTable(url, stringsAsFactors = FALSE)
  data <- data[[1]]
  data$year <- year
  data
}

# creating and loading a data base containing all attendance data from 1950 to 2016
attendance <- ldply(1950:2015, fetch_attendance, .progress="text")

# clean up the column names???
names(attendance) <- c("tm", "attendance", "attend_per_game", "batage", "page",
                       "bpf", "ppf", "n_hof", "n_aallstars", "n_a_ta_s", 
                       "est_payroll", "time", "managers", "year")

make_numeric <- function(x) {
  x <- str_replace_all(x, ",", "") # remove all commas
  x <- str_replace_all(x, "[$]", "") # remove all $
  as.numeric(x) # cast as a number
}
# and then use the function...
attendance$attendance <- make_numeric(attendance$attendance)
attendance$attend_per_game <- make_numeric(attendance$attend_per_game)
attendance$est_payroll <- make_numeric(attendance$est_payroll)

#________________________________

# from now on it lacks the function and data from standings

# reading a sample data for attendance
url <- "http://www.baseball-reference.com/leagues/MLB/1990-standings.shtml"
data <- readHTMLTable(url, stringsAsFactors = FALSE)
data[[2]] # select the second table

#______________________________________________________________________________#

# creating function to fetch standings data
fetch_standings <- function(year) {
  url <- paste0("http://www.baseball-reference.com/leagues/MLB/", year, "-standings.shtml")
  data <- readHTMLTable(url, stringsAsFactors = FALSE)
  data <- data[[2]]
  data$year <- year
  data
}

# creating and loading a data base containing all attendance data from:
# 1950 to 1993 and 1995 to 2016 
temp_standings <- ldply(
                   c(1950:1993,1995:2015),
                   fetch_standings,
                   .progress="text"
)

# needed to develop an exception for 1994 
# (because of lockout which prevented all postseason)
# for 1994 we need table [1]
url <- "http://www.baseball-reference.com/leagues/MLB/1994-standings.shtml"
data <- readHTMLTable(url, stringsAsFactors = FALSE)
data <- data[[1]] # select the first table

# need to add an extra column with the year of 1994 into data before the merging
# of datasets into standings matrix....

year1994 <- matrix(1994,29,1)
colnames(year1994) <- " year"
temp<-as.data.frame(year1994)
data <- dplyr::bind_cols(data, temp)

# merge both datas into standings database:
standings <- dplyr::bind_rows(temp_standings, data)

################################################################################
# further we may need to delete lines in which "avg" is listed as a team ("tm")#
################################################################################

# clean up the column names
names(standings) <- c("rk", "tm", "lg", "g", "w", "l", "wins_losses", "r", "ra", 
                      "rdiff", "sos", "srs", "pythwl", "luck", "home", "road", 
                      "exinn", "1run", "vrhp", "vlhp", "vs_teams_above_500", 
                      "vs_teams_under_500", "year","NA1","NA2")
# deleting extra columns
standings$NA1<-NULL
standings$NA2<-NULL

# remember:
# lg is league the team is playing
# sos is the strengh of schedule (aka the # runs their opponents are better than avg)
# srs is simple rating sistem (srs = rdiff + sos)
# pythwl is the pythagorean Win-Loss
# luck is difference between pythwl and actual wl
# ex_inn is the wl in extra innings
# 1_run is wl in one run games
################################################################################

# making the following columns numeric:
standings$w <- make_numeric(standings$w)
standings$wins_losses <- make_numeric(standings$wins_losses)
standings$rk <- make_numeric(standings$rk)
standings$l <- make_numeric(standings$l)
standings$r <- make_numeric(standings$r)
standings$ra <- make_numeric(standings$ra)
standings$rdiff <- make_numeric(standings$rdiff)
standings$sos <- make_numeric(standings$sos)
standings$srs <- make_numeric(standings$srs)
standings$luck <- make_numeric(standings$luck)

# merging attendance and standings data: 
fulldata <- merge(standings, attendance, by=c("tm", "year"))

# plotting to compare wins and attendance
#ggplot(fulldata, aes(x=w, y=attendance)) + geom_point()

# plotting to compare wins and average attendance
#ggplot(fulldata, aes(x=w, y=attend_per_game)) + geom_point()

# trying a basic linear model
#summary(lm(attend_per_game ~ w, data=fulldata))

# exporting database as Rda file
#save(fulldata, file="mlb_standings_and_payroll.Rda")

# exporting database as csv file
write.csv(fulldata, file="mlb_standings_and_payroll.csv")

################################################################################
# DATA ANALYSIS                                                                #
################################################################################

# re-uploading the dataset
df <- read.csv(file="mlb_standings_and_payroll.csv", stringsAsFactors = FALSE)
# sorting variables and limiting to observations from 1985 to present
df <- df[,c("tm", "year", "w", "wins_losses", "est_payroll")]
df <- df[df$year >= 1985,]
head(df)    #sample of data to check for errors

# loading metadata for each team (made by yhat's Greg Lamp)
# recovers and reclassify the teams with their hystorical counterparts
# (franchise is kept albeit the city changes)
team.lookups <- read.csv("./team-lookups.csv")
df <- merge(df, team.lookups, by.x="tm", by.y="historic_team", stringsAsFactors=FALSE)
head(df)

# loading metadata for team colors (made by Greg Lamp, data from http://jim-nielsen.com/teamcolors/)
team.colors <- read.csv("./team-colors.csv", stringsAsFactors=FALSE)
df <- merge(df, team.colors, by.x="modern_team", by.y="tm")
head(df)

ggplot(team.colors, aes(x=team_name, fill=team_color)) +
        geom_bar() +
        scale_fill_identity() +
        coord_flip() + xlab("") + ylab("")      # show team colors

# calculating the mean and standard deviation for the estimated payrolls by year
yearly_payroll <- ddply(df, .(year), function(x) {
  data.frame(
    mean_payroll=mean(x$est_payroll, na.rm=TRUE),
    std_payroll=sd(x$est_payroll, na.rm=TRUE)
  )
})

# adding the new info into the dataset
df <- merge(df, yearly_payroll, by="year")

# calculate number of standard deviations from mean and adding to dataset
df$standardized_payroll <- (df$est_payroll - df$mean_payroll) / df$std_payroll


################################################################################
# DATA ANALYSIS and VIZ                                                        #
################################################################################

# plotting 2015 wins-losses vs standardized payroll, for a visual...
ggplot(subset(df, year==2015), aes(x=standardized_payroll, y=wins_losses)) +
  geom_point() +
  scale_x_continuous(name="Standardized Salary\n(# of standard deviations from yearly mean)",
                     breaks=c(-2, 0, 2), limit=c(-2.5, 3.0), labels=c("-2", "0", "+2")) +
  scale_y_continuous(name="Win/Loss %", breaks=seq(0.3, 0.7, 0.1), limit=c(0.2, 0.8)) +
  annotate("text", x = -2, y = 0.52, label = "Cleveland Indians") +
  annotate("text", x = 2.1, y = 0.59, label = "Los Angeles Dodgers") +
  ggtitle("2015 Win % vs. Standardized Payroll")

# plotting the fitting curve for NYM using a polinomial regression of degree 2:
p <- ggplot(subset(df, tm=="NYM"), aes(x=standardized_payroll, y=wins_losses, color=team_color)) + 
  geom_point(alpha=0.75, size=3) + 
  #   stat_smooth(data=within(df, modern_team <- NULL), color="grey", size=.5,
  #               method="lm", formula = y ~ poly(x, 2), se=FALSE) +
  stat_smooth(size=1.5, method="lm", formula = y ~ poly(x, 2), se=FALSE) +
  scale_color_identity() +
  scale_x_continuous(name="Standardized Salary\n(# of standard deviations from yearly mean)",
                     breaks=c(-2, 0, 2), limit=c(-2.5, 2.5), labels=c("-2", "0", "+2")) +
  scale_y_continuous(name="Win/Loss %", breaks=seq(0.3, 0.7, 0.1), limit=c(0.2, 0.8)) +
  #   theme_fivethirtyeight() +                 # this makes title misalign
  ggtitle("New York Mets\nWin Percentage vs. stgandard deviations from average salary")
p
#ggsave(filename="nym.png", plot=p, width=8.67, height=6.17)

# overlaying with equivalent fitted curve for entire league:
p <- p + stat_smooth(data=df, color="grey", size=.5, method="lm", formula = y ~ poly(x, 2), se=FALSE)
p
#ggsave(filename="nym-with-league.png", plot=p, width=8.67, height=6.17)

############################################
# building subplots for each MLB division  #
############################################
# creating a list of divisions:
divisions <- c("AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West")

# creating loop through list of divisions and
# filtering each plot to plot data from relevant division:
for (div in divisions) {
  df.division <- subset(df, division==div)
  div.title <- sub("AL", "American League", div)
  div.title <- sub("NL", "National League", div.title)
  p <- ggplot(df.division, aes(x=standardized_payroll, y=wins_losses, color=team_color)) + 
    geom_point(alpha=0.75, size=3) + 
    stat_smooth(data=within(df, modern_team <- NULL), color="grey", size=.5,
                method="lm", formula = y ~ poly(x, 2), se=FALSE) +
    stat_smooth(size=1.5, method="lm", formula = y ~ poly(x, 2), se=FALSE) +
    scale_color_identity() +
    scale_x_continuous(name="Standardized Salary\n(# of standard deviations from yearly mean)",
                       breaks=c(-2, 0, 2), limit=c(-2.5, 2.5), labels=c("-2", "0", "+2")) +
    scale_y_continuous(name="Win/Loss %", breaks=seq(0.3, 0.7, 0.1), limit=c(0.25, 0.75)) +
    facet_wrap(~modern_team, ncol=5, scales="free_x") +
    theme_fivethirtyeight() +
    ggtitle(div.title)
  ggsave(filename=paste0(div, ".png"), plot=p, width=13.65, height=3.59)
}

# graphics are saved at working directory
# may try to embed all of them into a PDF file... use of LaTex

##########################
# End of script
##########################