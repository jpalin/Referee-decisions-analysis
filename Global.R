#
#  INITIALISATION
#  --------------
#

#Packages
installPacks <- function(){
  install.packages(c("ggpubr", "readxl", "dplry", "plyr", "Rcpp", "reshape", "ggalt", "forcats", "tigerstats", "tidyverse", "magrittr"))
}
installPacks()

#Libraries
useLibs <- function(){
  library(ggalt)
  library(readxl)
  library(dplyr)
  library(plyr)
  library(Rcpp)
  library(reshape)
  library(sqldf)
  library(skellam)
  library(data.table)
  library(tigerstats)
  library(tidyverse)
  library(magrittr)
  library(ggpubr)
  library(rstatix)
}
useLibs()

#
#  GLOBAL DATA
#  -----------
#

#Read data
events <- read_excel("C:/Jamie/Dissertation/Match official data ANON/Match official data ANON2.xlsx")

#Clean data 
tidyevents <- function(){
  #
  #REMOVE THE DUPLICATE ENTRIES IN THE EVENTS DATASET WHERE REF04 / REF09 WERE INCORRECTLY SHOWN TO BE
  #REFERING THE SAME GAMES AT THE SAME TIME. REF04 WAS NOT REFEREEING NEWCASTLE V SWANSEA OR ARSENAL V WEST HAM 
  #
  events <- events[!(events$fixture == "Newcastle United v Swansea City" & events$official == "Ref04"),]
  events <- events[!(events$fixture == "Arsenal v West Ham United" & events$official == "Ref04"),]
  #
  #CONVERT ALL COLUMN NAMES, REPLACING SPACES WITH UNDERSCORES AND UPPER CASE CHARS TO LOWER CASE
  #
  colnames(events) %<>% str_replace_all("\\s", "_") %<>% tolower()
  assign('events',events,envir=.GlobalEnv)
}
tidyevents()

#Enrich data 
eventsX = data.frame(events)

#Sort eventsX by season, game_date, fixture, period, minute, second
eventsX <- eventsX[order(eventsX$season, eventsX$game_date, eventsX$fixture, eventsX$period_id, eventsX$period_minute, eventsX$period_second),]
#Add event playing time secs
eventsX <- eventsX %>% mutate(playing_time_secs = period_minute*60 + period_second)

#New temporary fixture data frame, used to store first half playing time for each fixture
fixture <- eventsX %>% filter(period_id == 1) %>% group_by(season, fixture) %>% top_n(1, playing_time_secs) %>% ungroup %>% select(season, fixture, period_minute, period_second, playing_time_secs) %>% distinct()
      
#add missing row for Norwich City v Chelsea (2011/2012) as there were no recorded events in the first half!
newRow <- data.frame("Season 2011/2012","Norwich City v Chelsea",45,0,2700)
names(newRow) <- c("season", "fixture", "period_minute", "period_second", "playing_time_secs")  
fixture <- rbind(fixture, newRow) 
      
#add first half playing time to each fixture
first_half_stoppage_time_adjustment = 174 #secs, 2016/2017 av. first half stoppage time was 2 mins 54 secs (2.9 mins)
second_half_stoppage_time_adjustment = 305 #secs, 2016/2017 av. second half stoppage time was 5 mins 5 secs (total stoppage time was 7 mins 59 secs)
      
first_half_time <- function(period_minute, period_second) {
  if (period_minute < 45)
    45*60 + first_half_stoppage_time_adjustment
  else
    period_minute*60 + period_second + first_half_stoppage_time_adjustment
}
      
addFirstHalfTime <- function(f) {
  for(i in 1:length(f$season)){
    f$first_half_time[i] = first_half_time(f$period_minute[i], f$period_second[i])
  }
  
  return(f)
}
fixture <- addFirstHalfTime(fixture)

#Function to add first half playing time to each event
addFirstHalfTimeToEvents <- function(g) {
  for(i in 1:length(g$season))
    g$first_half_time[i] = fixture %>% filter(season == g$season[i] & fixture == g$fixture[i]) %>% select(first_half_time)
  
  return(g)
}

#Add first half playing time to each event
eventsX <- addFirstHalfTimeToEvents(eventsX)

#Convert first_half_time from a list to character vector
class(eventsX$first_half_time)  #before conversion
eventsX$first_half_time <- vapply(eventsX$first_half_time, paste, collapse = ", ", character(1L))
class(eventsX$first_half_time)  #after conversion

#Utility function to convert secs to hms
hms <- function(secs){
  paste(formatC(secs %/% (60*60) %% 24, width = 2, format = "d", flag = "0"),
        formatC(secs %/% 60 %% 60, width = 2, format = "d", flag = "0"),
        formatC(secs %% 60, width = 2, format = "d", flag = "0"),
        sep = ":" )
}

#Utility function to add actual playing time of events in three formats: secs, mins and hms
addUpdatedTimes <- function(g) {
  for(i in 1:length(g$season)) {
    
    if (g$period_id[i] == 1) {
      g$playing_time_updated_secs[i] = g$playing_time_secs[i]
    }
    else {  
      g$playing_time_updated_secs[i] = g$playing_time_secs[i] + as.numeric(g$first_half_time[i]) - 2700
    }
    
    g$mins[i] = g$playing_time_updated_secs[i] / 60
    g$hms[i] = hms(g$playing_time_updated_secs[i])
  }
  
  return(g)
}

#Update all events in eventsx with estimated playing time for each event, adjusting 2nd half event times to include first half stoppage time
eventsX <- addUpdatedTimes(eventsX)

#
#Construct a new dataframe DAsByRefandTeam of disciplinary actions by referee and team
#
S <- eventsX %>% select(season, fixture, official, team) %>% distinct()
S <- S %>% group_by(official, team) %>% dplyr::mutate(games=row_number()) %>% top_n(1, games) %>% ungroup() %>% select(official, team, games)
S <- S %>% dplyr::mutate(team_exposure=ifelse(games < 8, 3, ifelse(games < 15, 2, 1)))
#cards
R <- eventsX %>% filter(event_type == 'Card') %>% select(official, team, card_type)
R <- R %>% group_by(official, team, card_type) %>% dplyr::mutate(cards=row_number()) %>% top_n(1, cards) %>% ungroup()
R <- R %>% dplyr::mutate(R, yellow_tmp = ifelse(card_type == 'Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, second_yellow_tmp = ifelse(card_type == 'Second Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, red_tmp = ifelse(card_type == 'Red', cards, 0))
R <- R %>% group_by(official, team) %>% dplyr::mutate(yellow=max(yellow_tmp)) %>% top_n(1, yellow) %>% ungroup()
R <- R %>% group_by(official, team) %>% dplyr::mutate(second_yellow=max(second_yellow_tmp)) %>% top_n(1, second_yellow) %>% ungroup()
R <- R %>% group_by(official, team) %>% dplyr::mutate(red=max(red_tmp)) %>% top_n(1, red) %>% ungroup()
R <- R %>% select(official,team,yellow,second_yellow,red) %>% distinct()
#
DAsByRefandTeam <- merge(S, R, by=c("official","team"), all.x = TRUE)
DAsByRefandTeam[is.na(DAsByRefandTeam)] = 0
DAsByRefandTeam <- DAsByRefandTeam %>% dplyr::mutate(avg_yellow=round(yellow/games,digits=2))
#fouls
R <- eventsX %>% filter(eventsX$foul_won_lost == "Foul Conceded") %>% select(official, team, event_type)
R <- R %>% group_by(official, team) %>% dplyr::mutate(fouls=row_number()) %>% top_n(1, fouls) %>% ungroup()
R <- R %>% select(official,team,fouls) %>% distinct()
#
foulsByRefandTeam <- merge(S, R, by=c("official","team"), all.x = TRUE)
foulsByRefandTeam[is.na(foulsByRefandTeam)] = 0
foulsByRefandTeam <- foulsByRefandTeam %>% select(official, team, fouls)
#
DAsByRefandTeam <- merge(DAsByRefandTeam, foulsByRefandTeam, by=c("official","team"), all.x = TRUE)

#
#Construct a new dataframe cardsByFixtureByRefandTeam of cards by fixture by referee and team
#
S <- eventsX %>% select(season, fixture, official, team) %>% distinct()
R <- eventsX %>% filter(event_type == 'Card') %>% select(season, fixture, official, team, card_type)
R <- R %>% group_by(season, fixture, official, team, card_type) %>% dplyr::mutate(cards=row_number()) %>% top_n(1, cards) %>% ungroup()
R <- R %>% dplyr::mutate(R, yellow_tmp = ifelse(card_type == 'Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, second_yellow_tmp = ifelse(card_type == 'Second Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, red_tmp = ifelse(card_type == 'Red', cards, 0))
R <- R %>% group_by(season, fixture, official, team) %>% dplyr::mutate(yellow=max(yellow_tmp)) %>% top_n(1, yellow) %>% ungroup()
R <- R %>% group_by(season, fixture, official, team) %>% dplyr::mutate(second_yellow=max(second_yellow_tmp)) %>% top_n(1, second_yellow) %>% ungroup()
R <- R %>% group_by(season, fixture, official, team) %>% dplyr::mutate(red=max(red_tmp)) %>% top_n(1, red) %>% ungroup()
R <- R %>% select(season,fixture,official,team,yellow,second_yellow,red) %>% distinct()
#
cardsByFixtureByRefandTeam <- merge(S, R, by=c("season","fixture","official","team"), all.x = TRUE)
cardsByFixtureByRefandTeam[is.na(cardsByFixtureByRefandTeam)] = 0
cardsByFixtureByRefandTeam <- cardsByFixtureByRefandTeam %>% group_by(official, team) %>% dplyr::mutate(medianYellow=median(yellow)) %>% ungroup()

#
#Construct a new dataframe gameDAs of cards, fouls and official by fixture
#
S <- eventsX %>% select(season, fixture, official) %>% distinct()
R <- eventsX %>% filter(event_type == 'Card') %>% select(official, season, fixture, card_type, mins)
R <- R %>% group_by(season, fixture) %>% dplyr::mutate(first_yellow_mins=round(min(mins),digits=2)) %>% ungroup()
R <- R %>% group_by(official, season, fixture, card_type) %>% dplyr::mutate(cards=row_number()) %>% top_n(1, cards) %>% ungroup()
R <- R %>% dplyr::mutate(R, yellow_tmp = ifelse(card_type == 'Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, second_yellow_tmp = ifelse(card_type == 'Second Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, red_tmp = ifelse(card_type == 'Red', cards, 0))
R <- R %>% group_by(official, season, fixture) %>% dplyr::mutate(yellow=max(yellow_tmp)) %>% top_n(1, yellow) %>% ungroup()
R <- R %>% group_by(official, season, fixture) %>% dplyr::mutate(second_yellow=max(second_yellow_tmp)) %>% top_n(1, second_yellow) %>% ungroup()
R <- R %>% group_by(official, season, fixture) %>% dplyr::mutate(red=max(red_tmp)) %>% top_n(1, red) %>% ungroup()
R <- R %>% select(official,season, fixture, yellow,second_yellow,red, first_yellow_mins) %>% distinct()
#Cards by ref and fixture
gameDAs <- merge(S, R, by=c("official","season","fixture"), all.x = TRUE)
gameDAs[is.na(gameDAs)] = 0
#Figure 4-7: Example transformation of a dataset
#Transformation of Events dataset into foulsByRefereeAndFixture dataset
temp <- events %>% filter(events$foul_won_lost == "Foul Conceded")              #step 1
temp <- temp %>% select(official, season, fixture, event_type)                  #step 2
temp <- temp %>% group_by(official, season, fixture)                            #step 3
temp <- temp %>% dplyr::mutate(fouls=row_number()) %>% top_n(1, fouls)          #step 4
temp <- temp %>% ungroup()                                                      #step 5
temp <- temp %>% select(official, season, fixture, fouls)                       #step 6
temp <- temp %>% distinct()                                                     #step 7
foulsByRefereeAndFixture <- temp                                                #step 8
gameDAs <- merge(gameDAs, foulsByRefereeAndFixture, by=c("official","season","fixture"), all.x = TRUE)
gameDAs[is.na(gameDAs)] = 0

#Creates array with all teams in dataset
Teams = c("Manchester United", "Chelsea", "Manchester City", "Arsenal", "Tottenham Hotspur", "Liverpool", "Everton", "Fulham", "Aston Villa", "Sunderland", "West Bromwich Albion", "Newcastle United", "Stoke City", "Bolton Wanderers", "Blackburn Rovers", "Wigan Athletic", "Wolverhampton Wanderers", "Birmingham City", "Blackpool", "West Ham United"
          ,"Swansea City", "Norwich City", "Cardiff City", "Queens Park Rangers", "Southampton", "Reading", "Hull City", "Crystal Palace", "Burnley", "Leicester City")

#Creates array with all refs in dataset
Refs = c("Ref01", "Ref02", "Ref03", "Ref04", "Ref05", "Ref06", "Ref07", "Ref08", "Ref09", "Ref10", "Ref11", "Ref12", "Ref13", "Ref14", "Ref15", "Ref16", "Ref17", "Ref18") 

#
#  GLOBAL FUNCTIONS
#  ----------------
#

#count of yellow cards
getYellowCardsCount <- function(){
  nrow(events %>% filter(card_type == "Yellow"))
} 

#yellow card counts by referee
getYellowNumberCardsByRef <- function(referee){
  nrow(events %>% filter(events$official == referee & card_type == "Yellow"))
} 

#red card counts by referee
getRedNumberCardsByRef <- function(referee){
  nrow(events %>% filter(events$official == referee & card_type == "Red"))
} 

#yellow card count by referee for a given team
getYellowNumberCardsByRefForTeam <- function(referee, cardedTeam){
  nrow(events %>% filter(events$official == referee & events$card_type == "Yellow" &  events$team == cardedTeam))
} 

#number of yellow cards for all refs for a given team
getNumberYellowForTeam <- function(refdTeam){
  nrow(events %>% filter(events$card_type == "Yellow" & events$team == refdTeam))
}

#determine experience of ref
refExperienceLevel <- function(ref){
  if (getNumberGamesByRef(ref) > 119){
    expLvl = 1
  }
  else if (getNumberGamesByRef(ref) > 99){
    expLvl = 2
  }
  else {
    expLvl = 3
  }
  return(expLvl)
}

#experience level of all refs
expLvlForAllRefs <- function(){
  allrefsLvls <- c(refExpereinceLevel(Refs[1]), refExpereinceLevel(Refs[2]), refExpereinceLevel(Refs[3]),
                   refExpereinceLevel(Refs[4]), refExpereinceLevel(Refs[5]), refExpereinceLevel(Refs[6]), 
                   refExpereinceLevel(Refs[7]), refExpereinceLevel(Refs[8]), refExpereinceLevel(Refs[9]),
                   refExpereinceLevel(Refs[10]), refExpereinceLevel(Refs[11]), refExpereinceLevel(Refs[12]),
                   refExpereinceLevel(Refs[13]), refExpereinceLevel(Refs[14]), refExpereinceLevel(Refs[15]),
                   refExpereinceLevel(Refs[16]), refExpereinceLevel(Refs[17]), refExpereinceLevel(Refs[18]))
}

