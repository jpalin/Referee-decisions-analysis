#
#Q3 - When issuing cards are referees influenced by previous games they have officiated when judging player disciplinary actions?
#

#Construct a new dataframe teamDAs of team cards by official by fixture
S <- eventsX %>% select(season, fixture, official, team) %>% distinct()
R <- eventsX %>% filter(event_type == 'Card') %>% select(official, season, fixture, team, card_type, mins)
R <- R %>% group_by(season, fixture, team) %>% dplyr::mutate(first_yellow_mins=round(min(mins),digits=2)) %>% ungroup()
R <- R %>% group_by(official, season, fixture, team, card_type) %>% dplyr::mutate(cards=row_number()) %>% top_n(1, cards) %>% ungroup()
R <- R %>% dplyr::mutate(R, yellow_tmp = ifelse(card_type == 'Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, second_yellow_tmp = ifelse(card_type == 'Second Yellow', cards, 0))
R <- R %>% dplyr::mutate(R, red_tmp = ifelse(card_type == 'Red', cards, 0))
R <- R %>% group_by(official, season, fixture, team) %>% dplyr::mutate(yellow=max(yellow_tmp)) %>% top_n(1, yellow) %>% ungroup()
R <- R %>% group_by(official, season, fixture, team) %>% dplyr::mutate(second_yellow=max(second_yellow_tmp)) %>% top_n(1, second_yellow) %>% ungroup()
R <- R %>% group_by(official, season, fixture, team) %>% dplyr::mutate(red=max(red_tmp)) %>% top_n(1, red) %>% ungroup()
R <- R %>% select(official,season, fixture, team, yellow, second_yellow, red, first_yellow_mins) %>% distinct()
#
teamDAs <- merge(S, R, by=c("official","season","fixture","team"), all.x = TRUE)
teamDAs[is.na(teamDAs)] = 0

#Figure 5-22: Number of yellow cards (all teams) for each referee
ggplot(teamDAs, aes(official, yellow)) +
        geom_point(size=0.1, shape = ".") +
        scale_x_discrete(limits=c("Ref05","Ref06","Ref09","Ref17","Ref10","Ref11","Ref18","Ref12","Ref07","Ref14","Ref16","Ref04","Ref01","Ref15","Ref08","Ref13","Ref02","Ref03")) +
        theme(plot.caption = element_text(size=12,face="italic",hjust = 0),plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
        ggtitle("Number of yellow cards (all teams) for each referee") +
        xlab("Referee") +
        ylab("Number of yellow cards") +
        labs(caption = "Plots ordered left to right by mean number of yellow cards (red dot) for each referee") +
        geom_boxplot(fill='#B4C6E7', color="black") +
        stat_summary(fun.y="mean",color="red") +
        scale_y_continuous(limits=c(0,8), breaks=seq(0,8,0.5))

#Create a bubble chart of mean number of yellow cards grouped by team for each referee
dfsummary <- DAsByRefandTeam
# Convert team_exposure as a grouping variable
dfsummary$team_exposure <- as.factor(dfsummary$team_exposure)

#Figure 5-23: Mean number of yellow cards grouped by team for each referee        
ggplot(dfsummary, aes(x = official, y = avg_yellow)) + 
        geom_point(aes(color = team_exposure, size = games), alpha = 1.0) +
        scale_color_manual(values = c("#4472C4","#B4C6E7","#262626"), name = "Team\nExposure") + #"#262626"
        theme(plot.caption = element_text(size=12,face="italic",hjust = 0), plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
        scale_size(range = c(0.1, 10), name = "Games\nofficiated") + # Adjust the range of points size
        geom_hline(yintercept=1.65, linetype=2, color="black", size=0.75) +  #1.65 = average cards per team per match
        labs(caption = "Dashed line is the mean number of yellow cards for all teams = 1.65 cards.",title="Mean number of yellow cards (grouped by team) for each referee", colour = "games", x = "Referee",y = "Mean number of yellow cards")
        
#Summary statistics by team exposure
#Table 5-4: Means of yellow cards for the different referee team exposure groups
group_by(dfsummary, dfsummary$team_exposure) %>%
        dplyr::summarise(
                count = dplyr::n(),
                mean = mean(avg_yellow, na.rm = TRUE),
                sd = sd(avg_yellow, na.rm = TRUE),
                median = median(avg_yellow, na.rm = TRUE),
                IQR = IQR(avg_yellow, na.rm = TRUE)
        )

#Construct a new dataframe teamsWithBigExposure for teams that have had high exposure to a referee (>14 games)
teamsWithBigExposure <- DAsByRefandTeam %>% filter(team_exposure == 1)
R <- teamsWithBigExposure %>% group_by(team) %>% dplyr::mutate(norefs=row_number()) %>% top_n(1, norefs) %>% ungroup() %>% select(team,norefs)
teamsWithBigExposure <- merge(teamsWithBigExposure, R, by=c("team"), all.x = TRUE)

#Figure 5-24: Mean number of yellow cards grouped by referee for teams with high referee exposure
ggplot(teamsWithBigExposure, aes(team, avg_yellow)) +
        geom_point(size=0.1, shape = ".") +
        scale_x_discrete(limits=c("Everton","West Bromwich Albion","Tottenham Hotspur","Liverpool","Sunderland","Chelsea","Newcastle United","Arsenal","Manchester City","Manchester United","Aston Villa","West Ham United","Stoke City")) +
        theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle("Mean number of yellow cards (grouped by referee) for teams with high referee exposure") +
        xlab("Team") +
        ylab("Mean number of yellow cards") +
        geom_boxplot(fill='#B4C6E7', color="black") +
        stat_summary(fun.y="mean",color="red") +
        scale_y_continuous(limits=c(1,2.75), breaks=seq(1,2.75,0.25))

#Look at cards from experienced referees (at least 15 games)
cardsByFixtureByExperiencedRefsforNewcastleUnited <- cardsByFixtureByRefandTeam %>% filter(team == 'Newcastle United' & (official == 'Ref01' | official == 'Ref02' | official == 'Ref03' | official == 'Ref08' | official == 'Ref09' | official == 'Ref12' | official == 'Ref14'))
cardsByFixtureByExperiencedRefsforSunderland <- cardsByFixtureByRefandTeam %>% filter(team == 'Sunderland' & (official == 'Ref03' | official == 'Ref12' | official == 'Ref08' | official == 'Ref09'))
cardsByFixtureByExperiencedRefsforTottenhamHotspur <- cardsByFixtureByRefandTeam %>% filter(team == 'Tottenham Hotspur' & (official == 'Ref02' | official == 'Ref03' | official == 'Ref07'))

#Create a box plot looking at Sunderland specifically

#Convert official as a grouping variable
cardsByFixtureByExperiencedRefsforSunderland$official <- as.factor(cardsByFixtureByExperiencedRefsforSunderland$official)

#Statistics data for Sunderland to annotate box plot with 
stat_box_data <- function(y, upper_limit = max(cardsByFixtureByExperiencedRefsforSunderland$yellow) * 1.15) {
        return( 
                data.frame(
                        y = 0.95 * upper_limit,
                        label = paste('games =', length(y), '\n',
                                      'mean cards =', round(mean(y), 1), '\n',
                                      'IQR =', IQR(y), '\n')                       
                )
        )
}

#Figure 5-25: Number of yellow cards issued to Sunderland for referees with high team exposure
ggplot(cardsByFixtureByExperiencedRefsforSunderland, aes(official, yellow)) +
        geom_point(size=0.1, shape = ".") +
        scale_x_discrete(limits=c("Ref09","Ref08","Ref12","Ref03")) +
        theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
        ggtitle("Number of yellow cards issued to Sunderland for referees with high team exposure") +
        xlab("Referee") +
        ylab("Number of yellow cards") +
        geom_boxplot(fill='#B4C6E7', color="black") +
        stat_summary(fun.y="mean",color="red") +
        stat_summary(
                fun.data = stat_box_data, 
                geom = "text", 
                hjust = 0.5,
                vjust = 0.9
        ) + 
        scale_y_continuous(limits=c(0,7), breaks=seq(0,7,0.5))

#Figure 5-26: Kruskal-Wallis rank sum test on the number of yellow cards issued to Newcastle, Tottenham and Sunderland
kruskal.test(yellow ~ official, data = cardsByFixtureByExperiencedRefsforNewcastleUnited)
kruskal.test(yellow ~ official, data = cardsByFixtureByExperiencedRefsforTottenhamHotspur)
kruskal.test(yellow ~ official, data = cardsByFixtureByExperiencedRefsforSunderland)

#Figure 5-27: Dunn's pairwise referee comparison tests for Sunderland
dunn_test(cardsByFixtureByExperiencedRefsforSunderland,yellow ~ official,p.adjust.method = "BH",detailed = FALSE)

