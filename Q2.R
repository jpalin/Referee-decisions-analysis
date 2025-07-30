#
#Q2 - Do referees perform differently when judging disciplinary actions?
#

#
#Construct a new dataframe refDAs of games, cards (all types) and fouls by official
#
#Table 5-2: Summary statistics by referee
refDAs <- DAsByRefandTeam %>% select(official, games, fouls, yellow, second_yellow, red)
refDAs <- refDAs %>% group_by(official) %>% dplyr::mutate(matches=sum(games)) %>% dplyr::mutate(yellows=sum(yellow)) %>% dplyr::mutate(second_yellows=sum(second_yellow)) %>% dplyr::mutate(reds=sum(red)) %>% dplyr::mutate(fouls2=sum(fouls)) %>% ungroup()
refDAs <- refDAs %>% group_by(official) %>% dplyr::mutate(avg_yellows=round(yellows/matches, digits=2)) %>% dplyr::mutate(avg_fouls=round(fouls2/matches, digits=2)) %>% ungroup()
refDAs <- refDAs %>% select(official,matches,fouls2,yellows,second_yellows,reds,avg_yellows,avg_fouls) %>% distinct()

#Figure 5-12: Distribution of the number of yellow cards for each referee
ggplot(gameDAs, aes(official, yellow)) +
        geom_point(size=0.1, shape = ".") +
        scale_x_discrete(limits=c("Ref05","Ref06","Ref09","Ref07","Ref10","Ref14","Ref18","Ref11","Ref12","Ref17","Ref01","Ref04","Ref08","Ref15","Ref16","Ref02","Ref13","Ref03")) +
        theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=+0.5)) +
        ggtitle("Distribution of the number of yellow cards issued for each referee") +
        xlab("Referee") +
        ylab("Yellow cards per match") +
        geom_boxplot(fill='#B4C6E7', color="black") +
        scale_y_continuous(limits=c(0,11), breaks=seq(0,11,1))

#Figure 5-16: Kruskal-Wallis test result for cards issued by referees
kruskal.test(yellow ~ official, data = gameDAs)

#Figure 5-17: Pairwise Wilcoxon rank sum test on yellow cards issued by referees
pairwise.wilcox.test(gameDAs$yellow, gameDAs$official, p.adjust.method = "BH")

#Figure 5-13: Distribution of the number of fouls awarded for each referee
ggplot(gameDAs, aes(official, fouls)) +
        geom_point(size=0.1, shape = ".") +
        scale_x_discrete(limits=c("Ref05","Ref06","Ref04","Ref16","Ref09","Ref02","Ref01","Ref08","Ref07","Ref14","Ref12","Ref03","Ref11","Ref10","Ref18","Ref13","Ref15","Ref17")) +
        theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=+0.5)) +
        ggtitle("Distribution of the number fouls awarded for each referee") +
        xlab("Referee") +
        ylab("Fouls per match") +
        geom_boxplot(fill='#B4C6E7', color="black") +
        scale_y_continuous(limits=c(8,40), breaks=seq(8,40,2))

#Figure 5-18: Kruskal-Wallis test result for fouls awarded by referees
kruskal.test(fouls ~ official, data = gameDAs)

#Figure 5-19: Pairwise Wilcoxon rank sum test on fouls awarded by referees
pairwise.wilcox.test(gameDAs$fouls, gameDAs$official, p.adjust.method = "BH")

#
#Construct a new dataframe gameDAs2Sum of games, yellows, fouls by official
#
#Yellow cards for foul reasons
S <- eventsX %>% select(season, fixture, official) %>% distinct()
S <- S %>% group_by(official, season, fixture) %>% dplyr::mutate(game=row_number()) %>% top_n(1, game) %>% ungroup() %>% select(official, season, fixture, game)
R <- eventsX %>% filter(event_type == 'Card' & card_type == 'Yellow') %>% select(official, season, fixture, card_type, reason)
R <- R %>% filter(reason == 'Foul' | reason == 'Off the ball foul' | reason == 'Persistent Infringement' | reason == 'Dangerous play' | reason == 'Professional Foul Last Man')
R <- R %>% group_by(official, season, fixture) %>% dplyr::mutate(cards=row_number()) %>% top_n(1, cards) %>% ungroup()
R <- R %>% dplyr::mutate(R, yellow_tmp = ifelse(card_type == 'Yellow', cards, 0))
R <- R %>% group_by(official, season, fixture) %>% dplyr::mutate(yellow=max(yellow_tmp)) %>% top_n(1, yellow) %>% ungroup() %>% select(official, season, fixture, yellow)
gameDAs2 <- merge(S, R, by=c("official","season","fixture"), all.x = TRUE)
gameDAs2[is.na(gameDAs2)] = 0
#fouls
R <- eventsX %>% filter(eventsX$foul_won_lost == "Foul Conceded") %>% select(official, season, fixture, event_type)
R <- R %>% group_by(official, season, fixture) %>% dplyr::mutate(fouls=row_number()) %>% top_n(1, fouls) %>% ungroup()
R <- R %>% select(official,season, fixture, fouls) %>% distinct()
gameDAs2 <- merge(gameDAs2, R, by=c("official","season","fixture"), all.x = TRUE)
gameDAs2[is.na(gameDAs2)] = 0
gameDAs2Sum <- gameDAs2 %>% group_by(official) %>% dplyr::mutate(games=sum(game)) %>% dplyr::mutate(yellows=sum(yellow)) %>% dplyr::mutate(fouls2=sum(fouls)) %>% ungroup()
gameDAs2Sum <- gameDAs2Sum %>% dplyr::mutate(avg_yellows=round(yellows/games, digits=2)) %>% dplyr::mutate(avg_fouls=round(fouls2/games, digits=2))
gameDAs2Sum <- gameDAs2Sum %>% select(official, games, yellows, fouls2, avg_yellows, avg_fouls) %>% distinct()

#
#Construct a new dataframe gameDAs2SumFrame of avg. yellow cards and avg. fouls by official (contains 2 rows for each official)
#
createGameDAs2SumFrame <- function(g) {
        b <- data.frame(official=character(), das=character(), avg=double())
        
        for(i in 1:length(g$official)) {
                b <- b %>% add_row(official = g$official[i], das = "Yellows", avg = round(g$yellows[i] / g$games[i], digits=2))  
                b <- b %>% add_row(official = g$official[i], das = "Fouls", avg = round(g$fouls2[i] / g$games[i], digits=2))  
        }
        
        return(b)
}
gameDAs2SumFrame <- createGameDAs2SumFrame(gameDAs2Sum)

#Figure 5-14: Comparison of foul and card numbers by referee
ggplot(gameDAs2SumFrame, aes(fill=das, y=avg, x=official)) + 
        geom_bar(position="dodge", stat="identity", width=0.5) +
        scale_x_discrete(limits=c("Ref05","Ref06","Ref04","Ref09","Ref16","Ref02","Ref08","Ref15","Ref07","Ref14","Ref01","Ref12","Ref11","Ref03","Ref13","Ref17","Ref18","Ref10")) +
        scale_fill_manual(values = c("#B4C6E7","#4472C4")) +  #"#262626" grey  #FABA6C yellow "#D6603D" pink
        ggtitle("Comparison of the mean numbers of fouls and yellow cards (all games) by referee") +
        labs(fill = "Disciplinary\nAction") +
        theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
        xlab("Referee") +
        ylab("Means per game")

#Figure 5-15: Scatter plot of foul and yellow card numbers by referee
ggplot(gameDAs2Sum, aes(x=avg_yellows, y=avg_fouls, label=official)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +
        geom_text(aes(label=official),hjust=-0.10, vjust=+0.8) +
        ggtitle("Foul and yellow card numbers per game by referee") +
        theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.25),axis.title.y = element_text(size=12, vjust=+0.5)) +
        xlab("Mean yellow cards per game") +
        ylab("Mean fouls per game")

#Figure 5-20: Shapiro-Wilk normality tests on average cards and fouls per game
shapiro.test(gameDAs2Sum$avg_yellows)
shapiro.test(gameDAs2Sum$avg_fouls)

#Figure 5-21: Pearson's product-moment correlation between yellow cards and fouls
cor.test(gameDAs2Sum$avg_yellows, gameDAs2Sum$avg_fouls, method = "pearson")

#Table 5-3: Yellow card reasons
R <- eventsX %>% filter(event_type == 'Card' & card_type == 'Yellow') %>% select(official, season, fixture, card_type, reason)
R <- R %>% group_by(reason) %>% dplyr::mutate(count=row_number()) %>% top_n(1, count) %>% ungroup() %>% select(reason, count)

#Cards for foul reasons
S <- R %>% filter(reason == 'Foul' | reason == 'Off the ball foul' | reason == 'Persistent Infringement' | reason == 'Dangerous play' | reason == 'Professional Foul Last Man')

