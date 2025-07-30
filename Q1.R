#
#Q1 - Does a referee issuing early cards lead to a fairer match, i.e., fewer cards and fouls, overall?
#

#
#Time distribution of cards
#--------------------------
#

#New dataframe of cards only
allcards <- eventsX %>% filter(eventsX$event_type == 'Card')
#6277

#Figure 5-4: Time distribution of cards
ggplot(allcards, aes(x=mins)) + 
  geom_histogram(color="black", fill="#B4C6E7",binwidth=1.0) +
  ggtitle("Histogram of time distribution of cards") +
  theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
  xlab("Playing time (minutes)") +
  ylab("Card count")

#
#Time distribution of fouls
#--------------------------
#

#New dataframe of fouls only
allfouls <- eventsX %>% filter(eventsX$foul_won_lost == "Foul Conceded")
#41131

#Figure 5-5: Time distribution of fouls
ggplot(allfouls, aes(x=mins)) + 
  geom_histogram(color="black", fill="#B4C6E7",binwidth=1) +
  ggtitle("Histogram of time distribution of fouls") +
  theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
  xlab("Playing time (minutes)") +
  ylab("Foul count")

#
#Box plots of early and late card and foul distributions
#-------------------------------------------------------
#

mins6 <- 6
mins17 <- 17

calcGameDAsTable <- function(periodGroup, minsIn) {

  table <- gameDAs %>% dplyr::mutate(cards=yellow + second_yellow + red) %>% dplyr::mutate(period_group=periodGroup)
  table <- table %>% group_by(season, fixture) %>% dplyr::mutate(sample_group=ifelse(first_yellow_mins==0,"No early cards",ifelse(first_yellow_mins < minsIn, "Early cards", "No early cards"))) %>% ungroup()

  #Fixtures
  F <- eventsX %>% select(season, fixture) %>% distinct()
  
  #All cards
  U <- eventsX %>% filter(event_type == 'Card') %>% select(season, fixture, card_type, mins)
  
  #Late cards
  R <- U %>% filter(mins >= minsIn) %>% group_by(season, fixture) %>% dplyr::mutate(late_cards=row_number()) %>% top_n(1, late_cards) %>% ungroup()
  R <- R %>% select(season, fixture, late_cards) %>% distinct()
  R <- merge(F, R, by=c("season","fixture"), all.x = TRUE)
  R[is.na(R)] = 0
  
  #All fouls
  Y <- eventsX %>% filter(eventsX$foul_won_lost == "Foul Conceded") %>% select(season, fixture, mins)

  #Late fouls
  W <- Y %>% filter(mins >= minsIn) %>% group_by(season, fixture) %>% dplyr::mutate(lfouls=row_number()) %>% top_n(1, lfouls) %>% ungroup()
  W <- W %>% select(season, fixture, lfouls) %>% distinct()
 
  R <- merge(R, W, by=c("season","fixture"), all.x = TRUE)
  R[is.na(R)] = 0  

  #Early cards
  V <- U %>% filter(mins < minsIn) %>% group_by(season, fixture) %>% dplyr::mutate(early_cards=row_number()) %>% top_n(1, early_cards) %>% ungroup()
  V <- V %>% select(season, fixture, early_cards) %>% distinct()
  R <- merge(R, V, by=c("season","fixture"), all.x = TRUE)
  R[is.na(R)] = 0
  
  #Early fouls
  S <- Y %>% filter(mins < minsIn) %>% group_by(season, fixture) %>% dplyr::mutate(efouls=row_number()) %>% top_n(1, efouls) %>% ungroup()
  S <- S %>% select(season, fixture, efouls) %>% distinct()
  
  R <- merge(R, S, by=c("season","fixture"), all.x = TRUE)
  R[is.na(R)] = 0
  
  table <- merge(table, R, by=c("season","fixture"), all.x = TRUE)
  table[is.na(table)] = 0  

  return(table)
}

#
#Data for 6 minute early time period
#
gameDAsTable6mins <- calcGameDAsTable("6 minutes", mins6)
#
#Data for 17 minute early time period
#
gameDAsTable17mins <- calcGameDAsTable("17 minutes", mins17)

#
#Table 5-1: Card and foul numbers for the early time periods under investigation sourced from gameDAsTable6mins and gameDAsTable17mins
#

#
#Data for both early time periods
#
gameDAsTable <- rbind(gameDAsTable17mins, gameDAsTable6mins)

#Box plots of early and late card distributions
#Figure 5-6: Card numbers in games for the two early time periods under investigation
ggplot(gameDAsTable, aes(x=period_group, y=late_cards, fill=sample_group)) + 
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#4472C4","#B4C6E7")) +
  ggtitle("Card numbers in games with and without early cards") +
  xlab("Early time period") +
  ylab("Cards after early time period") +
  labs(fill = "Games group") +
  scale_y_continuous(limits=c(0,11), breaks=seq(0,11,1)) + #, expand = c(0, 0)) +
  facet_wrap(~period_group, scale="free")

#Figure 5-8: Kruskal-Wallis test result for late cards with an early time period of 6 minutes
kruskal.test(late_cards ~ sample_group, data = gameDAsTable6mins)
#Figure 5-9: Kruskal-Wallis test result for late cards with an early time period of 17 minutes
kruskal.test(late_cards ~ sample_group, data = gameDAsTable17mins)

#Summary statistics by group
#Figure 5-10: Summary statistics for the early time period of 17 minutes
group_by(gameDAsTable17mins, gameDAsTable17mins$sample_group) %>%
  dplyr::summarise(
    count = dplyr::n(),
    mean = mean(late_cards, na.rm = TRUE),
    sd = sd(late_cards, na.rm = TRUE),
    median = median(late_cards, na.rm = TRUE),
    IQR = IQR(late_cards, na.rm = TRUE)
  )

#Box plots of early and late foul distributions
#Figure 5-7: Foul numbers in games for the two early time periods under investigation
ggplot(gameDAsTable, aes(x=period_group, y=lfouls, fill=sample_group)) + 
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#4472C4","#B4C6E7")) +
  ggtitle("Foul numbers in games with and without early cards") +
  xlab("Early time period") +
  ylab("Fouls after early time period") +
  labs(fill = "Games group") +
  scale_y_continuous(limits=c(4,40), breaks=seq(4,40,2)) + #, expand = c(0, 0)) +
  facet_wrap(~period_group, scale="free")

#Figure 5-11: Kruskal-Wallis tests for late fouls with early time periods of 6 and 17 minutes
kruskal.test(lfouls ~ sample_group, data = gameDAsTable6mins)
kruskal.test(lfouls ~ sample_group, data = gameDAsTable17mins)

#
#Distribution of the number of cards per game
#--------------------------------------------
#

#Figure 5-1: Frequency distribution of the number of cards for all games
ggplot(gameDAsTable6mins, aes(x=cards)) + 
  geom_histogram(color="black", fill="#B4C6E7",binwidth=1.0,stat="count") +
  ggtitle("Histogram of the number of cards for all games (all card types)") +
  theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
  xlab("Number of cards") +
  ylab("Number of games") +
  scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10","11")) +
  scale_y_continuous(limits=c(0,400), breaks=seq(0,400,50))

#
#Distribution of the number of fouls per game
#--------------------------------------------
#

#Figure 5-2: Frequency distribution of the number of fouls for all games
ggplot(gameDAsTable6mins, aes(x=fouls)) + 
  geom_histogram(color="black", fill="#B4C6E7",binwidth=1.0) +
  ggtitle("Histogram of the number of fouls for all games") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=+1.0)) +
  xlab("Number of fouls") +
  ylab("Number of games") +
  scale_x_continuous(limits=c(8,42), breaks=seq(8,42,2)) +
  scale_y_continuous(limits=c(0,150), breaks=seq(0,150,10))

#Figure 5-3: Shapiro-Wilk test on the number of fouls awarded in games
shapiro.test(gameDAsTable6mins$fouls)
