#
#Q4 - When judging player disciplinary actions, does a referee's game management change (e.g., the number of cards or frequency of cards) after the first card is issued?
#

#New dataframe of cards only
allcards <- eventsX %>% filter(eventsX$event_type == 'Card')
#6277

#New dataframe of all cards without Persistent Infringement reasons
allcardslessPI <- allcards %>% filter(allcards$reason != 'Persistent Infringement')

#Add card numbers into allcardslessPI for each fixture
allcardslessPI <- allcardslessPI %>% group_by(season, fixture) %>% dplyr::mutate(card_number=row_number()) %>% ungroup()

#Calc time interval between cards
addCardTimeIntervals <- function(c) {
  
  for(i in 1:length(c$season)) {
    
    if (c$card_number[i] == 1) {
      c$card_interval_secs[i] = -1
      c$card_interval_mins[i] = -1
    }      
    else {   
      c$card_interval_secs[i] = c$playing_time_updated_secs[i] - c$playing_time_updated_secs[i-1]
      c$card_interval_mins[i] = round((c$playing_time_updated_secs[i] - c$playing_time_updated_secs[i-1]) / 60,2)    
    }      
  }
  
  return(c)
}
#Add to allcardslessPI the card interval between consecutive cards, -1 indicates no time interval as for the first card
allcardslessPI <- addCardTimeIntervals(allcardslessPI)

#New dataframe of all card intervals
allcardintervals <- allcardslessPI %>% filter(allcardslessPI$card_interval_secs != -1) %>% select(card_number, card_interval_secs, card_interval_mins)
allcardintervals <- allcardintervals %>% mutate(card_interval_group = paste(as.character(card_number - 1),as.character(card_number),sep='-'))

#Convert allcardslessPI$card_number to a factor in order to facilitate plotting
allcardslessPI$card_number <- as.factor(allcardslessPI$card_number)
levels(allcardslessPI$card_number)

#stat_box_data2 is used to annotate the box plot of card numbers (Figure 5-30) with median values
stat_box_data2 <- function(y, upper_limit = max(allcardslessPI$mins) * 1.15) {
  return( 
    data.frame(
      y = upper_limit, #0.95 * upper_limit,
      label = paste('~x =', round(median(y), 1), '\n',
                    '         (mins)','\n')
    )
  )
}

#Figure 5-30: Playing times for each card number
ggplot(allcardslessPI, aes(x=card_number,y= mins)) +
  #scale_y_time() +
  geom_point(size=0.1, shape = ".") +
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11")) +
  theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
  ggtitle("Playing times for each card number (all games)") +
  xlab("Card number") +
  ylab("Playing time (mins)") +
  geom_boxplot(fill='#B4C6E7', color="black") +
  stat_summary(fun.y="mean",color="red") +
  stat_summary(
    fun.data = stat_box_data2, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  ) + 
  scale_y_continuous(limits=c(0,130), breaks=seq(0,130,10))


#Figure 5-29: Shapiro-Wilk normality test on card intervals
shapiro.test(allcardintervals$card_interval_secs)

#Figure 5-28: Time interval between consecutive cards
ggplot(allcardintervals, aes(x=card_interval_mins)) + 
  geom_histogram(color="black", fill="#B4C6E7",binwidth=10.0) +
  ggtitle("Histogram of time interval between consecutive cards (all games)") +
  #theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_text(vjust=-0.5),axis.title.y = element_text(vjust=+1.0)) +
  theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+0.5)) +
  xlab("Time interval between consecutive cards (mins)") +
  ylab("Frequency") +
  scale_x_continuous(expand=c(0,0), limits=c(0,100), breaks=seq(0,100,10)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1600), breaks=seq(0,1600,100))

#Convert Card Interval Group into a grouping variable
allcardintervals$card_interval_group <- as.factor(allcardintervals$card_interval_group)
levels(allcardintervals$card_interval_group)

#Re-order levels to correct order
allcardintervals$card_interval_group <- ordered(allcardintervals$card_interval_group,
                                                levels = c("1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11"))

#Figure 5-32: Median time intervals between consecutive cards
#Summary statistics by group
group_by(allcardintervals, allcardintervals$card_interval_group) %>%
  dplyr::summarise(
    count = dplyr::n(),
    mean = mean(card_interval_secs, na.rm = TRUE),
    sd = sd(card_interval_secs, na.rm = TRUE),
    median = median(card_interval_secs, na.rm = TRUE),
    IQR = IQR(card_interval_secs, na.rm = TRUE)
  )

#Figure 5-31: Playing time intervals between consecutive cards
ggplot(allcardintervals, aes(allcardintervals$card_interval_group, allcardintervals$card_interval_mins)) +
  #scale_y_time() +
  geom_point(size=0.1, shape = ".") +
  scale_x_discrete(limits=c("1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11")) +
  theme(plot.title = element_text(size=16,hjust = 0.5),axis.title.x = element_text(size=12, vjust=-0.5),axis.title.y = element_text(size=12, vjust=+1.0)) +
  #theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold")) +
  #theme(legend.text=element_text(size=18,face="bold")) +
  #theme(plot.title = element_text(size=18,face="bold",hjust = 0.5)) +
  ggtitle("Playing time interval between consecutive cards") +
  xlab("Card numbers") +
  ylab("Playing time (mins)") +
  geom_boxplot(fill='#B4C6E7', color="black") +
  scale_y_continuous(limits=c(0,90), breaks=seq(0,90,10))

#Figure 5-33: Kruskal-Wallis rank sum test on card intervals between consecutive cards
kruskal.test(card_interval_secs ~ card_interval_group, data = allcardintervals)

#Figure 5-34: Pairwise Wilcoxon rank sum test on card interval groups
pairwise.wilcox.test(allcardintervals$card_interval_secs, allcardintervals$card_interval_group, p.adjust.method = "BH")


