# Project Goal, Check how many people voted per the amount in State
# 1. Add economic change data to the graph
# 2. Analyze whether the economic change correlates to the registered voters
 
library(readr)
library(dplyr)
library(ggplot2)
projDirec <- "projects/tidy/polling/" 


popReg <- read_csv(projDirec+ "RegVote.csv")
#modRed <- transform(popReg, 
#                    unregistered = round((RegisteredVoters + (1 - (RegisteredVotersAsPercentOfVotingAgePopulation / 100)))))
modReg <- popReg
modReg <- transform(popReg, unregistered = modReg$RegisteredVoters
                    - (modReg$RegisteredVoters * (modReg$RegisteredVotersAsPercentOfVotingAgePopulation / 100)))
modReg$RegisteredVotersAsPercentOfVotingAgePopulation <- round(modReg$RegisteredVotersAsPercentOfVotingAgePopulation, 1)
modReg$unregistered <- round(modReg$unregistered)
############
turn <- read_csv(projDirec + "turnout.csv")
# names(turn)[names(turn) == 'eligTurnoutRatePres'] <- 'presTurnout'

modReg$turnout <- turn$presTurnout  #this works because the Y axis happens to be the same
modReg$turnout <- round(modReg$turnout, 3)

#View(modReg)
grow <- read_csv(file.path(projDirec, "EconGrowth.csv"))

#regEcon <- left_join(modReg, grow, by = c('state' = 'State'))
View(regEcon)

Swing <- c('Arizona', 'Georgia', 'Michigan', 'Nevada',
          'North Carolina', 'Pennsylvania', 'Wisconsin')
Democratic <- c('Maine', 'Maryland', 'Massachusetts', 'New Hampshire', 'New York',
                'Washington', 'Virginia', 'New Mexico', 'Oregon', "New Jersey",
                'Hawaii', 'Minnesota')
Na <- c('District of Columbia', 'United States')

regEcon <- transform(regEcon, PartyTrend = ifelse(regEcon$state %in% Swing, "Swing",
                                                   ifelse(regEcon$state %in% Democratic, "Dem", 
                                                          ifelse(regEcon$state %in% Na, "NA", "Rep"))))
swingData <- regEcon %>%
  filter(PartyTrend == "Swing")
write_csv(swingData, file.path(projDirec, "saved", "SwingData.csv"))

write_csv(regEcon, file.path(projDirec, "saved", "stateRegEcon.csv"))

regEcon <- read_csv(file.path(projDirec, "saved", "stateRegEcon.csv"))

ggplot(regEcon, aes(x = GDPGrowth, y = turnout, color= PartyTrend, group = PartyTrend)) +
  geom_point() +
  theme(plot.title = element_text(size=14, face="bold", hjust = 0.5)) +
  labs(x = "GDP Growth", y = "Voter Turnout", title = "Voter Turnout per GDP Growth and Party Affinity", fill = "Party Affinity") +
  geom_smooth(method = 'lm', se = FALSE, fullrange = TRUE) +
  scale_color_manual(values = c("blue", "red", "purple", "black")) 
