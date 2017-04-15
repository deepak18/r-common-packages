library("faraway")
data(worldcup)
worldcup

library(magrittr)       #forward-pipe
library(dplyr)          #mutate
worldcup <- worldcup %>%
  mutate(player_name = rownames(worldcup))

worldcup %>% slice(1:3)

#grouping then creating new column : output in ungroped format
worldcup1 <- worldcup %>% 
  group_by(Position) %>%
  mutate(ave_shots = mean(Shots)) %>%
  ungroup()

worldcup1 %>% slice(1:10)


#grouping then creating new column : output in the grouped format
worldcup2 <- worldcup %>%
  group_by(Position) %>%
  mutate(ave_shots = mean(Shots))

worldcup2 %>% slice(1:10)

worldcup3 <- worldcup %>% 
  group_by(Position)

worldcup3 %>% slice(1:10)

# single row for each different Position with the mean Shot
worldcup4 <- worldcup %>% 
  group_by(Position) %>%
  summarize(ave_shots = mean(Shots)) %>%
  ungroup()

worldcup4 %>% slice(1:4)

