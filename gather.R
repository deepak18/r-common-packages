data("VADeaths")
head(VADeaths)

library(tidyr)

# Move age from row names into a column
VADeaths  <- VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths)) 
VADeaths


# Gather everything but age to tidy data
VADeaths %>%
  gather(key = key, value = death_rate, -age)


library("faraway")
data(worldcup)

library(ggplot2)
worldcup %>%
  select(Position, Time, Shots, Tackles, Saves) %>% 
  gather(Type, Number, -Position, -Time) %>%
  ggplot(aes(x = Time, y = Number)) + 
  geom_point() + 
  facet_grid(Type ~ Position)

library(knitr)

# Summarize the data to create the summary statistics you want
wc_table <- worldcup %>% 
  filter(Team %in% c("Spain", "Netherlands", "Uruguay", "Germany")) %>%
  select(Team, Position, Passes) %>%
  group_by(Team, Position) %>%
  summarize(ave_passes = mean(Passes),
            min_passes = min(Passes),
            max_passes = max(Passes),
            pass_summary = paste0(round(ave_passes), " (", 
                                  min_passes, ", ",
                                  max_passes, ")")) %>%
  select(Team, Position, pass_summary)

wc_table

# Use spread to create a prettier format for a table
wc_table %>%
  spread(Position, pass_summary) %>%
  kable()

wc_table %>%
  spread(Team, pass_summary) %>%
  kable()


library(readr)
# Merge multiple data frames
team_standings <- read_csv("data/team_standings.csv")
