library(readr)
# teams <- read_csv("Week1/data/team_standings.csv")
teams <- read_csv("data/team_standings.csv", col_types = "cc")
teams

logs <- read_csv("data/2017-01-10-r.csv.gz", n_max = 10)
logs <- read_csv("data/2017-01-10-r.csv.gz", col_types = "ccicccccci", n_max = 10)
logs

logdates <- read_csv("data/2017-01-10-r.csv.gz", 
                     col_types = cols_only(date = col_date()),
                     n_max = 10)
logdates
