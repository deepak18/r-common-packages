aqs_sites <- read_excel("data/aqs_sites.xlsx")
head(aqs_sites)

daily_measures <- read_csv("data/daily_SPEC_2014.csv.bz2")
head(daily_measures) 
  
q1 <- daily_measures %>%
      filter(`State Name` == "Wisconsin" & `Parameter Name` == "Bromine PM2.5 LC") %>%
      select(`Arithmetic Mean`, `State Name`, `Parameter Name`)


head(q1)

mean_bromine_wisc <- mean(q1$`Arithmetic Mean`, na.rm = TRUE)


aggregate(cbind(`State Name`, `Parameter Name`)~`Arithmetic Mean`, data = q1, FUN = mean)
colMeans(q2$`Arithmetic Mean`)

#-----------------------------------------------------------------------------------------------------#

mysample <- daily_measures[sample(1:nrow(daily_measures), 50,
                                  replace=FALSE),]

q2 <- daily_measures%>%
      select(`State Name`, `Arithmetic Mean`, `Parameter Name`) %>%
      group_by(`Parameter Name`) %>%
      summarise(avg_par = mean(`Arithmetic Mean` , na.rm = TRUE))

para_name_highest <- q2[order(-q2$avg_par),]


q2_ans <- tapply(daily_measures$`Parameter Name`, X = daily_measures$`Arithmetic Mean`, FUN = mean)
sort(q2_ans, decreasing = T)

#-----------------------------------------------------------------------------------------------------#

q3 <- daily_measures %>%
      filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
      select(`Parameter Name`, `Arithmetic Mean`, `State Code`, `County Code`, `Site Num`) %>%
      group_by(`Parameter Name`, `State Code`, `County Code`, `Site Num`) %>%
      summarize(avg_level = mean(`Arithmetic Mean`, na.rm = TRUE))

q3_arrange <-  arrange(q3, desc(`avg_level`))


q3 <- as.data.frame(q3)      
tapply(q3$`Arithmetic Mean` , INDEX = q3$`State Code` , FUN = mean, na.action = na.omit)

#-----------------------------------------------------------------------------------------------------#

q4_EC_Cali <- daily_measures %>%
              filter(`Parameter Name` == "EC PM2.5 LC TOR" & `State Name` %in% c("California", "Arizona")) %>%
              select(`Parameter Name`, `Arithmetic Mean`, `State Name`, `State Code`) %>%
              group_by(`State Name`, `State Code`) %>%
              summarize(avg_level = mean(`Arithmetic Mean`, na.rm = TRUE))


q4_EC_Cali$avg_level[2] - q4_EC_Cali$avg_level[1]

#-----------------------------------------------------------------------------------------------------#
  
q5 <- daily_measures %>%
      filter(`Parameter Name` == "OC PM2.5 LC TOR" & Longitude < -100) %>%
      select(`Parameter Name`, `Arithmetic Mean`, Longitude)

median(q5$`Arithmetic Mean`, na.rm = FALSE)

#-----------------------------------------------------------------------------------------------------#

q6 <- aqs_sites %>%
      filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN")

count(q6)

#----------------------------------------------------------------------------------------------------#

q7 <- daily_measures %>%
      filter(`Parameter Name` == "EC PM2.5 LC TOR" & Longitude >= -100) %>%
      left_join(aqs_sites, by = c("Latitude", "Longitude")) %>%
      select(`Parameter Name`, `Arithmetic Mean`, `Site Num`, Latitude, Longitude, `Land Use`, `Location Setting`) %>%
      filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN")

median(q7$`Arithmetic Mean`, na.rm = FALSE)

#-----------------------------------------------------------------------------------------------------#
q8 <- daily_measures %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  left_join(aqs_sites, by = c("Latitude", "Longitude")) %>%
  filter(`Land Use` == "COMMERCIAL") %>%
  mutate(Month_DateLocal = months(`Date Local`),
         Month_DateLastChange = months(`Date of Last Change`)) %>%
  select(`Parameter Name`, `Arithmetic Mean`, `Month_DateLocal`, `Month_DateLastChange`, Latitude, Longitude, `Land Use`, `Location Setting`)
  
q8_avg_monthLocal <- q8 %>%
              group_by(Month_DateLocal) %>%
              summarize(Avg_level_Local = mean(`Arithmetic Mean` , na.rm = TRUE)) %>%
              arrange(desc(Avg_level_Local))

#-----------------------------------------------------------------------------------------------------#
q9 <- daily_measures %>%
      filter(`Parameter Name` == "Sulfate PM2.5 LC"  | `Parameter Name` == "Total Nitrate PM2.5 LC") %>%
      select(`Parameter Name`, `Arithmetic Mean`, Latitude, Longitude, `Date Local`) %>%
      left_join(aqs_sites, by = c("Latitude", "Longitude")) %>%
      filter(`State Code` == "6" & `County Code` == "65" & `Site Number` == "8001") %>%
      select(`Parameter Name`, `Arithmetic Mean`, `Date Local`, `State Code`, `County Code`, `Site Number`, `State Name`)

q9_avg_date <- q9 %>%
              select(`Parameter Name`, `Date Local` , `Arithmetic Mean`) %>%
              group_by(`Parameter Name`, `Date Local`) %>%
              summarize(Avg_Level = mean(`Arithmetic Mean`)) %>%
              ungroup()

q9_sum_date <- q9_avg_date %>%
              select(`Date Local` , Avg_Level) %>%
              group_by(`Date Local`) %>%
              summarize(Sum_Level = sum(Avg_Level))

count(q9_sum_date[q9_sum_date$Sum_Level > 10,])

#---------------------------------------------------------------------------------------------------#
q10 <- q9 <- daily_measures %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC"  | `Parameter Name` == "Total Nitrate PM2.5 LC") %>%
  select(`Parameter Name`, `Arithmetic Mean`, Latitude, Longitude, `Date Local`) %>%
  left_join(aqs_sites, by = c("Latitude", "Longitude")) %>%
  select(`Parameter Name`, `Arithmetic Mean`, `Date Local`, `State Code`, `County Code`, `Site Number`, `State Name`)

q10_avg_date <- q10 %>%
              group_by(`Parameter Name`, `Date Local`, `State Code`, `County Code`, `Site Number`) %>%
              summarize(Avg_Level = mean(`Arithmetic Mean`))
        #      unite(Site, `State Code`, `County Code`, `Site Number`) %>%
           
   

q10_site_matrix <- as.matrix(cbind(q10_avg_date$`State Code`, q10_avg_date$`County Code`, q10_avg_date$`Site Number`))
colnames(q10_site_matrix) <- c("State Code", "County Code", "Site Number")
cor(q10_site_matrix, q10_avg_date$Avg_Level)
