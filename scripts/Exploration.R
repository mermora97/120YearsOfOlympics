# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# 1. Load data -----------------------------------------------------------------------------------
current_folder <- getwd()
athletes_raw <- read.csv(file.path(current_folder, "data/athlete_events.csv"))
noc <- read.csv(file.path(current_folder, "data/noc_regions.csv"))
athletes <- merge(athletes_raw, noc, by = "NOC") %>%
  select('ID','Sex','Team','Age','Weight','Height','NOC','Year','Season','Sport','Event','Medal','region') %>% 
  rename_at('region', ~'AthleteCountry') %>%
  mutate(WeightHeightRation = Weight/Height)

View(athletes_raw)
games <- read.csv(file.path(current_folder, "data/olympicgames.csv")) %>%
  select('Country','Year')

# For now, to simplify lets use 2016 GPD and Population
countries_gdp <-read.csv(file.path(current_folder, "data/countries_gdp_per_capita.csv")) %>%
  select('CountryName','X2016') %>%
  rename_at('X2016', ~'GDP') %>%
  mutate(GDP = as.integer(GDP)) %>%
  mutate(GDP = GDP/1000000)
countries_pop <- read.csv(file.path(current_folder, "data/countries_pop.csv")) %>% 
  select('CountryName','X2016') %>%
  rename_at('X2016', ~'Population') %>%
  mutate(Population = Population/1000000)

# 2. Clean data -----------------------------------------------------------------------------------
# Clean country data in games table
setdiff(games$Country, athletes$AthleteCountry)
# Same name for different countries
country_mapping <- setNames(c('USA', 'UK', 'Germany', 'Russia', 'Serbia'),c('United States', 'United Kingdom', 'FR Germany', 'Soviet Union', 'Yugoslavia'))
games <- games %>%
  mutate(Country = ifelse(Country %in% names(country_mapping), country_mapping[as.character(Country)], Country))
setdiff(games$Country, athletes$AthleteCountry)
# I no longer have differences

# Clean country data in countries_pop table
setdiff(athletes$AthleteCountry, countries_pop$CountryName)
country_mapping <- setNames(c('USA', 'UK', 'Russia'),c('United States', 'United Kingdom', 'Russian Federation'))
countries_pop <- countries_pop %>%
  mutate(CountryName = ifelse(CountryName %in% names(country_mapping), country_mapping[as.character(CountryName)], CountryName))
setdiff(athletes$AthleteCountry, countries_pop$CountryName)
# I no longer have differences

# Clean country data in countries_gdp table
setdiff(athletes$AthleteCountry, countries_gdp$CountryName)
country_mapping <- setNames(c('USA', 'UK', 'Russia'),c('United States', 'United Kingdom', 'Russian Federation'))
countries_gdp <- countries_gdp %>%
  mutate(CountryName = ifelse(CountryName %in% names(country_mapping), country_mapping[as.character(CountryName)], CountryName))
setdiff(athletes$AthleteCountry, countries_gdp$CountryName)
# I no longer have differences

athletes %>% distinct(Year) %>% arrange(Year) 
# From 1992 to 2016, we have Olympics every 2 years. Separation of Summer and Winter Games decision.
# For simplifying purposes, lets group summer and winter Olympics into the same year
year_mapping <- setNames(c(1992, 1996, 2000, 2004, 2008, 2012), c(1994, 1998, 2002, 2006, 2010, 2014))
# Apply the mapping to the Year column
athletes <- athletes %>%
  mutate(AdjustedYear = ifelse(Year %in% names(year_mapping), year_mapping[as.character(Year)], Year))
games <- games %>%
  mutate(AdjustedYear = ifelse(Year %in% names(year_mapping), year_mapping[as.character(Year)], Year))

# Null values in dataset
null_count <- sapply(athletes, function(x) sum(is.na(x)))
total_rows <- nrow(athletes)
data.frame(Column = names(null_count), 
           NullCount = null_count, 
           NullPercentage = (null_count / total_rows) * 100)
# We are missing 23 and 22% of values in Weight and Height columns respectively
athletes %>% 
  pivot_longer(cols = c('Height','Weight','Age'), names_to = "Variable", values_to = "Value") %>%
  group_by(AdjustedYear, Variable) %>%
  summarise(
    TotalCount = n(),
    NullCount = sum(is.na(Value)),
    NullPercentage = (NullCount / TotalCount) * 100
    ) %>%
  ggplot(aes(x = AdjustedYear, y = NullPercentage, color = Variable)) + 
    geom_line()

athletes %>% filter(Year > 1990) %>%
  group_by(AthleteCountry) %>% 
  summarize(ssumnull = sum(is.na(Height))) %>% 
  arrange(desc(ssumnull))
# In the latest years, we have much more data about Weight and Height going from ~90% to ~0% missing values

# 2. Exploration--------------------------------------------------------------------------------- 

# YEAR -----------------------------------------------
c(min(athletes$Year), max(athletes$Year))
# We have data from 1896 to 2016

# SPORT -----------------------------------------------
# Q1: How many sports in each season? And how the #sports per season changed with time?
athletes %>% 
  group_by(Season,Year) %>% summarise(n_distinct(Sport)) %>%
  group_by(Season) %>% 
  filter(Year==max(Year) | Year==min(Year))
# The fist summer Summer Olympics (1896) included 9 sports, the last (2016) ones include 34
# The fist summer Winter Olympics (1924) included 10 sports, the last (2014) ones include 15

# Plot
athletes %>%
  group_by(Year, Season) %>%
  summarize(NSports = n_distinct(Sport)) %>%
  ggplot(aes(x = Year, y = NSports, color=Season)) +
  geom_line() +
  geom_point() +
  labs(title = "Distinct Sports by Year by Season",
       x = "Year",
       y = "Number of Distinct Sports") +
  theme_minimal()

# Q1.2: Which sports were eliminated from 2008 to 2012? And included from 2012 to 2016
# From 2008 to 2012 eliminated:
setdiff(
  athletes %>% filter(Season == 'Summer', Year == 2008) %>% pull(Sport) %>% unique(),
  athletes %>% filter(Season == 'Summer', Year == 2016) %>% pull(Sport) %>% unique())
# From 2012 to 2016 added:
setdiff(
  athletes %>% filter(Season == 'Summer', Year == 2016) %>% pull(Sport) %>% unique(),
  athletes %>% filter(Season == 'Summer', Year == 2008) %>% pull(Sport) %>% unique())

# Q1.3: Which sports have been in every Olympic year?
athletes %>%
  group_by(Sport) %>%
  summarise(Years_Present = n_distinct(AdjustedYear)) %>%
  filter(Years_Present == n_distinct(athletes$AdjustedYear)) %>%
  pull(Sport)

# Q1.4: Which sports have been played just once in the Olympics?
athletes %>%
  group_by(Sport) %>%
  summarize(YearCount = n_distinct(Year),
            YearPlayed = first(Year[which(n_distinct(Year) == 1)])) %>%
  filter(YearCount == 1)

# WEIGHT AND HEIGHT  -----------------------------------------------
cor(athletes$Weight, athletes$Height, use = "complete.obs")
# High correlation between Height and Weight is expected = 0.7

# Q2: Is the height distribution of athletes different in different sports?
athletes %>% 
  filter(Sport %in% c('Basketball','Football')) %>% 
  ggplot(aes(x = Height)) +
  geom_histogram(binwidth = 5, fill = 'blue', color = 'black', alpha = 0.7) +
  facet_wrap(~ Sport) +
  theme_minimal() +
  labs(title = 'Height Distribution by Sport', x = 'Height (cm)', y = 'Frequency')

# T test to check if Basketball players are taller than Football players
basketball_players <- athletes %>% filter(Sport == "Basketball")
football_players <- athletes %>% filter(Sport == "Football")
t.test(basketball_players$Height, football_players$Height, alternate='greater')
# We have enough supportable evidence to affirm Basketball players are on average 15-16 cm taller than football players  

# Q3: Is the weight distribution of athletes different in different sports?
athletes %>% 
  filter(Sport %in% c('Wrestling','Weightlifting','Judo')) %>% 
  ggplot(aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = 'blue', color = 'black', alpha = 0.7) +
  facet_wrap(~ Sport) +
  theme_minimal() +
  labs(title = 'Weight Distribution by Sport', x = 'Weight (kg)', y = 'Frequency')

# ANOVA test
weight_diffs <- athletes %>% 
  filter(Sport %in% c('Wrestling','Weightlifting','Judo'))
anova <- aov(Weight~Sport, data=weight_diffs)
summary(anova)
## Q2: What is the difference between the means?
TukeyHSD(anova, conf.level = 0.95)
plot(TukeyHSD(anova, conf.level = 0.95))

# T test to check if Wresting players are heavier than Judo players
# wrestling_players <- athletes %>% filter(Sport == "Wrestling")
# judo_players <- athletes %>% filter(Sport == "Judo")
# t.test(wrestling_players$Weight, judo_players$Weight, alternate='greater')
# We have enough supportable evidence to affirm Judo athletes are on average 2.4-4.1 Kg heavier than wrestling ones  

# Q4: Has weight and height changed over time?
athletes %>%
  group_by(AdjustedYear) %>%
  summarise(
    Mean_Height = mean(Height, na.rm=T),
    Mean_Weight = mean(Weight, na.rm=T)
  ) %>%
  pivot_longer(cols = c(Mean_Height, Mean_Weight), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = AdjustedYear, y = Value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = 'Mean Height and Weight Over Years by Sport', x = 'Year', y = 'Value')

# Q5: Which sports have seen largest differences in weight over time?
athletes %>%  
  group_by(Sport) %>%
  summarise(
    Mean_Height = mean(Height, na.rm=T),
    SD_Height = sd(Height, na.rm=T),
    Mean_Weight = mean(Weight, na.rm=T),
    SD_Weight = sd(Weight, na.rm=T)
    ) %>%
  arrange(desc(SD_Height))
# Basketball has seen larger changes in the height of the athletes over time
# Plot
athletes %>%
  filter(Sport %in% c('Basketball')) %>%
  group_by(AdjustedYear,Sport) %>%
  summarise( Mean_Height = mean(Height, na.rm=T) ) %>%
  ggplot(aes(x = AdjustedYear, y = Mean_Height, color=Sport)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = 'Mean Height Over Years by Sport', x = 'Year', y = 'Mean Weight (cm)')

# Q6: Which sports have seen largest differences in weight over time?
athletes %>%  
  group_by(Sport) %>%
  summarise(
    Mean_Weight = mean(Weight, na.rm=T),
    SD_Weight = sd(Weight, na.rm=T)
  ) %>%
  arrange(desc(SD_Weight))
# Weightlifting, Judo and Wrestling have seen larger changes in the weight of the athletes over time
# Plot
athletes %>%
  filter(Sport %in% c('Weightlifting','Judo','Wrestling')) %>%
  group_by(AdjustedYear,Sport) %>%
  summarise( Mean_Weight = mean(Weight, na.rm=T) ) %>%
  ggplot(aes(x = AdjustedYear, y = Mean_Weight, color=Sport)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = 'Mean Weight Over Years by Sport', x = 'Year', y = 'Mean Weight (Kg)')

# Q7: Can I predict chances of winning a medal in some sport by having the athlete weight or height?
athletes %>% 
  filter(Sport == 'Rowing' & Sex == 'F') %>%
  mutate(Medal = ifelse(is.na(Medal), 'No Medal', Medal)) %>%
  mutate(GoldMedal = Medal == 'Gold') %>%
  group_by(GoldMedal) %>%
  summarize(
    MeanHeight = mean(Height, na.rm = TRUE),
    MeanWeight = mean(Weight, na.rm = TRUE),
    MeanWHRation = mean(WeightHeightRation, na.rm = TRUE)
    )

athletes %>% 
  filter(Sport=='Rowing' & Sex=='F') %>%
  mutate(Medal = ifelse(is.na(Medal), 'No Medal', Medal)) %>%
  mutate(GoldMedal = ifelse(Medal != 'Gold',F,T)) %>%
  ggplot(aes(x = Height, y = Weight, color = GoldMedal)) +
  geom_point()
# We can add a column for weight-height ration
# In Swimming, it appears to be a perfect weight-height ratio
# In rowing, heavier and taller athletes
# Speed skating is interesting but too little data points

# Different categories in Weightlifting
athletes %>% filter(Sport == 'Weightlifting') %>% pull(Event) %>% unique()

athletes %>% 
  filter(Sport == 'Weightlifting') %>% 
  group_by(Event) %>%
  summarize(Weight=mean(Weight, na.rm=T),WHRation=mean(WeightHeightRation, na.rm=T)) %>%
  arrange(Weight)

athletes %>% 
  filter(Event=="Weightlifting Women's Heavyweight") %>%
  mutate(Medal = ifelse(is.na(Medal), 'No Medal', Medal)) %>%
  mutate(GoldMedal = ifelse(Medal != 'No Medal',T,F)) %>%
  ggplot(aes(x = Height, y = Weight, color = GoldMedal)) +
  geom_point()

athletes %>% 
  filter(Sport == 'Basketball') %>%
  group_by(Year, Team) %>%
  summarize(Height = mean(Height, na.rm=T), Medals= sum(!is.na(Medal))) %>%
  ungroup() %>%
  ggplot(aes(y = Medals, x = Height)) + geom_point()

## Before 1920 there were no subcategories for weights within Weightlifting
athletes %>% filter(Sport=='Weightlifting' & Year < 1920) %>% pull(Event) %>% unique()

# Fit a model to predict medals in Swimming using 

# GENDER -----------------------------------------------
# Q7: Has female participation increased in the last 10 years?
athletes %>%
  group_by(AdjustedYear) %>%
  summarize(FemaleAthletes = sum(Sex=='F')/n()*100, MaleAthletes = sum(Sex=='M'), .groups = 'drop') %>%
  mutate(LabelFlag = ifelse(row_number() %% 2 == 1, TRUE, FALSE)) %>%
  ggplot(aes(y = FemaleAthletes, x = AdjustedYear)) + 
  geom_line(color = 'blue') +
  geom_point(color = 'red') +
  geom_text(aes(label = ifelse(LabelFlag, paste0(round(FemaleAthletes, 0), "%"), '')),
            vjust = -0.5, color = "black") + 
  labs(
    title = "Female Athletes Participation Over the Years",
    x = "Year",
    y = "% Female Athletes"
  ) +
  theme_minimal()
table(athletes$Sex, athletes$AdjustedYear)
prop.test(c(6203,6382),c(13656,17350))
#We have enough supportable evidence to affirm the proportion of female athletes has increased between 7-9 percent points

# COUNTRY -----------------------------------------------
# Q8: Has country participation increased with time?
athletes %>%
  group_by(Year, Season) %>%
  summarize(NCountries = n_distinct(AthleteCountry)) %>%
  ggplot(aes(x = Year, y = NCountries, color=Season)) +
  geom_line() +
  geom_point() +
  labs(title = "Distinct Countries by Year",
       x = "Year",
       y = "Number of Distinct Countries") +
  theme_minimal()
# Participation in the Olympic Games has increased significantly in the last 20 years
# Higher participation in the Summer games compared to the Winter ones
# A dip in the Summer Olympic Games in 1980 - Boycott of the Moscow Olympics by US to protest against Afghanistan invasion

athletes %>%
  group_by(Year, Season) %>%
  summarize(NAthletes = n_distinct(ID)) %>%
  ggplot(aes(x = Year, y = NAthletes, color=Season)) +
  geom_line() +
  geom_point() +
  labs(title = "#Athlethes by Year",
       x = "Year",
       y = "Number of Distinct Athletes by Season") +
  theme_minimal()
# Same pattern in the number of athletes

# Q9: Which sports offer the more medals (gold medals) on average?
athletes %>% 
  filter(Year == 2016) %>%
  group_by(Sport) %>% summarize(NMedals = sum(!is.na(Medal) & Medal=='Gold')) %>% arrange(desc(NMedals)) %>%
  ggplot(aes(x = reorder(Sport, NMedals), y = NMedals)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = NMedals), size = 3, color = "black") +
  coord_flip() +
  labs(title = "Number of Medals by Sport",
       x = "Sport",
       y = "Number of Medals") +
  theme_minimal()

athletes %>% 
  filter(Year == 2016 & !(is.na(Medal))) %>%
  group_by(Sport, Medal) %>%
  summarize(NMedals = sum(!is.na(Medal)), .groups = 'drop') %>%
  mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze"))) %>%
  ggplot(aes(x = reorder(Sport, NMedals), y = NMedals, fill = Medal)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = NMedals), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_flip() +
  labs(title = "Number of Medals by Sport (2016)",
       x = "Sport",
       y = "Number of Medals") +
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "brown")) +
  theme_minimal()

# Why are there more gold medals than silver or bronze in some sports e.g. athletics?
athletes %>% 
  filter(Sport == 'Athletics' & Year == 2016 & !(is.na(Medal))) %>%
  group_by(Event, Medal) %>%
  summarize(NMedals = sum(!is.na(Medal)), .groups = 'drop') %>%
  mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze"))) %>%
  ggplot(aes(x = reorder(Event, NMedals), y = NMedals, fill = Medal)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = NMedals), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_flip() +
  labs(title = "Number of Medals by Sport (2016)",
       x = "Sport",
       y = "Number of Medals") +
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "brown")) +
  theme_minimal()

athletes %>% 
  filter(Event == "Athletics Women's 4 x 400 metres Relay" & Year == 2016 & Medal == 'Gold')

athletes %>% filter(Sport=='Rowing' & Year == 2016) %>%
  group_by(AthleteCountry) %>% summarize(NMedals = sum(!is.na(Medal) & Medal=='Gold')) %>% arrange(desc(NMedals))
# USA got 48 out of the 70 medals for Swimming in 2016

# Do countries include sports where they excel when hosting?
athletes_games <- athletes %>% left_join(games)
athletes_games %>% group_by(AdjustedYear, Country) %>% summarize(n())
setdiff(
  athletes %>% filter(Year == 1992) %>% pull(Sport) %>% unique(),
  athletes %>% filter(Year == 1988) %>% pull(Sport) %>% unique())

athletes_games %>% filter(Year == 2016 & Sport %in% c("Rugby Sevens","Golf")) %>%
  group_by(Country, AthleteCountry, Sport) %>% summarize(Medals = sum(!is.na(Medal))) %>%
  arrange(desc(Medals))
athletes_games %>% filter(Year == 2000 & Sport %in% c("Triathlon","Trampolining","Taekwondo")) %>%
  group_by(Country, AthleteCountry, Sport) %>% summarize(Medals = sum(!is.na(Medal))) %>%
  arrange(desc(Medals))
athletes_games %>% filter(Year == 1996 & Sport %in% c("Beach Volleyball","Softball")) %>%
  group_by(Country, AthleteCountry, Sport) %>% summarize(Medals = sum(!is.na(Medal))) %>%
  arrange(desc(Medals))
athletes_games %>% filter(Year == 1992 & Sport %in% c("Freestyle Skiing","Badminton","Short Track Speed Skating","Baseball")) %>%
  group_by(Country, AthleteCountry, Sport) %>% summarize(Medals = sum(!is.na(Medal))) %>%
  arrange(desc(Medals))

# Do countries with historic wins win more?


# Q9: Which country has won more medals each year?
total_medals_per_year_season <- athletes %>%
  group_by(AdjustedYear, Season) %>%
  summarize(TotalYearSeasonMedals = sum(!is.na(Medal)), .groups = 'drop')

top_country_per_year <- athletes %>%
  group_by(AdjustedYear, Season, AthleteCountry) %>%
  summarize(TotalMedals = sum(!is.na(Medal))) %>%
  group_by(AdjustedYear, Season) %>%
  filter(TotalMedals == max(TotalMedals)) %>%
  left_join(total_medals_per_year_season, by = c("AdjustedYear", "Season")) %>%
  mutate(Percentage = (TotalMedals / TotalYearSeasonMedals)) %>%
  arrange(desc(AdjustedYear))
# USA and Canada have been dominating the last years. Canada in the winter games and USA in both winter and summer games.
# Russia also had its golden ages from 1956-92

# Q10: Is there a Country that performs better in a specific sport?
# Look for a better way to answer this question
medals_per_sport_tot <- athletes %>% group_by(Sport) %>%
  summarize(Total_Medals = sum(!is.na(Medal)), times=n_distinct(AdjustedYear))
medals_per_sport_tot

top_country_per_sport <- athletes %>%
  group_by(AdjustedYear, AthleteCountry, Sport) %>%
  summarize(Total_Medals = sum(!is.na(Medal))) %>%
  group_by(AthleteCountry, Sport) %>%
  summarize(Avg_Total_Medals = mean(Total_Medals), Total_Medals_Country = sum(Total_Medals)) %>%
  left_join(medals_per_sport_tot) %>%
  mutate(Percent_Medals = Total_Medals_Country/Total_Medals) %>%
  group_by(Sport) %>%
  filter(Avg_Total_Medals == max(Avg_Total_Medals)) %>% arrange(desc(Total_Medals))
# USA has 35% of all the swimming olympic medals

merged_data %>% filter(Sport=='Luge') %>% group_by(Country, AdjustedYear) %>% summarize(n())

# athletes %>%
#   group_by(AdjustedYear,AthleteCountry) %>%
#   summarize(MedalOpportunities = n(),
#             Medals = sum(!is.na(Medal))) %>%
#   mutate(MedalRatio = Medals/MedalOpportunities)

# Q11: Is there a significant advantage for countries playing at home vs away?
# Normalize medals to account for variation in total medals each year
games %>% group_by(Country) %>% summarise(Times=n_distinct(AdjustedYear)) %>% arrange(desc(Times))
country = 'France'
host_countries_lst <- unique(games$Country)
merged_data <- merge(athletes, games, by = "AdjustedYear") %>%
  mutate(
    HomeAway = ifelse(Country == AthleteCountry, "Home", "Away")) %>%
  filter(Country %in% host_countries_lst)

medals_per_country <- athletes %>%
  group_by(AdjustedYear, AthleteCountry) %>%
  summarise(Medals = sum(!is.na(Medal)))

hosting_years <- merged_data %>%
  filter(HomeAway=='Home' & Country==country & Season=='Summer') %>%
  pull(AdjustedYear) %>%
  unique()

plot_data <- athletes %>% 
  group_by(AdjustedYear) %>% 
  summarize(TotalMedals = sum(!is.na(Medal))) %>%
  left_join(medals_per_country, by='AdjustedYear') %>%
  mutate(MedalsPercent = Medals/TotalMedals) %>%
  filter(AthleteCountry %in% c(country))

country_medals <- plot_data %>% filter(AdjustedYear %in% hosting_years)

ggplot(data = plot_data, aes(x = AdjustedYear)) +
  geom_bar(data = country_medals, aes(y = MedalsPercent), stat = "identity", width = 4, fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = MedalsPercent)) +
  labs(title = "Percentage of Medals Won Over Time",
       x = "Year",
       y = "Percentage of Medals") +
  theme_minimal()

# Filter only countries that have hosted at least once
host_countries_lst <- unique(games$Country)

merged_data <- merge(athletes, games, by = "AdjustedYear") %>%
  mutate(
    HomeAway = ifelse(Country == AthleteCountry, "Home", "Away")) %>%
  filter(AthleteCountry %in% host_countries_lst)

total_medals_per_year <- athletes %>%
  group_by(AdjustedYear) %>%
  summarize(TotalMedals = sum(!is.na(Medal)), .groups = 'drop')

merged_data_reshape <- merged_data %>%
  group_by(AdjustedYear, AthleteCountry, HomeAway) %>%
  summarize(Medals = sum(!is.na(Medal)), .groups = 'drop') %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = Medals / TotalMedals) %>%
  group_by(AthleteCountry, HomeAway) %>%
  summarise(
    WinsAvg = mean(NormalizedMedals)
  )

merged_data_reshape <- merged_data %>%
  group_by(AdjustedYear, AthleteCountry, HomeAway) %>%
  summarize(Medals = sum(!is.na(Medal)), .groups = 'drop') %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = Medals / TotalMedals) %>%
  group_by(HomeAway) %>%
  summarise(
    WinsAvg = mean(NormalizedMedals)
  )
successes =  6842
trials = 24776 + 6842
p0 = 53664/(260764 + 53664)
binom.test(6842, trials, p0, alternative = "greater")

country = 'France'
merged_data %>%
  group_by(AdjustedYear, AthleteCountry, HomeAway) %>%
  summarize(Medals = sum(!is.na(Medal)), .groups = 'drop') %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = Medals / TotalMedals) %>%
  filter(AthleteCountry==country) %>%
  group_by(AthleteCountry, HomeAway) %>%
  summarise(
    WinsAvg = mean(NormalizedMedals)
  )
merged_data %>% filter(Country==country) %>% summarise(sum(!is.na(Medal)))
merged_data %>% filter(Country==country & AthleteCountry==country) %>% summarise(sum(!is.na(Medal)))
binom.test(591, 6145, p = 0.0654, alternative = "greater")

merged_data_reshape
# Ho: The probability of winning is the same whether the country is hosting or not.
# Ha: The probability of winning is higher when the country is hosting.
t.test(WinsAvg~HomeAway, data=merged_data_reshape)
# Unconclusive

# Q12: Is the number of medals per country correlated to Population of the country?
# Sum medals per country and merge to pop dataset
# Using avg for all years
athletes_pop <- athletes %>%
  group_by(AthleteCountry, AdjustedYear) %>%
  summarise(
    SumMedals=sum(!is.na(Medal))
  ) %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = SumMedals / TotalMedals) %>%
  group_by(AthleteCountry) %>%
  summarise(AvgMedals = mean(NormalizedMedals)) %>%
  left_join(countries_pop, by=c('AthleteCountry'='CountryName'))
# Using only last olympics
athletes_pop <- athletes %>% filter(AdjustedYear==2016) %>%
  group_by(AthleteCountry, AdjustedYear) %>%
  summarise(
    SumMedals=sum(!is.na(Medal))
  ) %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = SumMedals / TotalMedals) %>%
  left_join(countries_pop, by=c('AthleteCountry'='CountryName'))

# Plot the data
athletes_pop %>%
ggplot(aes(x = Population, y = SumMedals, label = AthleteCountry)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 1.5, size = 3, color = "blue") +
  labs(title = "Population vs. Number of Medals (Year 2016)",
       x = "Population (in millions)",
       y = "Number of Medals") +
  theme_minimal()

# Calculate correlation 0.1538356
cor(athletes_pop$Population, athletes_pop$SumMedals, use = "complete.obs")
# Perform linear regression
model <- lm(AvgMedals ~ Population, data=athletes_pop)
summary(model)
# Add regression line to the plot
abline(model, col="red")

# Q13: Is the number of medals per country correlated to GDP of the country?
# Sum medals per country and merge with gdp dataset
# Average for all years
athletes_gdp <- athletes %>%
  group_by(AthleteCountry, AdjustedYear) %>%
  summarise(
    SumMedals=sum(!is.na(Medal))
  ) %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = SumMedals / TotalMedals) %>%
  group_by(AthleteCountry) %>%
  summarise(AvgMedals = mean(NormalizedMedals)) %>%
  left_join(countries_gdp, by=c('AthleteCountry'='CountryName'))

athletes_gdp <- athletes %>% filter(AdjustedYear == 2016) %>%
  group_by(AthleteCountry, AdjustedYear) %>%
  summarise(
    SumMedals=sum(!is.na(Medal))
  ) %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = SumMedals / TotalMedals) %>%
  left_join(countries_gdp, by=c('AthleteCountry'='CountryName'))
# Plot the data
athletes_gdp %>%
  ggplot(aes(x = GDP/1000, y = SumMedals, label = AthleteCountry)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 1.5, size = 3, color = "blue") +
  labs(title = "GDP vs. Number of Medals (Year 2016)",
       x = "GDP per capita (K$)",
       y = "Number of Medals") +
  theme_minimal()
# Calculate correlation 0.1960307 (0.23495 for gdp per capita)
cor(athletes_gdp$GDP, athletes_gdp$SumMedals, use = "complete.obs")
model <- lm(AvgMedals ~ GDP, data=athletes_gdp)
summary(model)
# Add regression line to the plot
abline(model, col="red")














# MDOEL ------------------------------------------------

athletes_pop_gdp <- merged_data %>%
  group_by(AthleteCountry, AdjustedYear, HomeAway) %>%
  summarise(
    SumMedals=sum(!is.na(Medal))
  ) %>%
  left_join(total_medals_per_year, by = "AdjustedYear") %>%
  mutate(NormalizedMedals = SumMedals / TotalMedals) %>%
  left_join(countries_pop, by=c('AthleteCountry'='CountryName')) %>%
  left_join(countries_gdp, by=c('AthleteCountry'='CountryName')) %>%
  mutate(IsHome = ifelse(HomeAway=='Home',1,0))
  
model <- lm(NormalizedMedals ~ GDP+Population+IsHome, data=athletes_pop_gdp)
summary(model)
# Add regression line to the plot
abline(model, col="red")
