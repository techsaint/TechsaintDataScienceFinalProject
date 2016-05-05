library(readr)
library(dplyr)
library(rjson)
library(stringr)
library(tidyr)

keyword_data <- read_csv("https://docs.google.com/spreadsheets/d/1VVIprSPTFlmTAafUaPvjHQKJvA5nJM9dBExCd096cmw/pub?gid=0&single=true&output=csv")
keyword_update <- keyword_data
keyword_update$county_key <- str_replace_all(keyword_update$county_key, "new york", "new york,")
keyword_update <- keyword_update %>% mutate( county_key =  ifelse(!str_detect(county_key,","),str_replace_all(county_key," ", ","), county_key))
keyword_update <- keyword_update %>% 
  mutate( console_type =  ifelse(!str_detect(keyword,"x"),"Playstation", "Xbox")) 
keyword_update <- keyword_update %>%  separate(county_key, into = c("state", "county_name"), sep = "\\,") 
keyword_update <- keyword_update %>%  mutate(county_name = str_trim(  str_replace_all(county_name, ",", " ")))




counties_pop <- read_csv("http://api.datausa.io/api/csv/?show=geo&sumlevel=county&year=latest&required=pop")
counties_pop <- counties_pop %>%  separate(geo_name, into = c("county_old", "state"), sep = "\\,") 
counties_pop <- counties_pop %>% mutate(state = str_trim(state))


#counties_pop <- fromJSON(file="http://api.datausa.io/api/?show=geo&sumlevel=county&year=latest&required=pop")

state_abbr <- read_csv("https://docs.google.com/spreadsheets/d/1Utgyxd_nBvaaQM3wczd4zyA2B5RrhAhxgKXxbzuavSo/pub?gid=0&single=true&output=csv")
state_abbr <- state_abbr %>% mutate(state=str_to_lower(State), Abbreviation=str_to_lower(Abbreviation))
keyword_update <- keyword_update %>%  left_join(state_abbr)

geoid_key <- read_csv("https://docs.google.com/spreadsheets/d/1hENyGRj4nSXYJkhDipAw-BOMGBrcAQK8FT_kewEI818/pub?gid=0&single=true&output=csv")
#counties_pop <- counties_pop %>% left_join(geoid_key, by=c("geo" ="GEO_ID"))
counties_pop <- counties_pop %>% separate(geo, into= c("geo_prefix","geo_num"), sep="US") %>% mutate(geo_num = as.numeric(geo_num))
geoid_key <- geoid_key %>% mutate(GEO_ID2 = as.numeric(GEO_ID2 ))
counties_pop <- geoid_key %>% left_join(counties_pop, by=c("GEO_ID2"="geo_num"))

counties_pop <- counties_pop %>% mutate(county_name = str_to_lower(`County Name`), state=str_to_lower(state))




#keyword_update <- keyword_update %>%  left_join(state_abbr)
keyword_update <- keyword_update %>% left_join(counties_pop, by = c("Abbreviation"="state abbr", "county_name" = "county_name"))


keywords_combined <- keyword_update %>% 
  group_by(State,county_name,console_type, pop) %>% 
  summarize(total_searches = sum(average))


keyword_totals <- keyword_update %>% 
  group_by(State,county_name,console_type, pop) %>% 
  summarize(total_searches = sum(average)) %>% filter(!is.na(pop))

testp <- keyword_totals %>% filter(console_type == "Playstation")
testx <- keyword_totals %>% filter(console_type == "Xbox")



plot(testp$pop, testp$total_searches, col="red",pch=15)
points(testx$pop, testx$total_searches, col="green", pch=16)
myline <- lm(testx$total_searches ~ testx$pop)
summary(myline)
abline(myline)
