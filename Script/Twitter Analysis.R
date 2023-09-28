install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("scales")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)
getwd()
setwd("C:/Users/Djamila Heß/OneDrive - Friedrich-Schiller-Universität Jena/Dissertation/Studie 1/Twitter Abfrage/R Twitter/")
load("Data//joint_twitter.RData")
twitter_all_languages <- joint_twitter
rm(joint_twitter)

# change date format
twitter_all_languages <- twitter_all_languages %>%
  mutate(created_at = ymd_hms(created_at))

################################################################################
### first results before cleaning ###

# overview
nrow(twitter_all_languages)
  # number of tweets: 929961

table(twitter_all_languages$source)
  # CJ: 821581    JC: 57972       KG: 50408  


#### language differences ####

# per year
tweets_summary <- twitter_all_languages %>%
  mutate(year = lubridate::year(created_at)) %>%
  group_by(source, year) %>%
  summarize(number_of_tweets = n()) %>%
  ungroup()

ggplot(tweets_summary, aes(x = year, y = number_of_tweets, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Number of Tweets", title = "Tweets Summary per Year and Source") +
  scale_fill_manual(values = c("KG" = "darkcyan", "JC" = "darkgoldenrod1", "CJ" = "darkolivegreen1")) +
  theme_minimal()

# per month
# 1. show all three languages in one figure
tweets_summary1 <- twitter_all_languages %>%
  mutate(month = floor_date(created_at, "month")) %>%
  group_by(source, month) %>%
  summarize(number_of_tweets = n()) %>%
  ungroup()

ggplot(tweets_summary1, aes(x = as.Date(month), y = number_of_tweets, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Number of Tweets", title = "Tweets per Language") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  scale_fill_manual(values = c("KG" = "steelblue2", "JC" = "orangered", "CJ" = "seagreen3")) +
  theme_minimal()

# 2. one figure per language

tweets_summary1 <- twitter_all_languages %>%
  mutate(month = floor_date(created_at, "month")) %>%
  group_by(source, month) %>%
  summarize(number_of_tweets = n()) %>%
  ungroup()

# Plot for "Klimagerechtigkeit"
ggplot(filter(tweets_summary1, source == "KG"), aes(x = month, y = number_of_tweets)) +
  geom_bar(stat = "identity", position = "dodge", fill = "darkcyan") +
  labs(x = "Month", y = "Number of Tweets", title = "Tweets for 'Klimagerechtigkeit'") +
  theme_minimal()


# Plot for "Climate Justice"
ggplot(filter(tweets_summary1, source == "CJ"), aes(x = month, y = number_of_tweets)) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
  labs(x = "Month", y = "Number of Tweets", title = "Tweets for 'Climate Justice'") +
  theme_minimal()

# Plot for "Justicia climática"
ggplot(filter(tweets_summary1, source == "JC"), aes(x = month, y = number_of_tweets)) +
  geom_bar(stat = "identity", position = "dodge", fill = "grey") +
  labs(x = "Month", y = "Number of Tweets", title = "Tweets for 'Justicia climática'") +
  theme_minimal()


################################################################################
### Remove duplicates (language based) ###
################################################################################

twitter <- twitter_all_languages %>%
  distinct(id, .keep_all = TRUE)

################################################################################
#### Transformations ###
################################################################################

#### group per day ####
# create day variable
twitter <- twitter %>%
  mutate(day = format(created_at, "%Y-%m-%d"))

tweets_day <- twitter %>%
  group_by(day) %>%
  summarise(number_of_tweets = n())

#### group per month ####
# create month variable
twitter <- twitter %>%
  mutate(year_month = format(created_at, "%Y-%m"))

tweets_month <- twitter %>%
  group_by(year_month) %>%
  summarise(number_of_tweets = n())

#### group per year ####
# create year variable
twitter <- twitter %>%
  mutate(year = format(created_at, "%Y"))

tweets_year <- twitter %>%
  group_by(year) %>%
  summarise(number_of_tweets = n())



################################################################################
### first results after cleaning ###

# overview
nrow(twitter)
  # number of tweets: 927685

table(twitter$source)
  # CJ: 821567    JC: 57456       KG: 48662 

summary(tweets_day)
  # max of tweets per day: 5865, 2021-11-06

################################################################################
### Visualizations ###

#### number of tweets by month ####
# Nr.1 complete (too long)
ggplot(tweets_month, aes(x = as.Date(paste0(year_month, "-01")), y = number_of_tweets)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Month", y = "Number of Tweets", title = "Number of Tweets by Month") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# nr.2 two different visualizations
tweets_month$year_month <- as.Date(paste0(tweets_month$year_month, "-01"))
tweets_month_1 <- subset(tweets_month, as.Date(paste0(year_month, "-01")) <= as.Date("2014-12-31"))
tweets_month_2 <- subset(tweets_month, as.Date(paste0(year_month, "-01")) >= as.Date("2015-01-01"))

ggplot(tweets_month_1, aes(x = as.Date(paste0(year_month, "-01")), y = number_of_tweets)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Month", y = "Number of Tweets", title = "Number of Tweets by Month") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 50000)  # Set y-axis limit to 50,000

ggplot(tweets_month_2, aes(x = as.Date(paste0(year_month, "-01")), y = number_of_tweets)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Month", y = "Number of Tweets", title = "Number of Tweets by Month") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0, 50000)  # Set y-axis limit to 50,000



#### Number of tweets by year ####
tweets_month$year_month <- as.Date(paste(tweets_month$year_month, "01", sep = "-"))

ggplot(tweets_month, aes(x = year_month, y = number_of_tweets)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Year", y = "Number of Tweets", title = "Number of Tweets per Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma)


####number of tweets by month including specific dates ####
# Specify the specific dates for adding lines
specific_dates <- c("2022-11-06", "2011-11-28", "2010-11-29","2015-11-30","2015-09-25",
                    "2019-09-21","2021-10-31","2021-04-22", "2019-12-02", "2018-12-02",
                    "2017-11-06", "2016-11-07", "2014-12-01", "2013-11-11", "2012-11-26", 
                    "2009-12-07","2008-12-01", "2007-12-03", "2006-11-06","2018-09-12")

labels <- c("COP 27", "COP 17", "COP 16", "COP 21", "SGD Summit", "UN Climate Action Summit", 
            "COP 26", "Leaders' Summit on Climate", "COP 25", "COP 24", "COP 23", 
            "COP 22", "COP 20", "COP 19", "COP 18", "COP 15", "COP 14", "COP 13", "COP 12",
            "Global Climate Action Summit")

# Convert specific dates to date format
specific_dates <- as.Date(specific_dates)

# Create a data frame for specific dates and labels
labels_df <- data.frame(date = specific_dates, label = labels)

# Plotting the data
ggplot(tweets_month, aes(x = year_month, y = number_of_tweets)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Year", y = "Number of Tweets", title = "Number of Tweets per Month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_breaks = "1 year", 
               limits = as.Date(c("2008",NA),format="%Y"),
               date_labels="%Y") +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(data = labels_df, aes(xintercept = as.numeric(date)), color = "grey50", linetype = "dotted") +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), vjust = -0.25, hjust = 1, size = 1.8, 
            color = "grey50", angle = 90,position = position_nudge(y=48000))+
  theme_classic()

#### number of tweets by year ####
ggplot(tweets_year, aes(x = year, y = number_of_tweets)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Year", y = "Number of Tweets", title = "Number of Tweets per Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels = scales::comma)+
  theme_classic()


################################################################################
#### test for regions and languages in the CJ dataframe####
  # conclusion: test for regions not feasable

# country_codes
# create frequency table with country codes
frequency_country_codes <- twitter %>%
  count(country_codes, name = "count_country")
  # but 929797 tweets without country codes
  # 96 IN (India), 35 TR (Turkey)

# language 
# create frequency table with country codes
frequency_language <- twitter %>%
  count(lang, name = "count_lang")
  # 1185874 out of 1301192 tweets in English 
  # 18779 de, 5873 frz, 5378 es, 3737 ja, 2883 nl, 2081 it, 
  # 1691 tl (Tagalog, phillippines), 1434 pt, 1314 arabic, 1262 in (Indonesian)

# coordinates
frequency_coordinates <- twitter %>%
  count(coordinates, name = "count_coordinates")
  #  1298298 out of 1301192 tweets without coordinates

# place_id
frequency_place_id <- twitter %>%
  count(place_id, name = "count_place_id")
  #  1263332 out of 1301192 tweets without place_id

rm(frequency_coordinates)
rm(frequency_country_codes)
rm(frequency_language)
rm(frequency_place_id)


#### test for heavy users (author_id) ####
frequency_author_id <- twitter %>%
  count(author_id, name = "count_author_id")
#  teilweise sehr viele tweets mit enthalten, z.B. 10690 für id 35255668 (Tweets scheinen okay zu sein, viele mentions and hashtags, Account vermutlich gesperrt)
  # 8455 für id 85090936 (account vermutlich gesperrt)
  # 4414 für id 255797635
  # 4141 für ID 1459689445117075463 (Fehler: keine tweets im Datensatz gefunden)
  # 3513 für ID 2338275336

author_tweets <- subset(twitter, author_id == 2338275336)

# welche Rolle spielt Anstieg des Umfangs von Tweets? Lässt sich das irgendwie berücksichtigen?


