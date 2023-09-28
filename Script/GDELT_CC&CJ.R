#### Setup ####
install.packages("rworldmap")
install.packages("countrycode")
install.packages("knitr")
install.packages("kableExtra")
install.packages("gridExtra")   
install.packages("flextable")
install.packages("tidyverse")
install.packages("RColorBrewer")

library(RColorBrewer)
library(tidyverse)
library(flextable)
library(gridExtra)
library(kableExtra)
library(knitr)
library(countrycode)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(jsonlite)
library(rworldmap)


setwd("C:/Users/Djamila Heß/OneDrive - Friedrich-Schiller-Universität Jena/Dissertation/Studie 1/")

# import CJ data .csv
CJcountries <- read.csv("GDELT 2/Data/Timeline/countries_31.05.2023.csv")
  #dataframe with all countries and articles per day

# import CC data
CCcountries <- read.csv("GDELT 3/Data/Timeline/countries_CC_20.06.2023.csv")

#### add ISO 2 country code and country name ####
countrycodes <- read.csv("GDELT 2/Data/countrycodes_19.06.2023.csv", header = TRUE, sep = ";", na.strings = "")
countrycodes <- select(countrycodes, FIPS, country.name, ISO)
countrycodes <- rename(countrycodes, country = FIPS)

# Merge the dataframes based on the 'country' variable
CJcountries <- merge(CJcountries, countrycodes[c("country", "ISO", "country.name")], by = "country", all.x = TRUE)
CCcountries <- merge(CCcountries, countrycodes[c("country", "ISO", "country.name")], by = "country", all.x = TRUE)

#check for total of missing values
missing_count <- sum(is.na(CJcountries$ISO2))
missing_count <- sum(is.na(CCcountries$ISO2))
  #0 missing values

rm(missing_count)
CJcountries <- rename(CJcountries, FIPS = country)
CCcountries <- rename(CCcountries, FIPS = country)
CCcountries <- rename(CCcountries, articlecount_CC = articlecount_CJ)

################################################################################

#### Aggregated data ####

# calculate volume(only when articlecount_AA != 0)
CJcountries <- mutate(CJcountries, vol = ifelse(articlecount_AA != 0, (articlecount_CJ/articlecount_AA) * 100, NA))

### Aggregated data countries and months: months_c_CJ ###

# add a months variable
CJcountries$datetime <- as.Date(CJcountries$datetime)
CJcountries$month <- format(CJcountries$datetime, "%Y-%m")

# Group the data by month and country, and calculate the sums
months_c_CJ <- CJcountries %>%
  group_by(month, ISO) %>%
  summarise(
    articlecount_CJ_month = sum(articlecount_CJ),
    articlecount_AA_month = sum(articlecount_AA),
    All.Articles_month = sum(All.Articles)
  ) %>%
  ungroup()

# add volume
months_c_CJ <- mutate(months_c_CJ, vol_month = ifelse(articlecount_AA_month != 0, (articlecount_CJ_month/articlecount_AA_month)*100, NA))


### Aggregated data countries and days: days_c_CJ ###

# add a days variable
CJcountries$day <- format(CJcountries$datetime, "%Y-%m-%d")

# Group the data by day and country, and calculate the sums
days_c_CJ <- CJcountries %>%
  group_by(datetime, ISO) %>%
  summarise(
    articlecount_CJ_day = sum(articlecount_CJ),
    articlecount_AA_day = sum(articlecount_AA),
    All.Articles_day = sum(All.Articles)) %>%
    ungroup()

# add volume
#days_c_CJ <- mutate(days_c_CJ, vol_day = ifelse(articlecount_AA_day != 0, (articlecount_CJ_day/articlecount_AA_day)*100, NA))

### Aggregated data countries and years: years_c_CJ ###

# add a years variable
CJcountries$year <- format(CJcountries$datetime, "%Y")

# Group the data by year and country, and calculate the sums
years_c_CJ <- CJcountries %>%
  group_by(year, ISO) %>%
  summarise(
    articlecount_CJ_year = sum(articlecount_CJ),
    articlecount_AA_year = sum(articlecount_AA),
    All.Articles_year = sum(All.Articles)
  ) %>%
  ungroup()

# add volume
years_c_CJ <- mutate(years_c_CJ, vol_year = ifelse(articlecount_AA_year != 0, (articlecount_CJ_year/articlecount_AA_year)*100, NA))

### aggregated data countries: country_CJ ###
country_CJ <- CJcountries %>%
  group_by(ISO) %>%
  summarise(
    articlecount_CJ_total = sum(articlecount_CJ),
    articlecount_AA_total = sum(articlecount_AA),
    All.Articles_total = sum(All.Articles)
  ) %>%
  ungroup()

# add volume
country_CJ <- mutate(country_CJ, vol_total = ifelse(articlecount_AA_total != 0,  (articlecount_CJ_total/articlecount_AA_total)*100, NA))

### aggregated data: years_CJ ###
years_CJ <- CJcountries %>% 
  group_by (year) %>% 
  summarise(
    articlecount_CJ_year = sum(articlecount_CJ),
    articlecount_AA_year = sum(articlecount_AA),
    All.Articles_year = sum(All.Articles)
  ) %>%
  ungroup()

# add volume
years_CJ <- mutate(years_CJ, vol_year = ifelse(articlecount_AA_year != 0, (articlecount_CJ_year/articlecount_AA_year)*100, NA))

### aggregate months_CJ ###
months_CJ <- CJcountries %>%
  group_by(month) %>%
  summarise(
    articlecount_CJ_month = sum(articlecount_CJ),
    articlecount_AA_month = sum(articlecount_AA),
    All.Articles_month = sum(All.Articles)
  ) %>%
  ungroup()


# add volume
months_CJ <- mutate(months_CJ, vol_month = ifelse(articlecount_AA_month != 0,(articlecount_CJ_month/articlecount_AA_month)*100, NA))

#remove the day,month and year column from the dataframe
#CJcountries <- CJcountries %>%
  #select(-c(year, month, day))

#### basic results ####
# mean of CJ articles
mean(country_CJ$vol_total) 
  # 0.01372534
(sum(CJcountries$articlecount_CJ)/sum(CJcountries$articlecount_AA))*100
  # 0.009932838%
# mean of CC articles
(sum(CCcountries$articlecount_CC)/sum(CCcountries$articlecount_AA))*100
  # 1.105703%

# sum of all CJ and CC articles
sum(CJcountries$articlecount_CJ) 
  # 92.155
sum(CCcountries$articlecount_CC) 
  # 10.353.084

################################################################################
#### Visualization ####

#### plot Comparison of CC and CJ ####
# aggregate CC by months
CCcountries$datetime <- as.Date(CCcountries$datetime)
CCcountries$month <- format(CCcountries$datetime, "%Y-%m")

months_CC <- CCcountries %>%
  group_by(month) %>%
  summarise(
    articlecount_CC_month = sum(articlecount_CC),
    articlecount_AA_month = sum(articlecount_AA),
    All.Articles_month = sum(All.Articles)
  ) %>%
  ungroup()

months_CC <- mutate(months_CC, vol_month_CC = ifelse(articlecount_AA_month != 0,(articlecount_CC_month/articlecount_AA_month)*100, NA))

#merge CC and CJ columns

comparison <- left_join(select(months_CC, month, vol_month_CC),
                       select(months_CJ, month, vol_month),
                       by = "month")

ggplot(comparison, aes(x = month, group = 1)) +
  geom_line(aes(y = vol_month_CC), color ="blue") +
  geom_line(aes(y = vol_month), color = "red") +
  ggtitle("Volume of climate justice and climate change coverage by month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
  labs(x = "Year-Month",
       y = "Volume in %") 

# Vergleich CC und CJ mit NOrmalisierung der Daten -> zeigt Relation
# Normalisierung der Daten
comparison2 <- comparison %>%
  mutate(vol_month_CC_norm = scale(vol_month_CC),
         vol_month_norm = scale(vol_month))
ggplot(comparison2, aes(x = month, group = 1)) +
  geom_line(aes(y = vol_month_CC_norm), color = "blue") +
  geom_line(aes(y = vol_month_norm), color = "red") +
  ggtitle("Normalized Volume of Climate Justice and Climate Change Coverage by Month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
  labs(x = "Year-Month", y = "Normalized Volume")+
  #scale_color_manual(values = c(treat = "blue", control = "green"),
                     #labels = c(treat = "Treatment", control = "Control"),

# MAL NOCH LABEL DRANMACHEN UND MONATE/JAHR BESSER DARSTELLEN

#### table country volume ####
# crate table of countries including article counts, total counts and volume
country_CJ <- country_CJ[order(country_CJ$vol_total, decreasing = TRUE), ]
  # Sort the data frame in descending order

  # Create the flextable with the sorted data (funktion nicht gefunden!)
table_country_CJ <- flextable(
  country_CJ,
  col_keys = names(country_CJ),
  cwidth = 0.75,
  cheight = 0.25,
  defaults = list(),
  theme_fun = theme_booktabs
)

# save data
#save_as_docx(table_country_CJ, path = "GDELT Abfrage/Plotting/table_countries_CJ.docx")

#### plot bar chart years ####
# plot years (excluding 2023)
ggplot(subset(years_CJ, year != 2023), aes(x = factor(year), y = vol_year)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Year", y = "Volume of Articles", title = "CJ Article Volume per Year") +  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

#### plot bar chart months (excluding 06/2023) ####
ggplot(subset (months_CJ, month != 2023-06), aes(x = month, y = vol_month)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Year and Month", y = "Volume of Articles", title = "CJ Article Volume by Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_vline(xintercept = c(12.5, 24.5, 36.5, 48.5, 60.5, 72.5), color = "grey50", linetype = "dashed")
  # excluding 05/2023 did not work

### plot months including specific dates ###
specific_dates <- c("2022-11-06","2019-09-21","2021-10-31","2021-04-22", "2019-12-02", "2018-12-02", 
                    "2017-11-06")
labels <- c("COP 27", "Climate Action Summit", "COP 26", "Leaders' Summit on Climate", "COP 25", "COP 24", "COP 23")

# Convert specific dates to date format
specific_dates <- as.Date(specific_dates)

# Create a data frame for specific dates and labels
labels_df <- data.frame(date = specific_dates, label = labels)

Months_CJ <- months_CJ
Months_CJ$month <- as.Date(paste(months_CJ$month, "01", sep = "-"))


ggplot(Months_CJ, aes(x = month, y = vol_month)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Climate Justice Articles and Climate Conferences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept = as.numeric(specific_dates), linetype = "dashed", color = "grey50") +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
             angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))


#### plot complete per country ####
ggplot(country_CJ, aes(x = factor(ISO), y = vol_total)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Year", y = "Volume of Articles", title = "Article Volume by Countries") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### plot complete 10 countries with highest scores ####
country_CJ <- merge(country_CJ, countrycodes[c("ISO", "country.name")], by = "ISO", all.x = TRUE)
  #add country.names zu country_CJ
country_CJ %>%
  arrange(desc(vol_total)) %>%
  slice(1:15) %>%
  ggplot(aes(x = country.name, y = vol_total)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Country", y = "Volume of Articles", title = "Top 15 Countries with Highest Volume of CJ Articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot complete 2022
country_2022 <- subset(years_c_CJ, year == 2022)
ggplot(country_2022, aes(x = factor(ISO), y = vol_year)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Year", y = "Volume of Articles", title = "Article Volume 2022 by Countries") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # Warning message: Removed 1 rows containing missing values (`position_stack()`). 

#### subset dataframes of all countries per year ####
#add country.names zu years_c_CJ
years_c_CJ <- merge(years_c_CJ, countrycodes[c("ISO", "country.name")], by = "ISO", all.x = TRUE)
#2017
country_2017 <- subset(years_c_CJ, year == 2017)

#2018
country_2018 <- subset(years_c_CJ, year == 2018)

#2019
country_2019 <- subset(years_c_CJ, year == 2019)

#2020
country_2020 <- subset(years_c_CJ, year == 2020)

#2021
country_2021 <- subset(years_c_CJ, year == 2021)

#2022
country_2022 <- subset(years_c_CJ, year == 2022)

#2023
country_2023 <- subset(years_c_CJ, year == 2023)


#### map word total ####

# prepare data for plotting
map_world_total <- joinCountryData2Map(country_CJ
                            , joinCode = "ISO2"
                            , nameJoinColumn = "ISO",
                            verbose = TRUE)
  #184 codes from your data successfully matched countries in the map
  #4 codes from your data failed to match with a country code in the map: "GI" Gibraltar, "RE" Reunion, "XK" Kosovo, NA 
  # -> countries that failed to match are very small (GI, RE) and can possibly not be displayed or are not existing anymore (XK) 
  #57 codes from the map weren't represented in your data


# plot country total data I -> einteilung in 7 Abstände, berechnet anhand der Einstellung Quantil
# including legend adjustment
colorpalette1 <- brewer.pal(7, "YlGnBu") # set color palette

mapCountryData <- mapCountryData(map_world_total, 
                                  nameColumnToPlot="vol_total",
                                  catMethod = "quantiles",
                                  mapTitle = '"Climate Justice" Articles 2017-2023',
                                 colourPalette = colorpalette1,
                                  addLegend=FALSE )
do.call( addMapLegend, c(mapCountryData, legendWidth=0.5, legendMar = 2))


# plot country total data II -> einteilung in 8 gleichgroße Abstände, berechnet anhand des maximalen Volumes für alle Länder (total)
colorpalette2 <- brewer.pal(8, "YlGnBu") #set color palette

mapCountryData(map_world_total, 
               nameColumnToPlot = "vol_total",
               catMethod = c(0, 0.026125, 0.05225, 0.078375, 0.1045, 0.130625, 0.15675, 0.182875, 0.209),
               mapTitle = '"Climate Justice" Articles 2017-2023',
               colourPalette = colorpalette2,
               addLegend=TRUE )
  # macht keinen richtigen Sinn, weil kaum Färbung



# plot country total data III median -> Einteilung mit Median
#medianberechnung mit Gesamtzahlen
country_CJ %>%
  summarize(median_volume = median(vol_total))
  # median + 3/4 median -> open end: >= 0.010115
  # median + 2/4 median:0.00867
  # median + 1/4 median:0.007225
  # median: 0.00578
  # 3/4 median: 0.004335
  # 1/2median: 0.00289
  # 1/4median:  0.001445

colorpalette2 <- brewer.pal(7, "YlGnBu") #set color palette

mapCountryData(map_world_total, 
               nameColumnToPlot = "vol_total",
               catMethod = c(0,0.001445, 0.00289, 0.004335, 0.00578, 0.007225, 0.00867, 0.010115, 3),
               mapTitle = '"Climate Justice" Articles 2017-2023',
               colourPalette = colorpalette2,
               addLegend=TRUE )


#medianberechnung mit 2022-Werten
years_c_CJ %>%
  filter(year == "2022") %>%
  summarise(median_volume = median(vol_year, na.rm = TRUE))
# median + 3/4 median: 0.0174825
# median + 2/4 median:0.014985
# median + 1/4 median: 0.0124875
# median: 0.00999
# 3/4 median: 0.0074925
# 1/2median: 0.004995
# 1/4median:  0.0024975

colorpalette2 <- brewer.pal(7, "YlGnBu") #set color palette

mapCountryData(map_world_total, 
               nameColumnToPlot = "vol_total",
               catMethod = c(0,0.0024975, 0.004995, 0.0074925, 0.00999, 0.0124875, 0.014985, 0.0174825, 1.26),
               mapTitle = '"Climate Justice" Articles 2017-2023',
               colourPalette = colorpalette2,
               addLegend=TRUE )




# top 15 countries
country_CJ %>%
  arrange(desc(vol_total)) %>%
  slice(1:15) %>%
  mutate(country.name = factor(country.name, levels = country.name)) %>%
  ggplot(aes(x = country.name, y = vol_total)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Country", y = "Volume of Articles", title = "Top 15 Countries with Highest Volume of CJ Articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# including article count
country_CJ %>%
  arrange(desc(vol_total)) %>%
  slice(1:15) %>%
  mutate(country.name = factor(country.name, levels = country.name)) %>%
  ggplot(aes(x = country.name, y = vol_total)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  geom_text(aes(label = articlecount_CJ_total), vjust = -0.5, color = "black", size = 3) +  # Add text labels for number of cases
  labs(x = "Country", y = "Volume of Articles", title = "Top 15 Countries with Highest Volume of CJ Articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 25 countries
country_CJ %>%
  arrange(desc(vol_total)) %>%
  slice(25:50) %>%
  mutate(country.name = factor(country.name, levels = country.name)) %>%
  ggplot(aes(x = country.name, y = vol_total)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Country", y = "Volume of Articles", title = "Top 50 Countries with Highest Volume of CJ Articles") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# including article count
country_CJ %>%
  arrange(desc(vol_total)) %>%
  slice(25:50) %>%
  mutate(country.name = factor(country.name, levels = country.name)) %>%
  ggplot(aes(x = country.name, y = vol_total)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  geom_text(aes(label = articlecount_CJ_total), vjust = -0.5, color = "black", size = 3) +  # Add text labels for number of cases
  labs(x = "Country", y = "Volume of Articles", title = "Top 15 Countries with Highest Volume of CJ Articles") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#### map time series ####
#### 2017 ####

map_world_2017 <- joinCountryData2Map(country_2017
                                      , joinCode = "ISO2"
                                      , nameJoinColumn = "ISO",
                                      verbose = TRUE)

colorpalette2 <- brewer.pal(8, "YlGnBu") #set color palette

mapCountryData2017 <- mapCountryData(map_world_2017, 
                                     nameColumnToPlot="vol_year",
                                     catMethod = c(0,0.0024975, 0.004995, 0.0074925, 0.00999, 0.0124875, 0.014985, 0.0174825, 1.26),
                                     mapTitle = 'Volume of "Climate Justice" Articles 2017',
                                     colourPalette = colorpalette2,
                                     addLegend=FALSE )

####2018####
map_world_2018 <- joinCountryData2Map(country_2018
                                      , joinCode = "ISO2"
                                      , nameJoinColumn = "ISO",
                                      verbose = TRUE)

mapCountryData2018 <- mapCountryData(map_world_2018, 
                                     nameColumnToPlot="vol_year",
                                     catMethod = c(0,0.0024975, 0.004995, 0.0074925, 0.00999, 0.0124875, 0.014985, 0.0174825, 1.26),
                                     mapTitle = 'Volume of "Climate Justice" Articles 2018',
                                     colourPalette = colorpalette2,
                                     addLegend=FALSE )

#### 2019 ####
map_world_2019 <- joinCountryData2Map(country_2019
                                      , joinCode = "ISO2"
                                      , nameJoinColumn = "ISO",
                                      verbose = TRUE)

mapCountryData2019 <- mapCountryData(map_world_2019, 
                                     nameColumnToPlot="vol_year",
                                     catMethod = c(0,0.0024975, 0.004995, 0.0074925, 0.00999, 0.0124875, 0.014985, 0.0174825, 1.26),
                                     mapTitle = 'Volume of "Climate Justice" Articles 2019',
                                     colourPalette = colorpalette2,
                                     addLegend=FALSE )

#### 2020 ####
map_world_2020 <- joinCountryData2Map(country_2020
                                      , joinCode = "ISO2"
                                      , nameJoinColumn = "ISO",
                                      verbose = TRUE)

mapCountryData2020 <- mapCountryData(map_world_2020, 
                                     nameColumnToPlot="vol_year",
                                     catMethod = c(0,0.0024975, 0.004995, 0.0074925, 0.00999, 0.0124875, 0.014985, 0.0174825, 1.26),
                                     mapTitle = 'Volume of "Climate Justice" Articles 2020',
                                     colourPalette = colorpalette2,
                                     addLegend=FALSE )

#### 2021 ####
map_world_2021 <- joinCountryData2Map(country_2021
                                      , joinCode = "ISO2"
                                      , nameJoinColumn = "ISO",
                                      verbose = TRUE)

mapCountryData2021 <- mapCountryData(map_world_2021, 
                                     nameColumnToPlot="vol_year",
                                     catMethod = c(0,0.0024975, 0.004995, 0.0074925, 0.00999, 0.0124875, 0.014985, 0.0174825, 1.26),
                                     mapTitle = 'Volume of "Climate Justice" Articles 2021',
                                     colourPalette = colorpalette2,
                                     addLegend=FALSE )

####2022####
map_world_2022 <- joinCountryData2Map(country_2022
                                      , joinCode = "ISO2"
                                      , nameJoinColumn = "ISO",
                                      verbose = TRUE)

mapCountryData2022 <- mapCountryData(map_world_2022, 
                                     nameColumnToPlot="vol_year",
                                     catMethod = c(0,0.0024975, 0.004995, 0.0074925, 0.00999, 0.0124875, 0.014985, 0.0174825, 1.26),
                                     mapTitle = 'Volume of "Climate Justice" Articles 2022',
                                     colourPalette = colorpalette2,
                                     addLegend=FALSE )

# bar plot top 15 countries
country_2022 %>%
  arrange(desc(vol_year)) %>%
  slice(1:15) %>%
  ggplot(aes(x = country.name, y = vol_year)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Country", y = "Volume of Articles", title = "Top 15 Countries 2022") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### 2023 
map_world_2023 <- joinCountryData2Map(country_2023
                                      , joinCode = "ISO2"
                                      , nameJoinColumn = "ISO",
                                      verbose = TRUE)
  #185 codes from your data successfully matched countries in the map
  #4 codes from your data failed to match with a country code in the map: "GI" Gibraltar, "RE" Reunion, "XK" Kosovo, NA 
  # -> countries that failed to match are very small (GI, RE) and can possibly not be displayed or are not existing anymore (XK) 
  #57 codes from the map weren't represented in your data



mapCountryData2023 <- mapCountryData(map_world_2023, 
                                     nameColumnToPlot="vol_year",
                                     catMethod = c(0, 0.003875, 0.00775, 0.011625, 0.0155, 0.019375, 0.02325, 0.027125, 0.031, 3),
                                     mapTitle = 'Volume of "Climate Justice" Articles 2023',
                                     colourPalette = colorpalette1,
                                     addLegend=FALSE )

# bar plot top 15 countries
country_2023 %>%
  arrange(desc(vol_year)) %>%
  slice(1:15) %>%
  ggplot(aes(x = country.name, y = vol_year)) +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Country", y = "Volume of Articles", title = "Top 15 Countries 2023") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################################################################
#### ausgewählte Länder ####
### plot months including specific dates ###
specific_dates <- c("2022-11-06","2019-09-21","2021-10-31","2021-04-22", "2019-12-02", "2018-12-02", 
                    "2017-11-06")
labels <- c("COP 27", "Climate Action Summit", "COP 26", "Leaders' Summit on Climate", "COP 25", "COP 24", "COP 23")

# Convert specific dates to date format
specific_dates <- as.Date(specific_dates)

# Create a data frame for specific dates and labels
labels_df <- data.frame(date = specific_dates, label = labels)

Months_c_CJ <- months_c_CJ
Months_c_CJ$month <- as.Date(paste(months_c_CJ$month, "01", sep = "-"))

##????? Nina
Days_c_CJ <- days_c_CJ
Days_c_CJ$day <- as.Date(paste(days_c_CJ$day, "2017-01-01"))

#### US ####
us_data <- subset(Months_c_CJ, ISO == "US")
specific_datesUS <- c("2017-01-15", "2017-04-29","2022-04-21")
labels_US <- c("Announcement People's Climate March","People's Climate March","US Climate Action Summit")

# Convert specific dates to date format
specific_dates_US <- as.Date(specific_dates_US)

# Create a data frame for specific dates and labels
labels_dfUS <- data.frame(date = specific_dates_US, label = labels_US)



# Plot the filtered data
ggplot(us_data, aes(x = month, y = vol_month)) +
  geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_datesUS), linetype = "dotted", color = "grey50") +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in the US") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 0.5, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y = 0.1)) +
  geom_text(data = labels_dfUS, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 0.5, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y = 0.1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = - 0.75),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  theme_minimal()

#### Philippines ####
PH_data <- subset(Days_c_CJ, ISO == "PH")
specific_dates_PH <- c("2018-02-13","2020-11-10","2022-07-27")
labels_PH <- c("tropical cyclone","series of severe typhoons","earthquake")

# Convert specific dates to date format
specific_dates_PH <- as.Date(specific_dates_PH)

# Create a data frame for specific dates and labels
labels_dfPH <- data.frame(date = specific_dates_PH, label = labels_PH)

# Plot the filtered data
ggplot(PH_data, aes(x = day, y = vol_day)) +
  geom_vline(xintercept = as.numeric(specific_dates_PH), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Day", y = "Volume", title = "Volume of Climate Justice Articles in the Philippines") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_text(data = labels_dfPH, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2, 
            position = position_nudge(y=3)) +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = -0.25, hjust = 1, color = "grey50", size = 2, 
            position = position_nudge(y=3)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0, vjust = -0.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
    theme_classic()


#### Germany ####
DE_data <- subset(Months_c_CJ, ISO == "DE")

specific_dates_DE <- c("2018-10-15","2019-06-13","2019-06-23","2021-09-24","2022-08-09","2023-01-15")
labels_DE <- c("Lawsuit against government","Bonn CLimate Change Conference","Ende Gelände Prostest",
               "FFF Global Climate Strike","Ende Gelände Protests","Protest and Eviction of Lützerath")

# Convert specific dates to date format
specific_dates_DE <- as.Date(specific_dates_DE)

# Create a data frame for specific dates and labels
labels_dfDE <- data.frame(date = specific_dates_DE, label = labels_DE)

# Plot the filtered data
ggplot(DE_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_vline(xintercept = as.numeric(specific_dates_DE), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in Germany") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  #geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = -0.25, hjust = 1, color = "grey50", size = 2,
            #position = position_nudge(y = 0.09)) +
  #geom_text(data = labels_dfDE, aes(x = date, y = 0, label = label),
            #angle = 90, vjust = -0.25, hjust = 1, color = "grey50", size = 2,
            #position = position_nudge(y = 0.09)) +
  #theme(axis.text.x = element_text(angle = 0, hjust = -0.75),
        #axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  #theme_classic()
 
    

#### Canada ####
CA_data <- subset(Months_c_CJ, ISO == "CA")
specific_dates_CA <- c("2018-04-21","2021-09-24","2022-04-22","2023-04-21")
labels_CA <- c("Earth Day Parade Vancouver","Global Climate Strike","Earth Day",
"US President Biden Signs Executive Order Environmental Justice")
  
# Convert specific dates to date format
specific_dates_CA <- as.Date(specific_dates_CA)

# Create a data frame for specific dates and labels
labels_dfCA <- data.frame(date = specific_dates_CA, label = labels_CA)


# Plot the filtered data
ggplot(CA_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_vline(xintercept = as.numeric(specific_dates_CA), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in Canada") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  #geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 2) +
  #geom_text(data = labels_dfCA, aes(x = date, y = 0, label = label), 
  #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 2) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  theme_minimal()


#### countries combined in one figure ####
ggplot() +
  geom_line(data = PH_data, aes(x = month, y = vol_day, color = "Philippines")) +
  geom_line(data = DE_data, aes(x = month, y = vol_month, color = "Germany")) +
  geom_line(data = us_data, aes(x = month, y = vol_month, color = "United States")) +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = as.numeric(specific_dates), linetype = "dashed", color = "red") +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 0.5, hjust = 0, color = "red", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  scale_color_manual(values = c("Philippines" = "darkblue", "Germany" = "green", "United States" = "purple")) +
  guides(color = guide_legend(title = "Country"))


#### India ####
IN_data <- subset(Months_c_CJ, ISO == "IN")
specific_dates_IN <- c("2021-02-15","2021-06-01","2023-02-22")
labels_IN <- c("Multiple Climate Protests","International Conference on Climate Justice",
               "World Sustainable Development Summit")
               

# Convert specific dates to date format
specific_dates_IN <- as.Date(specific_dates_IN)

# Create a data frame for specific dates and labels
labels_dfIN <- data.frame(date = specific_dates_IN, label = labels_IN)

# Plot the filtered data
ggplot(IN_data, aes(x = month, y = vol_month)) +
  geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates_IN), linetype = "dotted", color = "grey50") +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in India") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = -0.25, hjust = 1, color = "grey50", size = 1.7,
            position = position_nudge(y=0.08)) +
  geom_text(data = labels_dfIN, aes(x = date, y = 0, label = label),
            angle = 90, vjust = -0.25, hjust = 1, color = "grey50", size = 1.7,
            position = position_nudge(y=0.08)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.75),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
  theme_minimal()


#### New Zealand ####
NZ_data <- subset(Months_c_CJ, ISO == "NZ")
specific_dates_NZ <- c("2017-01-19","2019-03-15","2020-04-30","2020-12-02","2023-03-03","2023-06-15")
labels_NZ <- c("Extreme Weather Events","First Global Climate Strike","New Climate Change Laws",
               "State of Climate Emergency","Climate Strike","Climate Strike")

# Convert specific dates to date format
specific_dates_NZ <- as.Date(specific_dates_NZ)

# Create a data frame for specific dates and labels
labels_dfNZ <- data.frame(date = specific_dates_NZ, label = labels_NZ)

# Plot the filtered data
ggplot(NZ_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_vline(xintercept = as.numeric(specific_dates_NZ), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in New Zealand") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  #geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  #geom_text(data = labels_dfNZ, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

#### Ethiopia ####
ET_data <- subset(Months_c_CJ, ISO == "ET")

# Plot the filtered data
ggplot(ET_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in Ethiopia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

#### Ghana ####
GH_data <- subset(Months_c_CJ, ISO == "GH")

# Plot the filtered data
ggplot(GH_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in Ghana") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

#### South Africa ####
ZA_data <- subset(Months_c_CJ, ISO == "ZA")

# Plot the filtered data
ggplot(ZA_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in South Africa") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

#### Nigeria ####
NG_data <- subset(Months_c_CJ, ISO == "NG")

# Plot the filtered data
ggplot(NG_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in Nigeria") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

#### Ecuador ####
EC_data <- subset(Months_c_CJ, ISO == "EC")

# Plot the filtered data
ggplot(EC_data, aes(x = month, y = vol_month)) +
  #geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  #geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles in Ecuador") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            #angle = 90, vjust = 0.5, hjust = 0, color = "grey50", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

#### countries combined in one figure ####
ggplot() +
  geom_line(data = PH_data, aes(x = month, y = vol_month, color = "Philippines")) +
  geom_line(data = DE_data, aes(x = month, y = vol_month, color = "Germany")) +
  geom_line(data = NZ_data, aes(x = month, y = vol_month, color = "New Zealand")) +
  geom_line(data = us_data, aes(x = month, y = vol_month, color = "United States")) +
  geom_line(data = NG_data, aes(x = month, y = vol_month, color = "Nigeria")) +
  labs(x = "Month", y = "Volume", title = "Volume of Climate Justice Articles") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = as.numeric(specific_dates), linetype = "dashed", color = "red") +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 0.5, hjust = 0, color = "red", size = 3) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  scale_color_manual(values = c("Philippines" = "darkblue", "Germany" = "green", "United States" = "purple", "Nigeria" = "orange", "New Zealand" = "yellow")) +
  guides(color = guide_legend(title = "Country"))

##all countries combined in a figure showing volume of Articles per Day
ggplot(Days_c_CJ, aes(x = day, y = vol_day)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  labs(x = "Day", y = "Volume", title = "Volume of Climate Justice Articles") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = as.numeric(specific_dates), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates_PH), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates_US), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates_DE), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates_IN), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates_NZ), linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = as.numeric(specific_dates_CA), linetype = "dotted", color = "grey50") +
  geom_text(data = labels_df, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y=110)) +
  geom_text(data = labels_dfPH, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y=110)) +
  geom_text(data = labels_dfUS, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y=110)) +
  geom_text(data = labels_dfDE, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y=110)) +
  geom_text(data = labels_dfIN, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y=110)) +
  geom_text(data = labels_dfNZ, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y=110)) +
  geom_text(data = labels_dfCA, aes(x = date, y = 0, label = label), 
            angle = 90, vjust = 1, hjust = 1, color = "grey50", size = 2,
            position = position_nudge(y=110)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  theme_minimal()
  


################################################################################
####VERGLEICH VERSCHIEDENER VISUALISIERUNGEN ####
#### world map 2022 ####

# prepare data for plotting
map_world_2022 <- joinCountryData2Map(country_2022
                                          , joinCode = "ISO2"
                                          , nameJoinColumn = "ISO",
                                      verbose = TRUE)
  # 185 codes from your data successfully matched countries in the map
  # 4 codes from your data failed to match with a country code in the map: "GI" Gibraltar, "RE" Reunion, "XK" Kosovo, NA
  # 56 codes from the map weren't represented in your data 

# plot data
mapCountryData(map_world_2022, 
               nameColumnToPlot="vol_year",
               catMethod = "quantiles",
               mapTitle = '"Climate Justice" Articles 2022')
  # quantiles

mapCountryData(map_world_2022, 
               nameColumnToPlot="vol_year",
               catMethod = c(0, 0.2625, 0.525, 0.7875, 1.05),
               colourPalette = "white2Black",
               mapTitle = '"Climate Justice" Articles 2022')
  # funktionier nicht, weil zu viele Ausreißer

mapCountryData(map_world_2022, 
               nameColumnToPlot="vol_year",
               catMethod = c(0, 0.00516667, 0.01033334, 0.01550001, 0.02066668, 0.02583335, 0.03100002, 1.05),
               colourPalette = "white2Black",
               mapTitle = '"Climate Justice" Articles 2022')
  # mit den werten der zeitübergreifenden Karte plus letzer wert



#### NOCH OFFEN ####
#### aggregate country level data to regions #### 
# did not work!
map2region(inFile = country_CJ,
           nameDataColumn ="vol_total",
           joinCode ="ISO2",
           nameJoinColumn ="ISO",
           regionType = "Stern", 
           FUN="mean")