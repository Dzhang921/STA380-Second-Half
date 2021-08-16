ABIA = read.csv('./ABIA.csv')
head(ABIA)
library(wesanderson)
library(tidyverse)
library(dplyr)
# Clean Data
summary(ABIA)

Clean_ABIA = ABIA %>%
  mutate(CarrierDelay = ifelse(is.na(CarrierDelay), 0, CarrierDelay),
         WeatherDelay = ifelse(is.na(WeatherDelay), 0, WeatherDelay),
         NASDelay = ifelse(is.na(NASDelay), 0, NASDelay),
         SecurityDelay = ifelse(is.na(SecurityDelay), 0, SecurityDelay)) %>%
  drop_na()



# Create a new data with AUS as origin
AUS_Monthly = ABIA %>%
  filter(Origin == 'AUS') %>%
  drop_na()%>%
  group_by(Month)%>%
  summarise(count = n(),
            mean_arr_delay = mean(ArrDelay),
            med_arr_delay = median(ArrDelay),
            mean_dep_delay = mean(DepDelay),
            med_dep_delay = median(DepDelay),
            mean_dist = mean(Distance)
            )

ggplot(AUS_Monthly) + 
  geom_col(aes(x=factor(Month), y=count))

ggplot(Clean_ABIA)+
  geom_bar(aes(x=factor(Month)))+
  facet_wrap(~DayOfWeek,ncol=2)

# Plot Daily Volume in different month
ggplot(Clean_ABIA)+
  geom_bar(aes(x=factor(DayOfWeek), fill=factor(DayOfWeek)))+
  facet_wrap(~Month,ncol=3)+
  scale_fill_manual(values = wes_palette('BottleRocket1'))+
  labs(x = 'Day of Week',
       y = 'Total Aircrafts Volume',
       Title = 'Total Volume in Austin Airport vs. Day of Week')


# Filter Out Austin as Destination
Dest_Aus = Clean_ABIA %>%
  filter(Dest == 'AUS')

ggplot(Dest_Aus)+
  geom_bar(aes(x=factor(DayOfWeek), fill=factor(DayOfWeek)))+
  facet_wrap(~Month,ncol=3)+
  scale_fill_manual(values = wes_palette('BottleRocket1'))+
  labs(x = 'Day of Week',
       y = 'Arrival Aircrafts Volume',
       Title = 'Arrival Volume in Austin Airport vs. Day of Week')

# Only 8, 9, 10
Dest_Aus_9 = ABIA %>%
  filter(Dest == 'AUS' & (Month == 9|Month==10|Month==11))

ggplot(Dest_Aus_9)+
  geom_boxplot(aes(x=factor(DayOfWeek), y=ArrDelay, color=factor(DayOfWeek)))+
  facet_wrap(~Month,nrow=1)+
  scale_fill_manual(values = wes_palette('BottleRocket1'))+
  labs(x = 'Day of Week',
       y = 'Arrival Delay(Total Delay)',
       Title = 'Arrival Delay Distribution vs. Day of Week')

# Filter Out Austin as Destination
Dep_Aus = Clean_ABIA %>%
  filter(Origin == 'AUS')

ggplot(Dep_Aus)+
  geom_bar(aes(x=factor(DayOfWeek), fill=factor(DayOfWeek)))+
  facet_wrap(~Month,ncol=3)+
  scale_fill_manual(values = wes_palette('BottleRocket1'))+
  labs(x = 'Day of Week',
       y = 'Arrival Aircrafts Volume',
       Title = 'Arrival Volume in Austin Airport vs. Day of Week')

Dep_Aus_9 = ABIA %>%
  filter(Origin == 'AUS' & (Month == 9|Month==10|Month==11))

ggplot(Dep_Aus_9)+
  geom_boxplot(aes(x=factor(DayOfWeek), y=DepDelay, color=factor(DayOfWeek)))+
  facet_wrap(~Month,nrow=1)+
  scale_fill_manual(values = wes_palette('BottleRocket1'))+
  labs(x = 'Day of Week',
       y = 'Departure Delay(Total Delay)',
       Title = 'Departure Delay Distribution vs. Day of Week')






