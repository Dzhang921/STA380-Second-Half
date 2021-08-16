green_building = read.csv('./greenbuildings.csv')
head(green_building)

library(tidyverse)
green_grouped = green_building %>%
  group_by(cluster, green_rating) %>%
  summarise(med_rent = median(Rent),
            median_rent = median(Rent),
            avg_leasing_rate = mean(leasing_rate),
            median_leasing_rate = median(leasing_rate),
            avg_age = mean(age),
            median_age = median(age),
            median_size = median(size)
            )
ggplot(green_grouped) +
  geom_point(aes(x=cluster, y=med_rent,color=green_rating))

ggplot(green_grouped) +
  geom_point(aes(x=cluster, y=median_size,color=green_rating))

green_building %>%
  filter(green_rating == 1)%>%
  summarise(avg_size = mean(size),
            min_size = min(size),
            max_size = max(size))


ggplot(green_building) +
  geom_boxplot(aes(group=green_rating, y=size))

Story_15_Not = green_building %>%
  filter(stories == 15 & amenities == 1) %>%
  group_by(green_rating) %>%
  summarise(median_size = median(size),
            median_rent = median(Rent),
            med_lease_rate = median(leasing_rate),
            med_age = median(age),
            med_elec = median(Electricity_Costs),
            med_gas = median(Gas_Costs)
  )
ggplot(Story_15_Not) +
  geom_col(aes(x=green_rating, y= median_rent, fill=green_rating))


Story_15 = green_building %>%
  filter(stories == 15) %>%
  group_by(green_rating,class_a) %>%
  summarise(median_size = median(size),
            median_rent = median(Rent),
            med_lease_rate = median(leasing_rate),
            med_age = median(age),
            med_elec = median(Electricity_Costs),
            med_gas = median(Gas_Costs)
          ) %>%
  mutate(total = median_size * median_rent * med_lease_rate/100)

ggplot(Story_15) +
  geom_col(aes(x=green_rating, y= median_size, fill=green_rating))+
  facet_wrap(~class_a, nrow=1)



class_grouped = green_building %>%
  group_by(green_rating, class_a, class_b) %>%
  summarise(med_rent = median(Rent),
            med_lease_rate= median(leasing_rate),
            med_size = median(size))
ggplot(class_grouped) +
  geom_col(aes(x=green_rating, y=med_size, fill=green_rating))+
  facet_wrap(~class_a+class_b, nrow=3)
