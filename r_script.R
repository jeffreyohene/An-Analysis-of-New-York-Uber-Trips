#Packages needed
library('tidyverse', 'lubridate', 
        'scales', 'ggcharts', 'magick')

#Loading datasets
apr <- read_csv("1uber-raw-data-apr14.csv")
may <- read_csv("2uber-raw-data-may14.csv")
jun <- read_csv("3uber-raw-data-jun14.csv")
jul <- read_csv("4uber-raw-data-jul14.csv")
aug <- read_csv("5uber-raw-data-aug14.csv")
sep <- read_csv("6uber-raw-data-sep14.csv")


#Joining datasets into a single dataframe
df <- rbind(
  apr,may,jun,jul,aug,sep
)

#Dataset structure
str(df)


#Converting Date/Time column to proper date time format
df$Date_Time<-as.POSIXct(
  df$`Date/Time`,format = '%m/%d/%Y %H:%M:%S'
)

str(df)

#Adding new columns for month, day, week day and hour
df$Month <- factor(
  month(df$Date_Time,label=TRUE)
)
df$Day <- as.integer(
  format(df$Date_Time, '%d')
)
df$Week_Day <- factor(wday(
  df$Date_Time,label = TRUE)
)
df$Hour <- as.integer(
  format(df$Date_Time, '%H')
)

str(df)

#Plotting trips by Month
trips_month <- df%>%
  group_by(Month)%>%
  summarize(tot_mrides=n()) %>%
  ggplot(
    aes(Month,tot_mrides)
    )+ 
  geom_col(fill = 'darkgreen', alpha = 0.6)+
  labs(
    x = 'Month',
    y = 'Rides',
    title = 'Rides per Month')+
  theme_classic()

trips_month


#Weekday Trips
trips_wday <- df%>%
  group_by(Week_Day)%>%
  summarize(tot_wrides=n()) %>%
  ggplot(
    aes(Week_Day,tot_wrides)
    )+
  geom_col(fill="orange",alpha=0.8)+
  labs(
    x = 'Day of the Week',
    y = 'Rides',
    title = 'Rides per Day')+
  scale_y_continuous(labels=comma)+
  theme_classic()

trips_wday

#Hourly Trips
trips_hour <- df%>%
  group_by(Hour)%>%
  summarize(tot_hrides=n()) %>%
  ggplot(
    aes(factor(Hour),tot_hrides)
    )+
  geom_col(fill="red",alpha=0.8)+
  labs(
    x = 'Hour',
    y = 'Rides',
    title = 'Rides per Hour')+
  scale_y_continuous(labels = comma)+
  theme_classic()

trips_hour


#Working Day Trips
wkd_trips <- df %>%
  select(Week_Day, Hour)%>%
  filter(Week_Day %in% c("Sat", "Sun"))%>%
  group_by(Hour)%>%
  summarize(wkd_hour = n())%>%
  ggplot(
    aes(factor(Hour), wkd_hour)
    )+
  geom_col(fill = 'black', alpha=0.8)+
  labs(
    x = 'Hour',
    y = 'Rides',
    title = 'Weekend Rides Distribution over Hours')+
  scale_y_continuous(labels = comma)+
  theme_classic()


#Weekend Trips
wkd_trips
wday_trips <- df %>%
  select(Week_Day, Hour)%>%
  filter(!Week_Day %in% c("Sat", "Sun"))%>%
  group_by(Hour)%>%
  summarize(wk_hour = n())%>%
  ggplot(
    aes(factor(Hour), wk_hour)
    )+
  geom_col(fill = 'black', alpha=0.8)+
  labs(
    x = 'Hour',
    y = 'Rides',
    title = 'Week Rides Distribution over Hours')+
  scale_y_continuous(labels = comma)+
  theme_classic()

wday_trips


#Trips Per Base

base_tot <- df%>%
  select(Month, Base)%>%
  group_by(Base)%>%
  summarize(b_total = n())%>%
  ggplot(
    aes(Base, b_total)
    )+
  geom_col(fill = 'brown', alpha = 0.8)+
  labs(
    x = 'Base',
    y = 'Rides',
    title = 'Rides by Base')+
  scale_y_continuous(labels = comma)+
  theme_classic()

base_tot


#Rides by Month & Days
m_d_rides <- df%>%
  select(Month, Week_Day)%>%
  group_by(Month, Week_Day) %>%
  summarize(total = n())%>%
  ggplot(
    aes(Month, total, fill = Week_Day)
    )+
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(
    x = 'Month',
    y = 'Rides',
    title = 'Rides by Day & Month')+
  scale_y_continuous(labels = comma)+
  theme_classic()

m_d_rides



#Rides by Base
rides_base <- df%>%
  select(Base, Month)%>%
  group_by(Base, Month) %>%
  summarize(total = n())%>%
  ggplot(
    aes(Base, total, fill = Month)
    ) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    x = 'Base',
    y = 'Rides',
    title = 'Rides by Base & Month')+
  scale_y_continuous(labels = comma)+
  theme_classic()

rides_base