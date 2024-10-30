##Created By: Ruby Smith
##Created On: 10/23/2024
##original data size is too large for posit, will be importing analyzed data

install.packages("tidyverse")
install.packages("formatr")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(formatR)
library(knitr)

# import spreadsheet of analyzed data from BigQuery SQL

Annual <- read_excel("202309_202408_CyclisticAnalyzedData.xlsx", sheet = "Annual")
Monthly <- read_excel("202309_202408_CyclisticAnalyzedData.xlsx", sheet = "Monthly")
Weekday <- read_excel("202309_202408_CyclisticAnalyzedData.xlsx", sheet = "Weekday")
Quarterly <- read_excel("202309_202408_CyclisticAnalyzedData.xlsx", sheet = "Quarterly")
Pop_Locations <- read_excel("202309_202408_CyclisticAnalyzedData.xlsx", sheet = "Popular_Locations")
Q2Pop_locations <- read_excel("202309_202408_CyclisticAnalyzedData.xlsx", sheet = "Q2_Popular_Locations")
time <- read_excel("time.xlsx")

##Create annual charts

View(Annual)

#Filtered out 'OVERALL' totals to remove from bar chart below.
filtered_annual <- subset(Annual, percent_of_riders<100)
View(filtered_annual)

#bar chart comparing casual rider counts vs membership rider counts, annually.
ggplot(data = filtered_annual) + 
  geom_col(mapping = aes(x = rider_status, y = percent_of_riders, fill = rider_status)) +
  scale_fill_manual(values=c("blue", "yellow")) +
  labs(title="Annual Percentage of Riders", x="Type of Rider", y="Percentage (%)") +
  ylim(0, 100) +
  theme(legend.position="none") 

#bar chart compares average trip time (minutes) of riders annually.
ggplot(data = Annual) + 
  geom_col(mapping = aes(x = rider_status, y = annual_avg_minutes, fill = rider_status)) +
  scale_fill_manual(values=c("purple", "blue", "yellow")) +
  labs(title="Annual Average Trip Time", x="Type of Rider", y="Average (minutes)") +
  ylim(0, 30) +
  theme(legend.position="none")

#line chart comparing bike type choice between riders.
ggplot(data=Annual) +
  geom_line(mapping = aes(x=rider_status, y=classic_count, group=3, color="green")) +
  geom_line(mapping = aes(x=rider_status, y=electric_count, group=3, color="purple")) +
  labs(title="Comparing Bike Type", x="Type of Rider", y="Rider Count") +
  scale_color_discrete(name="Bike Type", labels=c("Classic", "Electric"))

##Create monthly charts

View(Monthly)

#bar chart shows average trip time between rider type monthly.
ggplot(data=Monthly, aes(x=month, y=monthly_avg_minutes, fill=Rider_status)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=c("blue", "yellow")) +
  ylim(0, 30) +
  labs(title="Monthly Average Trip Time", x="Month", y="Average (minutes)") +
  scale_color_discrete(name="Rider Type", labels=c("Casual", "Membership"))

#line chart comparing counts of rider types per month
ggplot(data=Monthly) +
  geom_smooth(mapping = aes(x=month, y=rider_count, color=Rider_status)) +
  scale_y_continuous(label=scales::comma) +
  labs(title="Monthly Rider Count", x="Month", y="Number of Riders") +
  scale_color_manual(values=c("blue", "yellow"), name="Rider Type", labels=c("Casual", "Membership"))

##Create Weekly Charts
View(Weekday)

#create order factor within weekday data frame with 'weekday' column.
Weekday$weekday <- factor(Weekday$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Bar chart shows Average trip time (minutes) of each rider type on different weekdays.
ggplot(data=Weekday, aes(x=weekday, y=weekday_avg_minutes, fill=rider_status)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=c("blue", "yellow")) +
  ylim(0, 30) +
  labs(title="Weekday Average Trip Time", x="Day of the Week", y="Average (minutes)") +
  scale_color_discrete(name="Rider Type", labels=c("Casual", "Membership"))

#line chart showing count of each rider type per day.
ggplot(data = Weekday) +
  geom_smooth(mapping = aes(x=weekday, y=rider_count, group=1, color=rider_status)) +
  facet_wrap(~rider_status) +
  scale_y_continuous(label=scales::comma) +
  scale_x_discrete(guide=guide_axis(n.dodge = 2)) +
  scale_color_manual(values=c("blue", "yellow")) +
  labs(title="Weekday Rider Counts", x="Day of the Week", y="Number of Riders") +
  theme(legend.position="none")

##Create quarterly charts
View(Quarterly)

#create order factor within quarterly data frame with "quarter" column and "weekday" column.
Quarterly$quarter <- factor(Quarterly$quarter, levels=c("September - November", "December - February", "March - May", "June - August"))

Quarterly$weekday <- factor(Quarterly$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Quarterly counts of types of riders for each day of the week.
ggplot(data = Quarterly, aes(x=weekday, y=rider_count, fill=rider_status)) +
  geom_col(position="dodge") +
  facet_wrap(~quarter) +
  scale_fill_manual(values=c("blue", "yellow"), name="Rider Type", labels=c("Casual", "Membership")) +
  labs(title="Quarterly Rider Counts", x="Day of the Week", y="Number of Riders") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2)) 

#quarterly average trip time between types of riders.
ggplot(data = Quarterly, aes(x=weekday, y=quarterly_avg_minutes, fill=rider_status))+
  geom_col(position="dodge") +
  facet_wrap(~quarter) +
  ylim(0, 30) +
  scale_fill_manual(values=c("blue", "yellow"), name="Rider Type", labels=c("Casual", "Membership")) +
  labs(title="Quarterly Average Trip Time", x="Day of the Week", y="Average (minutes)") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2))

#filtered 2nd quarter out of quarter data frame.
filtered_quarterly <- subset(Quarterly, quarter == "December - February")
View(filtered_quarterly)

#2nd quarter rider counts.
ggplot(data = filtered_quarterly, aes(x=weekday, y=rider_count, fill=rider_status)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=c("blue", "yellow"), name="Rider Type", labels=c("Casual", "Membership")) +
  labs(title="2nd Quarter Rider Counts", x="Day of the Week", y="Number of Riders") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2)) 

##create time_interval chart

time$time_interval <- factor(time$time_interval, levels=c("< 1 min", "1 < n > 5 min", "5 < n > 10 min", "10 < n > 15 min", "15 < n > 30 min", "30 min < n > 1 hr", "1 hr < n > 3 hr", "< 3 hr"))

#Time interval chart comparing rider trip times
ggplot(data = time, aes(x=time_interval, y=rider_count, fill=rider_status)) +
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  scale_fill_manual(values=c("blue", "yellow"), name="Rider Type") +
  labs(title="Trip Time Compared in Different Intervals", x="Time Intervals", y="Number of Riders")

install.packages("rmarkdown")  
library(rmarkdown)
