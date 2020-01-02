
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(rStrava)

app_name <- "analysis"
app_client_id <- 33757
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))

my_activities <- get_activity_list(stoken)
acts_data <- compile_activities(my_activities) %>% as.data.table()

acts_data[, start_date := as.POSIXct(start_date, format="%Y-%m-%dT%H:%M:%SZ")]
acts_data[, start_date_local := as.POSIXct(start_date_local, format="%Y-%m-%dT%H:%M:%SZ")]
acts_data[, year := year(start_date)]
acts_data[, month := month(start_date)]
acts_data[, day := day(start_date)]
acts_data[, week_day := wday(acts_data$start_date, label = TRUE, abbr = FALSE, week_start = 1, locale = "English")]
acts_data <- acts_data[order(start_date)]

# only for 2019
acts_data <- acts_data[year==2019]

p1 <- ggplot(data=acts_data[, .(distance=sum(distance)), by = .(type)][distance>0], aes(x=type, y=distance, fill=type, colour=type, label=distance)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(show.legend = FALSE, vjust=-1, size=5) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "total distance by type", subtitle = "in kilometre", caption = "https://github.com/wittmaan/YearInSport2019") +
  theme_minimal()
ggsave("1_total_distance.png", p1)


p2 <- ggplot(data=acts_data[, .(count = .N), by = .(week_day)], aes(x=week_day, y=count, fill=week_day, colour=week_day, label=count)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(show.legend = FALSE, vjust=-1, size=5) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "number of activities by day", caption = "https://github.com/wittmaan/YearInSport2019") +
  theme_minimal()
ggsave("2_number_activities_day.png", p2)


p3 <- ggplot(data=acts_data[, .(count = .N), by = .(week_day, type)], aes(x=week_day, y=count, fill=week_day, colour=week_day, label=count)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(show.legend = FALSE, vjust=-1, size=5) +
  facet_wrap(~type) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "number of activities by day", subtitle = "seperated by type", caption = "https://github.com/wittmaan/YearInSport2019") +
  theme_minimal()
ggsave("3_number_activities_day_type.png", p3)


acts_data_swim <- acts_data[type=="Swim"][, .(pace=(moving_time/60/(distance*1000))*100, start_date, month, id)][pace<3 & pace>1.7]

p4 <- ggplot(data=acts_data_swim, aes(x=start_date, y=pace)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  scale_color_viridis() +
  labs(title = "swimming pace", subtitle = "minutes per 100 meter", caption = "https://github.com/wittmaan/YearInSport2019") +
  theme_minimal()
ggsave("4_swimming_pace.png", p4)


acts_data_ride <- acts_data[type=="Ride"][, .(average_speed, start_date, start_latitude, start_longitude, month, id)]
acts_data_ride[, outdoor := TRUE]
acts_data_ride[is.na(start_longitude), outdoor := FALSE]


p5 <- ggplot(data=acts_data_ride, aes(x=start_date, y=average_speed, colour=outdoor)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  scale_color_viridis(discrete = TRUE, begin=0, end=0.8) +
  labs(title = "ride average speed", subtitle = "kilometre per hour", caption = "https://github.com/wittmaan/YearInSport2019") +
  theme_minimal()
ggsave("5_ride_average_speed.png", p5)



acts_data_run <- acts_data[type=="Run"][, .(pace = moving_time/60/distance, start_date, month, id)][pace<10]

p6 <- ggplot(data=acts_data_run, aes(x=start_date, y=pace)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  scale_color_viridis() +
  labs(title = "running pace", subtitle = "minutes per kilometer", caption = "https://github.com/wittmaan/YearInSport2019") +
  theme_minimal()
ggsave("6_running_pace.png", p6)

