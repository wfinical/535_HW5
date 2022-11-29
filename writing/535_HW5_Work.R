library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggthemes)

homicides <- read_csv("homicide-data.csv")
homicides <- homicides %>%
  unite("city_name", c("city", "state"), sep = ", ", remove = FALSE)

View(homicides)

Baltimore <- homicides %>%
  filter(city == "Baltimore") %>%
  mutate(date = ymd(reported_date)) %>%
  mutate(month = ym(paste0(year(date), month(date)))) %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  ungroup()

## Freddie Grey
grey <- homicides %>%
  filter(victim_last == "GREY" & victim_first == "FREDDIE CARLOS") %>%
  mutate(date = ymd(reported_date)) %>%
  select(victim_last, victim_first, date)

fig_df <- Baltimore %>%
  mutate(month_name = month(month, label = TRUE)) %>%
  mutate(weather = case_when(month_name == "Jan" | month_name == "Feb" | 
                               month_name == "Mar" | month_name == "Apr" |
                               month_name == "Nov" | month_name == "Dec"
                             ~ "Winter",
                             month_name == "Jun" | month_name == "Jul" | 
                               month_name == "Aug" | month_name == "Sep" |
                               month_name == "May" | month_name == "Oct"
                             ~ "Summer"))

fig_df %>%
  ggplot(aes(x=month, y=count)) +
  geom_col(aes(fill = weather)) +
  geom_smooth(span = 0.2, se = FALSE) +
  scale_fill_manual(values = c("grey88", "lightblue")) +
  ggtitle("Homicides in Baltimore, MD") +
  xlab("Date") +
  ylab("Monthly homicides") +
  theme_dark() +
  annotate("text", x = as.Date("2013-08-30"), y = 44, 
           label = "Arrest of Freddie Grey", color = 'white') +
  geom_vline(xintercept = grey$date, linetype='dashed', color='red', size=2) +
  theme(legend.position= "bottom",
        legend.spacing.x = unit(0.2, 'cm'),
        legend.title = element_blank())


geom_vline(data = superstar_plot, aes(label = Player, color = NULL),
          hjust = 1.1, vjust = 0.4)