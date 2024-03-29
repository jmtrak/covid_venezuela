# The aim of this script is to extract and analyze Venezuelan's government information.

# Load libraries
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(lubridate)

# Setting working directories 
## Data directory
if (!dir.exists("data")) {
  dir.create("data")
}

## Directorio de salidas
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Extracting data from API

## Getting data from API
url_covid_ven <- 'https://covid19.patria.org.ve/api/v1/timeline'
response_url_ve <- GET(url_covid_ven)

## Raw data: is not structured and readable
json_resp_text<-content(response_url_ve,as="text") 

## transform data to data frame and a tibble
covid_ven_df <- fromJSON(json_resp_text)
covid_ven_df <- as_tibble(covid_ven_df)

# Data exploration
str(covid_ven_df) ### Data has nested tibbles inside


## Unnest the data frames isnde the tibble, and getting the right names

covid_ven_df_2 <- unnest(covid_ven_df,cols = c(Confirmed, Recovered, Deaths, Active),
       names_repair = "unique", names_sep = c("_"))

## Check colnames
colnames(covid_ven_df_2)
write.csv(covid_ven_df_2, "data/covid_ven_df_2.csv")

## Date formating 
covid_ven_df_2$Date <- as_date(covid_ven_df_2$Date)
summary(covid_ven_df_2$Date)

cumulative_cases <- filter(covid_ven_df_2, Date == max(Date))
class(cumulative_cases$Date)

# EDA
table_monhtly <- covid_ven_df_2 |>
  mutate(months = month(Date), year = year(Date)) |>
  select(everything(), - DateTS) |>
  group_by(year, months) |>
  summarise(Date = max(Date), 
            across(c(ends_with("New")), 
                   list(sum = sum, mean = mean), 
                   .names = "{.col}_{.fn}"))
tail(table_monhtly)

# Chart 1
g_covid_cases <- table_monhtly |>
  filter(Date < "2022-06-30") |>
  ggplot(aes(Date, Confirmed_New_sum)) +
  geom_line() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  ylab("Confirmed Cases") + xlab("") +
  theme_classic()

g_covid_cases
ggsave("outputs/g_covid_cases.png", g_covid_cases, dpi = 300, device = "png")

# Chart 2
g_covid_cum <- covid_ven_df_2 %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y= Confirmed_Count)) + theme_classic() +
  ylab("Cumulative Cases") + xlab("") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  scale_y_continuous(labels = scales::comma_format(),
                     n.breaks = 10) +
  geom_label(data =cumulative_cases, 
             aes(y = Confirmed_Count,
             label = Confirmed_Count)) +
  theme(axis.text.x = element_text(size =8))

g_covid_cum
ggsave("outputs/covid_cum.png", g_covid_cum, dpi = 300, device = "png")

glimpse(covid_ven_df, width = 10)
