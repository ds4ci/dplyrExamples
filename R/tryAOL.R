

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
aol <- read_tsv("C:/proj/dplyrExamples/inst/extdata/user-ct-test-collection-01.txt")
length(unique(aol$AnonID))

Sys.time()
aol_sessions <- aol %>%
  arrange(AnonID, QueryTime) %>%
  group_by(AnonID) %>%
  mutate(Minutes_After_Last = difftime(QueryTime, lag(QueryTime), units = "mins"),
         # Minutes_After_Last = time_length(QueryTime - lag(QueryTime), unit = "minute"),
         New_Session_Flag = is.na(lag(AnonID)) |
           AnonID != lag(AnonID) | Minutes_After_Last > 30,
         Session_Seq_Num = cumsum(New_Session_Flag)
         ) %>%
  group_by(AnonID, Session_Seq_Num) %>%
  summarize(Session_Start_At = first(QueryTime),
            Number_Searches = n(),
            Number_Terms = n_distinct(Query),
            Session_Duration = difftime(last(QueryTime), first(QueryTime), units = "mins"),
            # Session_Duration = time_length(last(QueryTime) - first(QueryTime), unit = "minute"),
            Number_Clicks = sum(!is.na(ClickURL))
            )
Sys.time()
## 3:09 minutes with difftime
## 24 minutes with lubridate::time_length
## ~ 4 minutes with simple difference - was bug as units would change!

ggplot(aol_sessions, aes(Session_Duration)) + geom_histogram(binwidth = 10) + scale_y_log10()
ggplot(aol_sessions, aes(Number_Clicks)) + geom_histogram(binwidth = 1) + scale_y_log10() + xlim(1, 50)
ggplot(aol_sessions, aes(as.Date(Session_Start_At))) + geom_histogram(binwidth = 1)

Sys.time()
aol_visitors <- aol_sessions %>%
  group_by(AnonID) %>%
  summarize(Number_Sessions = n(),
            First_Session_At = min(Session_Start_At),
            Last_Session_At = max(Session_Start_At),
            Total_Duration = sum(Session_Duration),
            Avg_Duration = mean(Session_Duration),
            Median_Duration = median(Session_Duration),
            Avg_Num_Searches = mean(Number_Searches),
            Median_Num_Searches = median(Number_Searches),
            Avg_Num_Clicks = mean(Number_Clicks),
            Median_Num_Clicks = median(Number_Clicks)
            )
Sys.time()
## ~ 30 seconds
summary(aol_visitors)

ggplot(aol_visitors, aes(Number_Sessions)) + geom_histogram(binwidth = 1) + scale_y_log10() + xlim(1, 200)
ggplot(aol_visitors, aes(Avg_Duration)) + geom_histogram(binwidth = 1) + scale_y_log10() + xlim(0, 60)
ggplot(aol_visitors, aes(Avg_Num_Clicks)) + geom_histogram(binwidth = 1) + scale_y_log10() + xlim(0, 20)
