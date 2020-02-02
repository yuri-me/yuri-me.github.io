---
title: Evaluating the best date to launch ads for Valentine's Day
date: 2019-12-27
tags: [analysis, ggplot, theatre]
excerpt: "We will discuss how campaign start timing is important and determine optimal window to launch prospecting and search ads."
---

St.Valentine's Day (VDay) celebrates love and what can describe your feelings better than a song? Only musical! [LondonTheatreDirect](https://www.londontheatredirect.com/) is an official West End theatre ticket seller and offers access to shows with famous love anthems like [As Long As You’re Mine (Wicked)](https://open.spotify.com/track/5EvzWLxFxzWX5Gx6YyPIUo?si=fwH18gk_S3CyEcAuZwwMIw) or [All I Ask Of You (The Phantom of the Opera)](https://open.spotify.com/track/5klrh466oGToybceGHPGAX?si=EGJd-caQSLSY6sgJtHQtLA).

This post shows how to track changes in customer interest for 14th February theatre performances. We will discuss how campaign start timing is important and determine optimal window to launch prospecting and search ads.

#### Disclaimer

The data was modified to protect business interest of London Theatre Direct Ltd. The views and opinions included in this analysis belong to their author and do not necessarily mirror the views and opinions of the company.

#### Table of Contents  

* [Known General Trend](#known-general-trend)

* [Setup](#setup)

* [Traffic Data Import and Preparation](#traffic-data-import-and-preparation)

* [Sales Data Import and Preparation](#sales-data-import-and-preparation)

* [Sales and Traffic Trends on Line Graphs](#sales-and-traffic-trends-on-line-graphs)

* [Recognize Changes in Customer Interest + why timing is important](#recognize-changes-in-customer-interest)

* [Recommendations (:rocket: Fast-track to results)](#recommendations)

---

### Known General Trend

According to [Google Trends](https://trends.google.com/trends/explore?date=2019-01-01%202019-12-31&geo=GB&q=%2Fm%2F018y5m) in the United Kingdom, the search term "Valentine's Day" starts generating interest from early January and peaks  approaching the day. Thus, it is probably sufficient to analyse only fraction of the year.  

![Google Trends](/yuri-me.github.io/images/2019-12-27-Evaluating/Google-Trends-VDay-UK-2019.png){: .full}

### Setup

We will require following packages:

```R
library(tidyverse) # to prepare data
library(RODBC) # to connect to SQL server
library(getPass) # to hide sensitive details
library(googleAnalyticsR) # to interact with Google Analytics Reporting API
```

Our preference is not to treat strings as factors.

```R
options(stringsAsFactors = FALSE)
```

For Google Analytics (GA) API we need credentials and to avoid sign-in every time, authentication is done through json keyfile (name excluded for confidentiality).

```R
# set up location of GA auth key file
Sys.setenv(GA_AUTH_FILE = "filename.json")
# get access to GA reporting API as service
ga_auth(new_user = FALSE, no_auto = FALSE)
```

As a connector to SQL Server, ODBC driver does the job (running the code requests user 3 times to provide input).

```R
db_con <- odbcDriverConnect(paste0("DRIVER={SQL Server}; SERVER=",
                                   getPass(msg = "IP address of server"),
                                   "; Database=Reporting_api; uid=",
                                   getPass(msg = "DB User"),
                                   "; pwd=",
                                   getPass(msg = "Password")))
```

### Traffic Data Import and Preparation

We want to look into website traffic to the pages with seating plans of performances happening on VDay, one week before Vday and one week after Vday. In the booking process, the previous step to seating plan is to select date on the calendar page of the event.

The principle behind website link structure simplifies our task. For example, the link below is the seating plan (starts with /booking2/) of Mamma Mia show at the Novello theatre commencing on 11-01-2020 and performance identificator is 524887.

```html
https://www.londontheatredirect.com/booking2/mamma-mia-tickets-at-the-novello-theatre-on-saturday-11-january-2020?performanceId=524887
```

As a hint from Google Trends, we look into traffic 90 days before performance date. Because we are interested in a few dates ranging 2 weeks in between, we should count for extra 14 days.

```R
# Set up parameters for the request to GA api
period_end <-  as.Date("2019-02-21")
# 90 days in the past + 2 weeks
period_start <-  period_end - 104

#Use ga_account_list to find the viewId
my_id <- 12345678
```

Next step is to define dimensional filters for _pagePath_ to select only relevant performance dates.

```R
# thursday one week before
df7 <- dim_filter("pagePath", operator = "REGEXP", "-7-february-2019")
# valentine's day and following 
df14 <- dim_filter("pagePath", operator = "REGEXP", "-14-february-2019")
# thursday one week after 
df21 <- dim_filter("pagePath", operator = "REGEXP", "-21-february-2019")
```

In order to meassure traffic to seating plan pages, there is a choice between metrics _Pageviews_ and _Unique Page Views_. The latter does not count page refreshes and page returns during one session and is more accurate to determine what pages attract the most traffic.

Data import from GA looks like this:

```R
data_in <- google_analytics(my_id,
                              date_range = c(as.character(period_start), as.character(period_end)),
                              metrics = c( "uniquePageviews"),
                              dimensions = c("date", "pagePath", "pageTitle"),
                              anti_sample = TRUE, 
                              dim_filters = filter_clause_ga4(list(df7,df14,df21)))
```

Structure of a newly created data frame is illustrated below.

```R
>str(data_in, give.attr = FALSE)
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	13097 obs. of  4 variables:
 $ date           : Date, format: "2018-11-09" "2018-11-09" "2018-11-09" ...
 $ pagePath       : chr  "/booking2/9-to-5-the-musical-tickets-at-the-savoy-theatre-on-thursday-14-february-2019?performanceId=497472" ...
 $ pageTitle      : chr  "9 to 5: The Musical Tickets Thursday 14th February 2019 7.30pm | London Theatre Direct" ...
 $ uniquePageviews: num  9 11 12 5 6 18 6 20 4 7 ...
```

Some _pagePath_ do not include "booking2" part or display an error page. We perform a bit of data cleansing.

```R
data_clean <- data_in[which(str_detect(string = data_in$pagePath, pattern = "booking2")), ]
# Get rid of Error Pages
data_clean <- data_clean[-which(str_detect(string = data_clean$pageTitle, pattern = "Error")), ]
```

From column _pageTitle_, we can extract more information about performance. Firstly, we answer "What is the show?" by detecting pattern  ```<Event name> Tickets ...```. Secondly, performance date is revealed in the pattern of date phrase and question mark ```... 15-february-2019? ...```.  

```R
# Event
data_clean$Event <- str_split(string = data_clean$pageTitle, pattern = " Tickets", simplify = TRUE)[, 1]
# Performance Date
pattern <- "[:digit:]{1,}-[:alpha:]+-[:digit:]{4}\\?"
data_clean$PerformanceDate <- str_extract(string = data_clean$pagePath, pattern = pattern)
data_clean$PerformanceDate <- sub(x = data_clean$PerformanceDate, pattern = "\\?", replacement = "")
data_clean$PerformanceDate <- as.Date(data_clean$PerformanceDate, format = "%d-%B-%Y")
```

Now it is possible to find out _Visit Window_ which designates the period in days between seating plan page visit date and performance date. Here, we created measure to know how in advance web visitor views seating plan of the show on the specific date. _Visit Window_ of 0 means that webpage visit happened on the same date as performance date.

```R
data_clean$VisitWindow <- data_clean$PerformanceDate - data_clean$date
data_clean$VisitWindow <- as.numeric(data_clean$VisitWindow)
```

The next stage is data aggregation based on 2 groups: _PerformanceDate_ and _VisitWindow_. The purpuse is to operate with daily statistics.  

```R
daily_traffic <- data_clean %>%
  filter(VisitWindow >= 0, VisitWindow <= 90) %>%
  group_by(PerformanceDate, VisitWindow) %>%
  summarise(UniqueViews = sum(uniquePageviews)) %>%
  arrange(PerformanceDate, VisitWindow)
  # for plotting
daily_traffic$PerformanceDate <- format(daily_traffic$PerformanceDate, format = "%d %b %Y")
daily_traffic$PerformanceDate <- as.factor(daily_traffic$PerformanceDate)
```

```R
Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	272 obs. of  3 variables:
 $ PerformanceDate: Factor w/ 3 levels "07 Feb 2019",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ VisitWindow    : num  0 1 2 3 4 5 6 7 8 9 ...
 $ UniqueViews    : num  2932 2583 1987 1969 1546 ...
```

### Sales Data Import and Preparation

Another way to assess customer interest is, obviously, number of transactions. GA can not be a source because the reporting platform does not have performance date assigned to sold ticket. The transactional data is stored in the table in MS SQL database. Similarly to traffic, we will explore sales of performances commencing on one week before Vday, VDay, and one week after Vday. 

Our T-SQL query looks like this:

```R
import_df <- sqlQuery(db_con, "SELECT
                              OrderDate
                              , PublicBookingReference
                              , EventEventName
                              , PerformanceDate
                              , DATEDIFF(day, OrderDate PerformanceDate) AS BookingWindow
                              , 1 AS 'Tickets'

                              FROM transaction_history
                              WHERE OrderStatus = 'Purchased'
                              -- Limit to webSales
                              AND SourceType = 'WHITE'
                              AND SalesAgentName IS NULL
                              -- custom dates
                              AND (OrderDate >= '2018-11-09' AND OrderDate <= '2019-02-21')
                              AND PerformanceDate IN ('2019-02-07', '2019-02-14', '2019-02-21')")
```

A metric _Booking Window_ is the period between the booking and the actual date of the performance. When _Booking Window_ equals 0, it is meant that purchase happened on the same date as performance date.

We continue with usual operations of filtering and adjusting data types:

```R
# limit to 90 days booking window
import_90 <- import_df %>% filter(BookingWindow <= 90)
# change data types
import_90$PerformanceDate <-  as.Date(import_90$PerformanceDate)
import_90$OrderDate <-  as.Date(import_90$OrderDate)
```

Since VDay indicates couple activities, a smart move is to select transactions strictly with **2** purchased tickets.  

```R
two_tix <- import_90 %>% count(PublicBookingReference, name = "OrderTix") %>%
  filter(OrderTix == 2) %>% select(-OrderTix)
# leave only "couples" bookings, could use match() as well
couple_df <- semi_join(import_90, two_tix, by = "PublicBookingReference")
```

Again, we aggregate numbers grouping by _PerformanceDate_ and _BookingWindow_.

```R
daily_tickets <-  couple_df %>% group_by(PerformanceDate, BookingWindow) %>%
  summarise(DailyTix = sum(Tickets))
# adjust data types for plotting
daily_tickets$PerformanceDate <- format(daily_tickets$PerformanceDate, format = "%d %b %Y")
daily_tickets$PerformanceDate <- as.factor(daily_tickets$PerformanceDate)
```

```R
Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	261 obs. of  3 variables:
 $ PerformanceDate: Factor w/ 3 levels "07 Feb 2019",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ BookingWindow  : int  0 1 2 3 4 5 ...
 $ DailyTix       : int  200 186 127 100 87 87 ...
```

### Sales and Traffic Trends on Line Graphs

Accomplishing all preparations, we are ready to plot and discuss trends of daily tickets and traffic. For time-series data, the chart type choice falls classically on the line chart. On the x axis is _Visit Window_ (_Booking Window_) and y axis is taken by _daily page views_ (_daily sold tickets_). We have 3 series for each _Performance Date_: week before Vday, Vday and week after Vday.

The resulting ggplot2 code is illustrated fully only for one chart, styling is identical for all plots.
> Visit Window vs Daily Traffic  

```R
ggplot(data = daily_traffic) +
  geom_line(mapping = aes(x = VisitWindow, y = UniqueViews, color = PerformanceDate),
            size = 1.5, alpha = 0.7) +
  # Styling
  labs(title = "Seating Plans Unique PageViews by Days Between Performance and Seating Plan Page Visit",
       subtitle = "\nDay difference of 0 means that webpage visit happened on the same date as performance date\n",
       y = "Unique \nPageViews",
       x = "Visit window",
       color = "Performance Date")  +
  theme_minimal() +
  # select Arial as font sans
  theme(plot.title = element_text(family = "sans", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "sans", size = 14, hjust = 0.5),
        axis.title.y = element_text(family = "sans", angle = 0,  size = 12, face = "italic", vjust = 0.93),
        axis.title.x = element_text(family = "sans", size = 12, face = "italic"),
        axis.text = element_text(family = "sans", size = 12, color = "#000000"),
        legend.position = "top",
        legend.text = element_text(family = "sans", size = 12, color = "#000000"),
        legend.title = element_text(family = "sans", size = 12, color = "#000000"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = c("#d2d2d2", "#ec008b","#000000")) +
  scale_x_reverse(breaks = seq(90,0, by = -15))
```

![Daily Traffic](/yuri-me.github.io/images/2019-12-27-Evaluating/Absolute-Views-by-DayDiff.png)

> Booking Window vs Daily Ticket Sales  

```R
ggplot(data = daily_tickets) +
  geom_line(mapping = aes(x = BookingWindow, y = DailyTix, color = PerformanceDate),
            size = 1.5, alpha = 0.7)
```

![Daily Tickets](/yuri-me.github.io/images/2019-12-27-Evaluating/Tickets-by-BookingWindow.png)

First figure shows _daily traffic_ to seating plan pages from _Visit Window_ perspective. The trend moves from low-activity area (around 90-45 days) to slow increase (~ 45-15) to rapid growth (~ 15-0). Noticeably, lines are ordered according to the performance date, the earliest is below and the latest lies atop. Series order correlates with seasonal recovery of interest to theater events after weak post-Christmas weeks.

Second figure illustrates _daily ticket sales_ (limited to bookings with 2 tickets) against _booking window_. As you can see, "exponential"-like behaviour is similar to previous graph with major difference - around day 30, Vday ticket sales propelled above other categories.  Vday sales were dominant up to day 0. The trend is evident and suggests different reasons than seasonality being responsible in the boost of business. Love in the air buys.

### Recognize Changes in Customer Interest

You might be wondering why it is important to launch ads at the right time. In case of prospecting ads, early campaign start results in low engagement rate due to irrelevancy (and you pay for it). Too late start suggests missed opportunity on the market. With search ads, the penalty for premature start is less severe and consists of strategic misuse of team time on building ads. Thus, we want to be a bit ahead of the timepoint when customer starts showing elevated interest. For us, volume approximates interest, to be precise, volume of traffic or sales. To observe rate of change in volume over time (visit or booking window), we are going to use logarithmic scale on y-axis.

In order to put _daily traffic_ and _ticket sales_ on the same chart, we normalize data ranges to [0,1] [following min-max normalization method](https://en.wikipedia.org/wiki/Feature_scaling#Rescaling_(min-max_normalization)). Here's the code supporting calculations:

```R
norm_daily_traffic <- daily_traffic %>% group_by(PerformanceDate) %>%
  mutate(MaxViewsDate = max(UniqueViews), MinViewsDate = min(UniqueViews)) %>%
  ungroup() %>%
  # define min, max per PerformanceDate
  mutate(NormViews = (UniqueViews - MinViewsDate)/(MaxViewsDate-MinViewsDate)) %>%
  arrange(PerformanceDate, VisitWindow)
```

> Rate Change (log10) of Normalized Sales and Traffic vs Booking/Visit Window

```R
 ggplot() +
  geom_line( data = norm_daily_tickets %>% filter(PerformanceDate == "14 Feb 2019"),
             mapping = aes(x = BookingWindow, y = log10(NormTix)),
             size = 1.5, alpha = 0.8, color = "#ec008b") +
  geom_line( data = norm_daily_traffic %>% filter(PerformanceDate == "14 Feb 2019"),
             mapping = aes(x = DaysDiff, y = log10(NormViews)),
             size = 1.5, alpha = 0.8, color = "#1696d2")
```

The visual was post-edited in graphical program Inkscape to add backgrounds and several labels.

![Rate of change](/yuri-me.github.io/images/2019-12-27-Evaluating/Rate-of-change-of-metrics-by-days-before-perf-Inkscape-edit.png)

There are 3 distinctive regions:

* Sleepy (90-51 days) - values fluctuate at the bottom of the graph.
* Awakening (~ 51-40) - values of _PageViews_ become involved in strong upward trend. Shortly after, _Tickets_ joins upward climb.  
* Steady Growth (~ 40-0) - strong positive trend for both metrics.

#### Recommendations

From the findings of this analysis we are capable to suggest that:

:green_heart: **Theatre tickets buyers begin to develop consistent interest to Valentine's Day performances shortly after Christmas. We recommend launching prospecting ads on social channels following by search ads before the end of the year.**

:broken_heart: **Delaying the action after the first week of the year probably jeopardizes marketing outcomes.**
