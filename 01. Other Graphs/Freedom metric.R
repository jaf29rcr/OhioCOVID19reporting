library(tidyverse)
library(readxl)
library(lubridate)
library(maps)
library(tidyquant)
library(fs)

colors<-read_csv("https://coronavirus.ohio.gov/static/OPHASM/ophas_data.csv")

colors2<-colors %>% group_by(county, published_date) %>%
    filter(metric_date==max(metric_date)) %>% 
    distinct(county, county_pop, indicator_1, published_date) %>% 
    ungroup() %>% 
    mutate(raw_cases=indicator_1*(county_pop/100000)) %>%
    group_by(published_date) %>% 
    summarize(raw_cases=sum(raw_cases), population=sum(county_pop)) %>% 
    ungroup() %>% 
    mutate(incidence=raw_cases/(population/100000))

colors2 %>% 
    ggplot(aes(x=published_date, y=incidence)) +
    geom_line(size=2, color="#2C3E50")+
    geom_point(size=5, color="#2C3E50")+
    geom_hline(aes(yintercept=50), color="red", size=2)+
    theme_tq()+
    labs(title="Ohio Cases per 100K by Week",
         x="Published Date",
         y="Cases per 100K",
         caption="Calculated from published results for indicator 1 on OPHAS"
    )
