library(tidyverse)
library(readxl)
library(lubridate)
library(maps)
library(tidyquant)
library(fs)

Region<-read_excel("Dashboard/Region.xlsx") 



# Old Link https://coronavirus.ohio.gov/static/COVIDSummaryData.csv
# Old Link2 https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv

COVID<-read_csv("https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv", 
                col_types=cols(col_character(), col_character(), col_character(),
                               col_character(), col_character(), col_character(),
                               col_number(), col_number(), col_number())) %>% 
    filter(County!="Grand Total") %>%
    mutate(`Admission Date`=case_when(`Admission Date`== "Unknown" ~ `Onset Date`,
                                      TRUE ~ `Admission Date`)) %>%
    inner_join(Region) %>% 
    mutate(Region=case_when(
        Region==1 ~ "1. Toledo",
        Region==2 ~ "2. Cleveland",
        Region==3 ~ "3. Dayton",
        Region==4 ~ "4. Columbus",
        Region==5 ~ "5. Akron",
        Region==6 ~ "6. Cincinnati",
        Region==7 ~ "7. SE Ohio (Athens)",
        TRUE ~ "8. SE Ohio (Zanesville)")) %>% 
    rename(`Death Count`=9)

# Cases


Dates<-COVID %>% 
    mutate(Date=ymd(`Onset Date`)) %>% 
    distinct(Date)

Counties<-COVID %>% distinct(County)

combos<-crossing(Counties, Dates)

Cases<-COVID %>% 
    mutate(Date=ymd(`Onset Date`)) %>%
    group_by(County, Date) %>% 
    summarize(Cases=sum(`Case Count`)) %>% 
    ungroup() %>% 
    full_join(combos) %>% 
    arrange(County, Date) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    group_by(County) %>%
    mutate(Cases_7=rollmean(Cases, k=7, fill=NA, align="right")) %>% 
    ungroup() %>% 
    mutate(Max=today()-days(1)) %>% 
    filter(Date>=Max-days(20) & Date<=Max)

Max<-Cases %>% distinct(Max) %>% pull() %>% pluck()

Cases %>% 
    group_by(County) %>% 
    mutate(increase=case_when(
        Cases_7>lag(Cases_7, n=1) &
            lag(Cases_7, n=1) > lag(Cases_7, n=2) &
            lag(Cases_7, n=2) > lag(Cases_7, n=3) &
            lag(Cases_7, n=3) > lag(Cases_7, n=4) &
            lag(Cases_7, n=4) > lag(Cases_7, n=5) ~ "Yes", TRUE ~ "No")) %>% 
    arrange(County, desc(Date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(County, Date) %>% 
    ungroup() %>% 
    ggplot(aes(x=Date, y=Cases_7, group=County, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~County, nrow=8)+
    theme_tq()+
    scale_color_tq()+
    scale_x_date(date_breaks = "8 days", date_labels = "%b %d")+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 2: Sustained increase in new cases (PREDICTIONS)",
        caption=str_glue("Data Thru {Max}"),
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="New Cases (7 Day Average)",
        color="Indicator tripped?"
    )

# Hospital

Hospital<-COVID %>% 
    mutate(Date=ymd(`Admission Date`)) %>%
    group_by(County, Date) %>% 
    summarize(Cases=sum(`Hospitalized Count`)) %>% 
    ungroup() %>% 
    full_join(combos) %>% 
    arrange(County, Date) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    group_by(County) %>%
    mutate(Cases_7=rollmean(Cases, k=7, fill=NA, align="right")) %>% 
    ungroup() %>% 
    mutate(Max=today()-days(1)) %>% 
    filter(Date>=Max-days(20) & Date<=Max)

Hospital %>% 
    group_by(County) %>% 
    mutate(increase=case_when(
        Cases_7>lag(Cases_7, n=1) &
            lag(Cases_7, n=1) > lag(Cases_7, n=2) &
            lag(Cases_7, n=2) > lag(Cases_7, n=3) &
            lag(Cases_7, n=3) > lag(Cases_7, n=4) &
            lag(Cases_7, n=4) > lag(Cases_7, n=5) ~ "Yes", TRUE ~ "No")) %>% 
    arrange(County, desc(Date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(County, Date) %>% 
    ungroup() %>% 
    ggplot(aes(x=Date, y=Cases_7, group=County, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~County, nrow=8)+
    theme_tq()+
    scale_color_tq()+
    scale_x_date(date_breaks = "8 days", date_labels = "%b %d")+
    theme(panel.spacing=unit(0.1, "lines"))+ labs(
        title="Indicator 6: Sustained increase in new COVID hospital admissions (PREDICTION)",
        caption=str_glue("Data Thru {Max}"),
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="Hospital Admissions (7 Day Average)",
        color="Indicator tripped?"
    )