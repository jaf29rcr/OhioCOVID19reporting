library(tidyverse)
library(readxl)
library(lubridate)
library(maps)
library(tidyquant)
library(fs)
library(gridExtra)
library(patchwork)

colors<-read_csv("https://coronavirus.ohio.gov/static/OPHASM/ophas_data.csv") %>% 
    mutate(published_date=ymd(published_date), metric_date=ymd(metric_date))


# colors<-read_csv("Data/ophas_data.csv") %>% 
#     mutate(published_date=ymd(published_date), metric_date=ymd(metric_date))



last_week<-colors %>% filter(published_date==max(published_date)) %>% 
    select(county, metric_date, county_pop, indicator_2, indicator_4, indicator_5, indicator_6) %>% 
    arrange(county, metric_date) %>% 
    mutate(indicator6_scaled=indicator_6/county_pop*100000)


# Indicator 2

last_week %>%
    group_by(county) %>% 
    mutate(increase=case_when(
        indicator_2>lag(indicator_2, n=1) &
            lag(indicator_2, n=1) > lag(indicator_2, n=2) &
            lag(indicator_2, n=2) > lag(indicator_2, n=3) &
            lag(indicator_2, n=3) > lag(indicator_2, n=4) &
            lag(indicator_2, n=4) > lag(indicator_2, n=5) ~ "Yes", TRUE ~ "No")) %>% 
    arrange(county, desc(metric_date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(county, metric_date) %>% 
    ungroup() %>% 
    ggplot(aes(x=metric_date, y=indicator_2, group=county, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~county, nrow=8)+
    theme_tq()+
    scale_color_tq()+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 2: Sustained increase in new cases",
        caption="Data Published 4/15/21",
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="New Cases (7 Day Average)",
        color="Indicator tripped?"
    )


# Indicator 4

last_week %>%
    group_by(county) %>% 
    mutate(increase=case_when(
        indicator_4>lag(indicator_4, n=1) &
            lag(indicator_4, n=1) > lag(indicator_4, n=2) &
            lag(indicator_4, n=2) > lag(indicator_4, n=3) &
            lag(indicator_4, n=3) > lag(indicator_4, n=4) &
            lag(indicator_4, n=4) > lag(indicator_4, n=5) ~ "Yes", TRUE ~ "No")) %>%
    arrange(county, desc(metric_date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(county, metric_date) %>% 
    ungroup() %>% 
    ggplot(aes(x=metric_date, y=indicator_4, group=county, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~county, nrow=8)+
    theme_tq()+
    scale_color_tq()+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 4: Sustained increase in Emergency Department (ED) visits for COVID-like illness",
        caption="Data Published 4/15/21",
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="ED Visits (7 Day Average)",
        color="Indicator tripped?"
    )

# Indicator 5

last_week %>%
    group_by(county) %>% 
    mutate(increase=case_when(
        indicator_5>lag(indicator_5, n=1) &
            lag(indicator_5, n=1) > lag(indicator_5, n=2) &
            lag(indicator_5, n=2) > lag(indicator_5, n=3) &
            lag(indicator_5, n=3) > lag(indicator_5, n=4) &
            lag(indicator_5, n=4) > lag(indicator_5, n=5) ~ "Yes", TRUE ~ "No")) %>%
    arrange(county, desc(metric_date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(county, metric_date) %>% 
    ungroup() %>% 
    ggplot(aes(x=metric_date, y=indicator_5, group=county, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~county, nrow=8)+
    theme_tq()+
    scale_color_tq()+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 5: Sustained increase in outpatient visits for COVID-like illness",
        caption="Data Published 4/15/21",
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="Outpatient Visits (7 Day Average)",
        color="Indicator tripped?"
    )

# Indicator 6

last_week %>%
    group_by(county) %>% 
    mutate(increase=case_when(
        indicator_6>lag(indicator_6, n=1) &
            lag(indicator_6, n=1) > lag(indicator_6, n=2) &
            lag(indicator_6, n=2) > lag(indicator_6, n=3) &
            lag(indicator_6, n=3) > lag(indicator_6, n=4) &
            lag(indicator_6, n=4) > lag(indicator_6, n=5) ~ "Yes", TRUE ~ "No")) %>%
    arrange(county, desc(metric_date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(county, metric_date) %>% 
    ungroup() %>% 
    ggplot(aes(x=metric_date, y=indicator_6, group=county, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~county, nrow=8)+
    theme_tq()+
    scale_color_tq()+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 6: Sustained increase in new COVID hospital admissions",
        caption="Data Published 4/15/21",
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="Hospital Admissions (7 Day Average)",
        color="Indicator tripped?"
    )


# Indicator 6 Per Capita

last_week %>%
    mutate(indicator6_scaled=indicator_6/county_pop*100000) %>% 
    group_by(county) %>% 
    mutate(increase=case_when(
        indicator_6>lag(indicator_6, n=1) &
            lag(indicator_6, n=1) > lag(indicator_6, n=2) &
            lag(indicator_6, n=2) > lag(indicator_6, n=3) &
            lag(indicator_6, n=3) > lag(indicator_6, n=4) &
            lag(indicator_6, n=4) > lag(indicator_6, n=5) ~ "Yes", TRUE ~ "No")) %>%
    arrange(county, desc(metric_date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(county, metric_date) %>% 
    ungroup() %>% 
    ggplot(aes(x=metric_date, y=indicator6_scaled, group=county, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~county, nrow=8)+
    theme_tq()+
    scale_color_tq()+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 6: Sustained increase in new COVID hospital admissions (Per Capita View)",
        caption="Data Published 3/11/21",
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="Hospital Admissions Per 100,000 Residents (7 Day Average)",
        color="Indicator tripped?"
    )




# Indicator 6 by Hospital Region


Region<-read_excel("03 Ohio Dashboard/Region.xlsx") %>% select(County, Region) %>%
    rename(county=County) %>% 
    mutate(Region=case_when(
        Region==1 ~ "1. Toledo",
        Region==2 ~ "2. Cleveland",
        Region==3 ~ "3. Dayton",
        Region==4 ~ "4. Columbus",
        Region==5 ~ "5. Akron",
        Region==6 ~ "6. Cincinnati",
        Region==7 ~ "7. SE Ohio (Athens)",
        TRUE ~ "8. SE Ohio (Zanesville)"))

last_week %>%
    inner_join(Region) %>% 
    group_by(Region, metric_date) %>% 
    summarize(indicator_6=sum(indicator_6), county_pop=sum(county_pop)) %>%
    ungroup() %>% 
    mutate(indicator6_scaled=indicator_6/county_pop*100000) %>% 
    group_by(Region) %>% 
    mutate(increase=case_when(
        indicator_6>lag(indicator_6, n=1) &
            lag(indicator_6, n=1) > lag(indicator_6, n=2) &
            lag(indicator_6, n=2) > lag(indicator_6, n=3) &
            lag(indicator_6, n=3) > lag(indicator_6, n=4) &
            lag(indicator_6, n=4) > lag(indicator_6, n=5) ~ "Yes", TRUE ~ "No")) %>%
    arrange(Region, desc(metric_date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(Region, metric_date) %>% 
    ungroup() %>% 
    ggplot(aes(x=metric_date, y=indicator6_scaled, group=Region, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    facet_wrap(~Region, nrow=4)+
    theme_tq()+
    scale_color_tq()+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 6: Sustained increase in new COVID hospital admissions (Per Capita View)",
        caption="Data Published 11/26/20",
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="Hospital Admissions Per 100,000 Residents (7 Day Average)",
        color="Indicator tripped?"
    )

# Indicator 6 entire State


Region<-read_excel("03 Ohio Dashboard/Region.xlsx") %>% select(County, Region) %>%
    rename(county=County) %>% 
    mutate(Region=case_when(
        Region==1 ~ "1. Toledo",
        Region==2 ~ "2. Cleveland",
        Region==3 ~ "3. Dayton",
        Region==4 ~ "4. Columbus",
        Region==5 ~ "5. Akron",
        Region==6 ~ "6. Cincinnati",
        Region==7 ~ "7. SE Ohio (Athens)",
        TRUE ~ "8. SE Ohio (Zanesville)"))

last_week %>%
    group_by(metric_date) %>% 
    summarize(indicator_6=sum(indicator_6), county_pop=sum(county_pop)) %>%
    ungroup() %>% 
    mutate(indicator6_scaled=indicator_6/county_pop*100000) %>% 
    mutate(increase=case_when(
        indicator_6>lag(indicator_6, n=1) &
            lag(indicator_6, n=1) > lag(indicator_6, n=2) &
            lag(indicator_6, n=2) > lag(indicator_6, n=3) &
            lag(indicator_6, n=3) > lag(indicator_6, n=4) &
            lag(indicator_6, n=4) > lag(indicator_6, n=5) ~ "Yes", TRUE ~ "No")) %>%
    arrange(desc(metric_date)) %>% 
    mutate(increase2=case_when(
        increase=="Yes" ~ "Yes",
        lag(increase, n=1) == "Yes" ~ "Yes",
        lag(increase, n=2) == "Yes" ~ "Yes",
        lag(increase, n=3) == "Yes" ~ "Yes",
        lag(increase, n=4) == "Yes" ~ "Yes",
        TRUE ~ "No")) %>%
    arrange(metric_date) %>%
    mutate(state="Ohio") %>% 
    ggplot(aes(x=metric_date, y=indicator6_scaled, group=state, color=as.factor(increase2))) + 
    geom_line()+
    geom_point()+
    theme_tq()+
    scale_color_tq()+
    theme(panel.spacing=unit(0.1, "lines"))+
    labs(
        title="Indicator 6: Sustained increase in new COVID hospital admissions (Per Capita View)",
        caption="Data Published 11/26/20",
        subtitle="Tripped if metric increases for 5 consecutive days",
        x="",
        y="Hospital Admissions Per 100,000 Residents (7 Day Average)",
        color="Indicator tripped?"
    )
