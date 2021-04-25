library(tidyverse)
library(readxl)
library(lubridate)
library(maps)
library(tidyquant)
library(fs)
library(gridExtra)
library(patchwork)

colors<-read_csv("https://coronavirus.ohio.gov/static/OPHASM/ophas_data.csv") %>% arrange(published_date) %>% 
    mutate(published_date=ymd(published_date), metric_date=ymd(metric_date))


county_map <- map_data("county") %>% 
    filter(region=="ohio") 

colors2<-colors %>%
    distinct(county, published_date, indicator_1_score, indicator_2_score, indicator_3_score, indicator_4_score,
             indicator_5_score, indicator_6_score, indicator_7_score) %>% 
    mutate(score=indicator_1_score+indicator_2_score+indicator_3_score+indicator_4_score+
               indicator_5_score+indicator_6_score+indicator_7_score) %>% 
    mutate(color=case_when(
        score<=1 ~ "Yellow",
        score<=3 ~ "Orange",
        score<=5 ~ "Red",
        score <=7 ~ "Purple"
    )) %>% 
    mutate(subregion=tolower(county)) %>% 
    inner_join(county_map) %>% 
    arrange(county, published_date)


cols <- c("Yellow" = "yellow", "Orange" = "orange", "Red" = "red", "Purple"="purple")

ggplot() +  
    geom_polygon(data=colors2, aes(x=long, y=lat, fill=color, group=group), color="black", alpha=1)+
    coord_equal(ratio=1)+# square plot to avoid the distortion
    theme_tq()+
    scale_fill_manual(values = cols)+
    coord_equal(ratio=1)+
    facet_wrap(~published_date, labeller = label_wrap_gen(), nrow=4)+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(0.9), face = "bold"),
          plot.subtitle = element_text(size = rel(1.0)),
          plot.caption = element_text(size = rel(0.8)),
          panel.spacing=unit(0.1, "lines"),
          legend.position="none"
    )+
    labs(
        title="Ohio Public Health Advisory System if only looking at number of indicators tripped"
    )

color_history<-colors %>% distinct(county, published_date, color) %>% 
    mutate(subregion=tolower(county)) %>% 
    inner_join(county_map)

ggplot() +  
    geom_polygon(data=color_history, aes(x=long, y=lat, fill=color, group=group), color="black", alpha=1)+
    coord_equal(ratio=1)+# square plot to avoid the distortion
    theme_tq()+
    scale_fill_manual(values = cols)+
    coord_equal(ratio=1)+
    facet_wrap(~published_date, labeller = label_wrap_gen(), nrow=4)+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(0.9), face = "bold"),
          plot.subtitle = element_text(size = rel(1.0)),
          plot.caption = element_text(size = rel(0.8)),
          panel.spacing=unit(0.1, "lines"),
          legend.position="none"
    )+
    labs(
        title="Ohio Public Health Advisory System"
    )

# New Incidence Chart

incidence<-colors %>%
    mutate(metric_date=ymd(metric_date)) %>% 
    group_by(county, published_date) %>% 
    filter(metric_date==max(metric_date)) %>%
    mutate(subregion=tolower(county)) %>% 
    inner_join(county_map) %>% 
    arrange(county, published_date)


ggplot() +  
    geom_polygon(data=incidence, aes(x=long, y=lat, fill=indicator_1, group=group), color="black", alpha=1)+
    coord_equal(ratio=1)+# square plot to avoid the distortion
    theme_tq()+
    scale_fill_gradient2(
        low = "yellow",
        mid = "orange",
        high = "red",
        midpoint = 750, 
        labels = scales::number_format(accuracy=1, big.mark=","),
        limits=c(0,1500),
        breaks = c(0,500,1000,1500)
    )+
    coord_equal(ratio=1)+
    facet_wrap(~published_date, labeller = label_wrap_gen(), nrow=4)+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(0.9), face = "bold"),
          plot.subtitle = element_text(size = rel(1.0)),
          plot.caption = element_text(size = rel(0.8)),
          panel.spacing=unit(0.1, "lines")
    )+
    labs(
        title="Incidence - Cases per 100,000 Residents Over 2 Weeks",
        fill="",
        caption="Data from Ohio Public Health Advisory System
        Value for Indicator 1 Each Week"
    )


# ICU Chart

icu<-colors %>%
    filter(indicator_7_covid != "NULL") %>%
    mutate(TOTAL=as.numeric(indicator_7_total), COVID=as.numeric(indicator_7_covid)) %>%
    mutate(Share2=COVID/TOTAL) %>% 
    group_by(county, published_date) %>% 
    summarize(Share=mean(Share2)) %>% 
    ungroup() %>%
    mutate(subregion=tolower(county)) %>% 
    inner_join(county_map)


Region<-read_excel("Dashboard/Region.xlsx") %>% select(County, Region) %>% rename(county=County) %>%  mutate(subregion=tolower(county)) 

icu_labels<-icu %>% inner_join(Region) %>% group_by(published_date, Region, Share) %>% 
    summarize(lat2=mean(lat), long2=mean(long)) %>% 
    ungroup() %>% 
    mutate(label=Share %>% scales::percent(accuracy=0.1))


ggplot() +  
    geom_polygon(data=icu, aes(x=long, y=lat, fill=Share, group=group), color=NA, alpha=1)+
    geom_label(data=icu_labels, aes(x=long2, y=lat2,label=label))+
    coord_equal(ratio=1)+# square plot to avoid the distortion
    theme_tq()+
    scale_fill_gradient2(
        low = "white",
        mid = "deepskyblue",
        high = "blue",
        midpoint = 0.75/2, 
        labels = scales::percent_format(accuracy=1),
        limits=c(0,0.75),
        breaks = c(0,0.25,0.5,075)
    )+
    coord_equal(ratio=1)+
    facet_wrap(~published_date, labeller = label_wrap_gen(), nrow=4)+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(0.9), face = "bold"),
          plot.subtitle = element_text(size = rel(1.0)),
          plot.caption = element_text(size = rel(0.8)),
          panel.spacing=unit(0.1, "lines")
    )+
    labs(
        title="Regional COVID-19 ICU Utilization",
        fill="",
        caption="Data from Ohio Public Health Advisory System
        Average of % COVID ICU / % Total ICU"
    )


# Just last week map comparison


colors_latest<-colors2 %>% filter(published_date==max(published_date))

A<-ggplot() +  
    geom_polygon(data=colors_latest, aes(x=long, y=lat, fill=color, group=group), color="black", alpha=1)+
    coord_equal(ratio=1)+# square plot to avoid the distortion
    theme_tq()+
    scale_fill_manual(values = cols)+
    coord_equal(ratio=1)+
    facet_wrap(~published_date, labeller = label_wrap_gen(), nrow=4)+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(1.2), face = "bold",hjust = 0.5),
          plot.subtitle = element_text(size = rel(1.2), hjust = 0.5),
          plot.caption = element_text(size = rel(0.8)),
          panel.spacing=unit(0.1, "lines"),
          legend.position="none"
    )+
    labs(
        title="Ohio Public Health Advisory System based on number of indicators tripped",
        subtitle="Old Rules"
    )


history_latest<-color_history %>% filter(published_date==max(published_date))

B<-ggplot() +  
    geom_polygon(data=history_latest, aes(x=long, y=lat, fill=color, group=group), color="black", alpha=1)+
    coord_equal(ratio=1)+# square plot to avoid the distortion
    theme_tq()+
    scale_fill_manual(values = cols)+
    coord_equal(ratio=1)+
    facet_wrap(~published_date, labeller = label_wrap_gen(), nrow=4)+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = rel(1.2), hjust = 0.5),
          plot.caption = element_text(size = rel(0.8)),
          panel.spacing=unit(0.1, "lines"),
          legend.position="none"
    )+
    labs(
        title="Ohio Public Health Advisory System as Published by the State",
        subtitle="New Rules"
    )

library(patchwork)

(B | A)+plot_annotation(title="Comparison of New and Old Rules for OPHAS", theme = theme(plot.title = element_text(hjust = 0.5, size=20, color="#2C3E50")))



# of indicators 

colors_line<-colors %>%
    distinct(county, published_date, indicator_1_score, indicator_2_score, indicator_3_score, indicator_4_score,
             indicator_5_score, indicator_6_score, indicator_7_score, color) %>% 
    mutate(score=indicator_1_score+indicator_2_score+indicator_3_score+indicator_4_score+
               indicator_5_score+indicator_6_score+indicator_7_score) %>% 
    mutate(color2=case_when(
        score<=1 ~ "Yellow",
        score<=3 ~ "Orange",
        score<=5 ~ "Red",
        score <=7 ~ "Purple"
    ))

cols <- c("Yellow" = "yellow", "Orange" = "orange", "Red" = "red", "Purple"="purple")



colors_line %>% 
    ggplot(aes(x=published_date, y=score, fill=color2, group=county)) +
    geom_col(color="black")+
    facet_wrap(~county, nrow=8)+
    scale_color_manual(values = cols)+
    scale_fill_manual(values = cols)+
    theme_tq()+
    theme(panel.spacing=unit(0.1, "lines"), legend.position="none")+
    labs(
        title="Ohio Public Health Advisory System: Number of Indicators Tripped versus Color Assigned based on Indicators Tripped"
    )