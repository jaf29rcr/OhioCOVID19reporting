```{r}
library(tidyverse)
library(readxl)
library(lubridate)
library(maps)
library(tidyquant)
library(fs)

# Read in reference files (County and Age Population Info)

# Key to map County to Hospital Region, also has county population info

Region<-read_excel("Dashboard/Region.xlsx") 

# Ohio population by Age Range

Age<-read_excel("Dashboard/OhioPop.xlsx") %>% mutate(Population=Population/100000)


# Read in Ohio Covid-19 CSV file, file path keeps changing so keeping record of old links

# Old Link https://coronavirus.ohio.gov/static/COVIDSummaryData.csv
# Old Link2 https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv
# Old Link3 https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv"

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


# Create shell of all dates between 1/1/20 and today 

dates<-tibble::enframe(seq(as.Date("2020/1/1"), today(), "days")) %>% 
    rename(Date=value) %>% 
    select(Date)

# Merge dates with info

Region_Date<-COVID %>% distinct(Region) %>% crossing(dates)

Age_Date<-COVID %>% distinct(`Age Range`) %>% crossing(dates)

# Get total population for Ohio

Ohio_Pop<-Region %>% summarize(Population=sum(Population)/100000) %>% pull() %>% pluck()

# Population by Region

Region_Pop<-Region %>%
    mutate(Region=case_when(
        Region==1 ~ "1. Toledo",
        Region==2 ~ "2. Cleveland",
        Region==3 ~ "3. Dayton",
        Region==4 ~ "4. Columbus",
        Region==5 ~ "5. Akron",
        Region==6 ~ "6. Cincinnati",
        Region==7 ~ "7. SE Ohio (Athens)",
        TRUE ~ "8. SE Ohio (Zanesville)"))%>% 
    group_by(Region) %>% 
    summarize(Population=sum(Population)/100000) %>% 
    ungroup()


State_Plot<-COVID %>% 
    filter(`Onset Date` != "Unknown") %>% 
    filter(!is.na(`Onset Date`)) %>% 
    group_by(`Onset Date`) %>% 
    summarize(Cases=sum(`Case Count`)) %>% 
    ungroup() %>% 
    mutate(Date=ymd(`Onset Date`)) %>% 
    filter(Date<=today()-days(2) & Date>=today()-days(15)) %>% 
    summarize(Cases=sum(Cases)/Ohio_Pop) %>% 
    ggplot(aes(x="Ohio", y=Cases))+
    geom_col(fill="#2C3E50")+
    geom_hline(aes(yintercept=50), color="red")+
    scale_y_continuous(limits=c(0,300))+
    geom_label(aes(label=Cases%>%scales::number(accuracy=0.1)))+
    theme_tq()+
    labs(
        title="Entire State",
        x="", y=""
    )

Region_plot<-COVID %>% 
    filter(`Onset Date` != "Unknown") %>% 
    filter(!is.na(`Onset Date`)) %>% 
    group_by(Region, `Onset Date`) %>% 
    summarize(Cases=sum(`Case Count`)) %>% 
    ungroup() %>% 
    mutate(Date=ymd(`Onset Date`)) %>% 
    filter(Date<=today()-days(2) & Date>=today()-days(15)) %>% 
    inner_join(Region_Pop) %>% 
    group_by(Region, Population) %>% 
    summarize(Cases=sum(Cases)) %>% 
    ungroup() %>% 
    mutate(Cases=Cases/Population) %>% 
    mutate(Region=Region %>% fct_rev()) %>% 
    ggplot(aes(x=Region, y=Cases))+
    geom_col(fill="#2C3E50")+
    geom_hline(aes(yintercept=50), color="red")+
    scale_y_continuous(limits=c(0,300))+
    coord_flip() +
    geom_label(aes(label=Cases%>%scales::number(accuracy=0.1)))+
    theme_tq()+
    labs(
        title="Region",
        x="", y=""
    )

Age_plot<-COVID %>% 
    filter(`Onset Date` != "Unknown") %>% 
    filter(!is.na(`Onset Date`)) %>% 
    group_by(`Age Range`, `Onset Date`) %>% 
    summarize(Cases=sum(`Case Count`)) %>% 
    ungroup() %>% 
    mutate(Date=ymd(`Onset Date`)) %>% 
    filter(Date<=today()-days(2) & Date>=today()-days(15)) %>% 
    inner_join(Age) %>% 
    group_by(`Age Range`, Population) %>% 
    summarize(Cases=sum(Cases)) %>% 
    ungroup() %>% 
    mutate(Cases=Cases/Population) %>% 
    mutate(`Age Range`=`Age Range` %>% fct_rev()) %>% 
    ggplot(aes(x=`Age Range`, y=Cases))+
    geom_col(fill="#2C3E50")+
    geom_hline(aes(yintercept=50), color="red")+
    scale_y_continuous(limits=c(0,300))+
    coord_flip() +
    geom_label(aes(label=Cases%>%scales::number(accuracy=0.1)))+
    theme_tq()+
    labs(
        title="Age Range",
        x="", y=""
    )

library(patchwork)

(State_Plot | Region_plot | Age_plot) + plot_annotation(title="Freedom Tracker", subtitle="Cases per 100K over last two weeks")