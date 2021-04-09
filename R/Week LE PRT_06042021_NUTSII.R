rm(list=ls(all=TRUE))
library(extrafont)
#font_import()
loadfonts()
#fonts()
library(tidyverse)
library(scales)
library(grid)
library(ggthemes)
library(gghighlight)
library(ISOweek)
library(hablar)
library(openxlsx)
library(aweek)
library(matrixStats)
library(hrbrthemes)
library(viridis)
#library(readr)
library(demography)
library(RColorBrewer)
options( scipen = 10 ) # print full numbers, not scientific notation

setwd("/Users/filiperibeiro/Documents/University of Évora 2013to/Articles/New ideas for use/WorkingNow/02 February 2021/LE Week Portugal_NUTSII")
source("LT_function_weekLE.R")

### Good but not detailed below age 1
#### Weekly deaths from Eurostat 
#### adding a repeated column to split 0-5 into 0 & 1-4...
wdeath.EUStat <- read_csv("demo_r_mwk2_05/demo_r_mwk2_05_1_Data.csv", locale = locale(encoding = "ISO-8859-1", grouping_mark = ",")) %>% 
  select(-UNIT, -`Flag and Footnotes`) %>% 
  mutate(GEO = fct_recode(GEO, Centro = "Centro (PT)",
                          AML ="Área Metropolitana de Lisboa",
                          Madeira = "Região Autónoma da Madeira (PT)",
                          Acores = "Região Autónoma dos Açores (PT)"),
         Year=substr(TIME, 1, 4) %>% as.numeric(),
         W1=substr(TIME, 6, 7) %>% as.numeric(),
         W2=ifelse(W1<10, paste0("W0", substr(W1, 1, 1),"-6"), paste0("W", substr(W1, 1, 2),"-6")),
         SEX=ifelse(SEX=="Males", "Male", "Female")) %>% 
  unite(Week, c(Year,W2), sep="-", remove = FALSE) %>% filter(W1 <= 53, TIME <= 2021) %>%
  mutate(Day = ISOweek2date(Week), Value = Value %>% as.numeric()) %>% 
  add_column(Age1=rep(seq(0,90,5), 17536)) %>% 
  mutate(Dupl=ifelse(Age1==0, 1, 0)) %>% 
  uncount(Dupl + 1) %>% add_column(Age=rep(c(0,1,seq(5,90,5)), 17536)) %>% 
  select(SEX, GEO, Age, Year, Week, Day, Value)

wdeath.EUStat %>% filter(GEO=="Portugal" & SEX=="Female" & Year>=2018) %>% 
  mutate(Age=factor(Age,levels=sort(unique(Age), decreasing = TRUE))) %>% 
  ggplot(aes(x=Day, y=Value, fill=Age)) + 
  geom_bar(position="stack", stat="identity", width=7) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "RdYlGn"))(20)) +
  scale_y_continuous(name = "Deaths",
                     limits=c(0,1750), breaks=seq(0,1750,250), labels=label_number()) +
  theme_ipsum(base_family = "Gill Sans MT") + 
  labs(title = "Weekly mortality in Portugal, 2018-2020",
       caption = paste0("Image generated: ", Sys.time(), "\n", "Data source: INE - Statistics Portugal", "\n", "Author: Filipe Ribeiro - fribeiro@uevora.pt")) +
  xlab("")
## don't pay attention to warnings, they're solved ;) 

### Deaths by age and year from INE and Eurostat: merging both
## 90 + as open-end age group
## Split deaths from 0-5 into 0 & 1-4: Stage 1, proportions
death.EUStat <- read_csv("demo_r_magec/demo_r_magec_1_Data.csv", locale = locale(encoding = "ISO-8859-1", grouping_mark = ",")) %>% 
  select(-UNIT, -`Flag and Footnotes`) %>%
  mutate(GEO = fct_recode(GEO, Centro = "Centro (PT)",
                          AML ="Área Metropolitana de Lisboa",
                          Madeira = "Região Autónoma da Madeira (PT)",
                          Acores = "Região Autónoma dos Açores (PT)"),
         SEX=ifelse(SEX=="Males", "Male", "Female"),
         Value = as.numeric(Value)) %>%
  add_column(Age1=rep(0:100, 464)) %>% 
  group_by(Age=cut(Age1, breaks = c(c(0,1,seq(5, 90, by = 5)), Inf), labels = c(0 ,1, seq(5, 90, 5)), right = FALSE) %>% as.character() %>% as.numeric()) %>%
  group_by(TIME, Age, SEX, GEO) %>%
  summarize(Deaths=sum(Value, na.rm = TRUE)) %>% ungroup()

## Deaths from INE
## Need to be merged with previous to get 2019 deaths
# Still need to update
death.ine2019 <- read_csv2("Deaths INE 2019.csv", locale = locale(encoding = "ISO-8859-1")) %>%
  mutate(GEO = fct_recode(GEO, 
                          Portugal = "PT: Portugal",
                          Norte = "11: Norte",
                          Centro = "16: Centro",
                          AML ="17: Área Metropolitana de Lisboa",
                          Alentejo = "18: Alentejo",
                          Algarve = "15: Algarve",
                          Madeira = "3: Região Autónoma da Madeira",
                          Acores = "2: Região Autónoma dos Açores"),
         Value = as.numeric(Value)) %>%
  add_column(Age1=rep(0:120, 16)) %>% 
  group_by(Age=cut(Age1, breaks = c(c(0,1,seq(5, 90, by = 5)), Inf), labels = c(0 ,1, seq(5, 90, 5)), right = FALSE) %>% as.character() %>% as.numeric()) %>%
  group_by(TIME, Age, SEX, GEO) %>%
  summarize(Deaths=sum(Value, na.rm = TRUE)) %>% ungroup()

death.ine2019 %>% head()
death.EUStat %>% head()

## Join both databases
death <- death.EUStat %>% full_join(death.ine2019) %>% 
  filter(TIME>=2000) %>%
  group_by(SEX, GEO, Age) %>% summarize(AvD=mean(Deaths, na.rm = TRUE)) %>% ungroup() %>% 
  group_by(Age.group=cut(Age, breaks = c(seq(0, 90, by = 5), Inf), labels = seq(0, 90, 5), right = FALSE) %>% as.character() %>% as.numeric()) %>% 
  group_by(SEX, GEO, Age.group) %>% 
  mutate(Propd=AvD/sum(AvD)) %>%
  ungroup %>%
  select(SEX, GEO, Age, AvD, Propd)
  
wdeath.EUStat %>% head()
death %>% head()

### Distribute 0-5 age deaths by 0 and 1-4 age groups
## Seems correct and I strongly believe it is :) 
deathsLE <- wdeath.EUStat %>% rename(TIME = Year) %>% 
  left_join(death) %>% 
  group_by(SEX, Age, GEO, Week) %>% 
  mutate(Deathf=Value*Propd) %>%
  group_by(Age)
deathsLE %>% head()

deathsLE %>%  filter(TIME==2020 & GEO=="Alentejo") %>% 
  group_by(GEO,Week, Age, SEX) %>% summarize(Total=sum(Deathf, na.rm = TRUE)) %>% tail(20)
wdeath.EUStat %>% filter(Year==2020 & GEO=="Alentejo") %>% 
  group_by(GEO,Week, Age, SEX) %>% 
  summarize(Total=sum(Value, na.rm = TRUE)) %>% tail(20)

### Population by age and year from INE and Eurostat
### to calculate Exposures
# Still need to update further..
exp.ine <- read_csv2("Pop Est INE 13012021.csv", locale = locale(encoding = "ISO-8859-1")) %>%
  mutate(GEO = fct_recode(GEO, 
                          Portugal = "PT: Portugal",
                          Norte = "11: Norte",
                          Centro = "16: Centro",
                          AML ="17: Área Metropolitana de Lisboa",
                          Alentejo = "18: Alentejo",
                          Algarve = "15: Algarve",
                          Madeira = "3: Região Autónoma da Madeira",
                          Acores = "2: Região Autónoma dos Açores"),
         Value = as.numeric(Value),
         TIME=TIME+1) %>%
  add_column(Age1=rep(0:100, 32)) %>%
  select(-AGE) %>%  filter(TIME>=2020)
exp.ine %>% head()
exp.ine %>% tail()


exp.EUStat <- read_csv("demo_r_d2jan/demo_r_d2jan_1_Data.csv", locale = locale(encoding = "ISO-8859-1", grouping_mark = ",")) %>%
  select(-UNIT, -`Flag and Footnotes`) %>% 
  filter(TIME>=2000) %>%
  mutate(GEO = fct_recode(GEO, Centro = "Centro (PT)",
                          AML ="Área Metropolitana de Lisboa",
                          Madeira = "Região Autónoma da Madeira (PT)",
                          Acores = "Região Autónoma dos Açores (PT)"),
         SEX = ifelse(SEX=="Males", "Male", "Female"),
         Value = Value %>% parse_number(),
         Value = ifelse(is.na(Value), 4810, Value)) %>%
  add_column(Age1=rep(0:100, 320)) %>% 
  select(-AGE)
## don't pay attention to warnings, they're solved ;) 

exp.EUStat %>% head()
exp.EUStat %>% tail()
exp.ine %>% head()
exp.ine %>% tail()

##### Calculating exposures by age group
library(zoo)
exp <- exp.EUStat %>% full_join(exp.ine) %>% 
  group_by(Age=cut(Age1, breaks = c(c(0,1,seq(5, 90, by = 5)), Inf), labels = c(0 ,1, seq(5, 90, 5)), right = FALSE) %>% as.character() %>% as.numeric()) %>%
  group_by(SEX, GEO, TIME, Age) %>%
  summarize(Exposures=sum(Value, na.rm = TRUE)) %>% ungroup() %>% 
  group_by(SEX, GEO, Age) %>%
  mutate(Exposures2=rollapply(Exposures, 2, mean, fill = NA, align = c("left"))) %>% ungroup() %>% 
  group_by(SEX, GEO, TIME, Age) %>%
  transmute(Exposures = coalesce(Exposures2, Exposures)) %>% ungroup()

### Weekly exposures
mort <- deathsLE %>% full_join(exp) %>% select(-AvD, -Propd) %>% filter(TIME>=2018) %>% ### Calculating only 2018+ due to long calculation time...
  mutate(Exp_week=Exposures/(365/7),
         mx=Deathf/Exp_week)
mort %>% head()
mort %>% filter(GEO=="Portugal", TIME==2000, SEX=="Female")
exp %>% filter(GEO=="Portugal", TIME==2000, SEX=="Female")

# Prepare data to run ex
mort2 <- mort %>%
  mutate(ex=9999,
         rsp=paste(GEO,SEX, Week),
         mx=if_else(mx==0, 0.0000001, mx),
         Deathf=if_else(Deathf==0, 0.0000001, Deathf),
         Age=as.numeric(as.character(Age)))
mort2 %>% head()

outputw0 <- mort2 %>% filter(Age==0)
outputw65 <- mort2 %>% filter(Age==65)

# Running ex
# It takes around 30 minutes in my laptop for the 7 regions and years 2000+ ...
# 2018+ takes around 5 minutes... :)
# Algarve, Azores and Madeira are not reliable due to zeros...
for (i in names(table(mort2$rsp))){
  est <- lifetable(x=mort2[mort2$rsp==i,]$Age, 
              Nx=mort2[mort2$rsp==i,]$Exp_week, 
              Dx=mort2[mort2$rsp==i,]$Deathf,
              sex = mort2[mort2$rsp==i,]$SEX[1]) 
  
  outputw0[outputw0$rsp==i,]$ex <- est$ex[1] 
  outputw65[outputw0$rsp==i,]$ex <- est$ex[15]
  print(i)
}

outputw0 %>% head()
outputw0 %>% tail()
outputw0 %>% filter(SEX=="Female", GEO=="Portugal", TIME==2020) %>% tail(20)
outputw0 %>%filter(Day >= "2019-03-01") %>% 
  group_by(TIME, SEX, GEO) %>% 
  summarise(Av_ex = mean(ex))

library(lubridate)
w0 <- outputw0.2020 <- outputw0 %>% 
  mutate(#Day2=if_else(TIME==2020, Day, Day+years(1)),
    Day3 = format(as.Date(Day), "%m-%d"),
    sy = paste(SEX, TIME),
    Day2 = paste0("2020-", Day3) %>% as.Date())
w0 %>% tail(20)

library(hrbrthemes)
library(RColorBrewer)
rbrew_pal <- (brewer.pal(11,"BrBG"))

w0 %>% filter(TIME >=2015 & GEO != "Acores" & GEO != "Madeira") %>% 
  ggplot(aes(x=Day3, y=ex, group=sy, colour=sy)) +
  geom_line() + geom_point() + facet_wrap(. ~ GEO)

Sys.getlocale("LC_TIME") # "pt_PT.UTF-8"
curr_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","en_US.UTF-8")


w0 %>% filter(TIME >= 2018 & GEO !="Acores" & GEO != "Madeira") %>% 
  ggplot(aes(x=Day2, y=ex, group=sy, colour=sy)) +
  geom_line() + geom_point(size=2.5, shape=17) + facet_wrap(. ~ GEO) + geom_smooth() +
  scale_y_continuous(name = "Life Expectancy at birth (by week, in years)",
                     limits=c(70,90), breaks=seq(70,90,2.5), labels=label_number()) +
  theme_ipsum(base_family = "Gill Sans MT", base_size = 14, axis_title_size = 13) + 
  scale_colour_brewer(name = "", type = "div", palette = 1, direction = -1, aesthetics = "colour") +
  labs(title = "Weekly life expectancy at birth by sex, in Portugal", #\nWeeks 1 to 48, 2020
       caption = paste0("Image generated: ", Sys.time(), "\n", "Data source: Statistiscs Portugal & Eurostat", "\n", "Author: Filipe Ribeiro - fribeiro@uevora.pt")) +
  theme_ipsum(base_family = "Gill Sans MT") +
  theme(legend.background = element_rect(fill = "transparent", color="transparent"),
        legend.position = "bottom",
        legend.box = "horizontal") +
  guides(size=FALSE, shape=FALSE)
#ggsave("LEweek_04012021_2.pdf", width = 30, height = 27.5, units = "cm", device=cairo_pdf)

