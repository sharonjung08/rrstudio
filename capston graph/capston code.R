data<-read.table(
  "/Users/seo-yun/Downloads/United States and Puerto Rico Cancer Statistics, 1999-2019 Incidence.txt", 
  header=TRUE, sep = "\t")

View(data)

install.packages("tidyverse")
library(tidyverse)

data <- data %>% subset(select = -c(Notes, Year.Code))
##-c = delete something / c = insert something

colnames(data)
data <- data %>% subset(select = c(Leading.Cancer.Sites, Leading.Cancer.Sites.Code, Year, Age.Groups.Code, Count))
View(data)

datas <- data %>% select(Count, Year) %>% group_by(Year) %>% summarise(sumCount=sum(Count))
View(datas)

library(ggplot2)
ggplot(datas,aes(x=Year, y=sumCount)) + geom_line() + 
  labs(title="Cancer Count Over Years", y = "Cancer Incidence")

###### HW ######
## only breast cancer
datah <- data%>%filter(Leading.Cancer.Sites=="Breast") %>% group_by(Year) %>% summarise(sumCOunt=sum(Count))
View(datah)                         
                         
                         
library(ggplot2)
ggplot(datah,aes(x=Year, y=sumCOunt)) + geom_line() + 
  labs(title="Breast Cancer Count Over Years", y = "Cancer Incidence")

####################
## All Year Incidence Data

AllYear <- data%>% group_by(Leading.Cancer.Sites,Leading.Cancer.Sites.Code,Age.Groups.Code,Count) %>%
  summarize(sumCOUnt=sum(Count))

View(AllYear)

ggplot(AllYear,aes(x=Age.Groups.Code, y=sumCOUnt)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(title="Cancer Incidence Recent 20 Years by age group", x = "Age", y = "Cancer Incidence")
##dodge=bar graph vertically (not horizontal), tat = "identity: 쌓아서 보여준다 

ggplot(AllYear,aes(x=Age.Groups.Code, y=sumCOUnt, fill=Leading.Cancer.Sites)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(title="Cancer Incidence Recent 20 Years by age group", x = "Age", y = "Cancer Incidence")

########################
unique(AllYear$Age.Groups.Code) ##unique/distinct info

AllYearDecade <- AllYear%>% 
  mutate(Age.Groups.Code = case_when(
    grepl("1",Age.Groups.Code)~"< 10 year old", 
    grepl("1-4",Age.Groups.Code)~"< 10 year old", 
    grepl("5-9",Age.Groups.Code)~"< 10 year old", 
    grepl("10-14",Age.Groups.Code)~"10s", 
    grepl("15-19",Age.Groups.Code)~"10s", 
    grepl("20-24",Age.Groups.Code)~"20s",
    grepl("25-29",Age.Groups.Code)~"20s",
    grepl("30-34",Age.Groups.Code)~"30s",
    grepl("35-39",Age.Groups.Code)~"30s",
    grepl("40-44",Age.Groups.Code)~"40s",
    grepl("45-49",Age.Groups.Code)~"40s",
    grepl("50-54",Age.Groups.Code)~"50s",
    grepl("55-59",Age.Groups.Code)~"50s",
    grepl("60-64",Age.Groups.Code)~"60s",
    grepl("65-69",Age.Groups.Code)~"60s",
    grepl("70-74",Age.Groups.Code)~"70s",
    grepl("75-79",Age.Groups.Code)~"70s",
    grepl("80-84",Age.Groups.Code)~"80+",
    grepl("85+",Age.Groups.Code)~"80+",
    TRUE ~ Age.Groups.Code
    ))
View(AllYearDecade)

ggplot(AllYearDecade,aes(x=Age.Groups.Code, y=sumCOUnt, fill=Leading.Cancer.Sites)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(title="Cancer Incidence Recent 20 Years by age group", x = "Age", y = "Cancer Incidence")

#SelectOnlyTop5CancerSites 
Top5Year<-AllYearDecade%>%
  group_by(Leading.Cancer.Sites)%>% 
  summarize(sumCOUNt=sum(sumCOUnt), .groups = "drop") %>%
  arrange(desc(sumCOUNt))%>%
  head(5)

View(Top5Year)

Top5Code<-factor(Top5Year$Leading.Cancer.Sites)
to_graph_data <- AllYearDecade %>%
  filter(Leading.Cancer.Sites %in% as.character(Top5Code)) %>%
  mutate(Leading.Cancer.Sites = factor(Leading.Cancer.Sites, levels = levels(Top5Code)))  
  
View(to_graph_data)
ggplot(to_graph_data,aes(x=Age.Groups.Code, y=sumCOUnt, fill=Leading.Cancer.Sites)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title="Top 5 Cancer Incidence Recent 20 Years by age group", 
       x = "Age", y = "Cancer Incidence", fill = "Leading.Cancer.Sites")

getwd()
