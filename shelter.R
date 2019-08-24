setwd("C:/Users/derri/Onedrive/r")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
train <- read.csv('train.csv', stringsAsFactors = F)

train$OutcomeType[train$OutcomeType == "Return_to_owner"] <- "Return to Owner" 
train$age  <- train$AgeuponOutcome  
train$age[grepl("day", train$AgeuponOutcome)] <- "< a week" 
train$age[grepl("week", train$AgeuponOutcome)] <- "< a month"  
filter(train, age != "") -> train 
filter(train, age != "0 years") -> train  
train$age[train$age %in% c("13 years", "14 years", "15 years", "16 years", "17 years", "18 years",
                           "19 years", "20 years")] <- "> 12 years"    
train$age <- factor(train$age, level = c("< a week", "< a month", 
                                         "1 month", "2 months", "3 months", "4 months", "5 months", "6 months" ,"7 months",
                                         "8 months", "9 months", "10 months", "11 months",                                          
                                         "1 year", "2 years", "3 years", "4 years", "5 years",                                          
                                         "6 years", "7 years", "8 years", "9 years", "10 years", "11 years",                                          
                                         "12 years", "> 12 years"))

cat <- filter(train, AnimalType == "Cat")

cat %>% count(age) %>%       
  ggplot(aes(x = age, y = n)) +         
  geom_bar(stat = "identity", alpha = 0.5) +
  theme_void() +          
  ylab("Count") +          
  ggtitle("Outcome Type by Age upon Outcome - Cats") +         
  theme(axis.title.y = element_text(angle = 90, color = "#737373" )) -> g1

ggplot(cat, aes(x = age, fill = OutcomeType)) +
  geom_bar(stat = "count", position = "fill",  alpha = 0.9) +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  ylab("Relative Count by Outcome") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11, color = "#737373"),
        legend.position = "bottom",
        axis.title.y = element_text(angle = 90, color = "#737373")) -> g2  

grid.arrange(g1, g2, ncol = 1, heights = c(1,3))

gsub(" Mix", "", train$Breed) -> temp
strsplit(x = temp, split = "/") %>% sapply(function(x){x[1]}) -> train$breed1
train %>% 
  count(breed1) %>% 
  arrange(desc(n)) %>%  head(20) %>% 
  ggplot(aes(x = reorder(breed1, n), y = n)) +
  geom_bar(stat = "identity", width = 0.9,col='orange',fill='lightblue') +
  coord_flip() + 
  theme(axis.title.y = element_blank()) +
  ggtitle("Popular Breeds of Cats") +
  ylab("Numbers")

filter(train, AnimalType == "Cat") %>% 
  count(breed1) %>% 
  arrange(desc(n)) %>%  head(10) %>% 
  ggplot(aes(x = reorder(breed1, n), y = n)) +
  geom_bar(stat = "identity", width = 0.9,col='orange',fill='lightblue') +
  coord_flip() + 
  theme(axis.title.y = element_blank()) +
  ggtitle("Popular Breeds of Cats") +
  ylab("Numbers")

filter(train, AnimalType == "Dog") %>% 
  count(breed1) %>% 
  arrange(desc(n)) %>%  head(10) %>% 
  ggplot(aes(x = reorder(breed1, n), y = n)) +
  geom_bar(stat = "identity", width = 0.9,col='orange',fill='lightblue') +
  coord_flip() + 
  theme(axis.title.y = element_blank()) +
  ggtitle("Popular Breeds of Dogs") +
  ylab("Numbers")

count(train, breed1) %>%
  arrange(desc(n)) %>%
  filter(n >150) -> popular

train$breed1[!(train$breed1 %in% popular$breed1)] <- "Others"

filter(train, AnimalType == "Cat") %>% 
  ggplot(aes(x = breed1,fill = OutcomeType)) +
  geom_bar(stat = "count", position = position_fill(reverse = FALSE), width = 0.8) + 
  coord_flip() +
  ggtitle(" Cat Breeds by Outcome1") + 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title = element_blank()) -> g1

filter(train, AnimalType == "Cat") %>% 
  ggplot(aes(x = breed1,fill = OutcomeType)) +
  geom_bar(stat = "count", position = position_stack(reverse = FALSE), width = 0.8) + 
  coord_flip() +
  ggtitle(" Cat Breeds by Outcome2") + 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title = element_blank()) -> g2

grid.arrange(g1, g2, ncol = 1, heights = c(2,1))

filter(train, AnimalType == "Dog") %>% 
  ggplot(aes(x = breed1,fill = OutcomeType)) +
  geom_bar(stat = "count", position = position_fill(reverse = FALSE), width = 0.8) + 
  coord_flip() +
  ggtitle(" Dog Breeds by Outcome1") + 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title = element_blank()) -> g3

filter(train, AnimalType == "Dog") %>% 
  ggplot(aes(x = breed1,fill = OutcomeType)) +
  geom_bar(stat = "count", position = position_stack(reverse = FALSE), width = 0.8) + 
  coord_flip() +
  ggtitle(" Dog Breeds by Outcome2") + 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title = element_blank()) -> g4

grid.arrange(g3, g4, ncol = 1, heights = c(1,1))

strsplit(x = train$Color, split = "/") %>% sapply(function(x){x[1]}) -> train$color1
cats <- filter(train, AnimalType == "Cat")

cats %>% count(color1) %>% arrange(desc(n)) %>% 
  filter(n > 450) -> cat_colors

cats$color1[!(cats$color1 %in% cat_colors$color1)] <- "Other"

ggplot(cats, aes(x = color1, fill = OutcomeType)) +
  geom_bar(stat = "count", position = "fill", width = 0.8) + 
  ggtitle("Cat Colors by Outcome") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title = element_blank(),legend.position = "none") -> g5

dogs <- filter(train, AnimalType == "Dog")

dogs %>% count(color1) %>% arrange(desc(n)) %>% 
  filter(n > 450) -> dog_colors

dogs$color1[!(dogs$color1 %in% dog_colors$color1)] <- "Other"

ggplot(dogs, aes(x = color1, fill = OutcomeType)) +
  geom_bar(stat = "count", position = "fill", width = 0.9) + 
  coord_flip() +
  ggtitle("Dog Colors by Outcome") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title = element_blank()) -> g6

grid.arrange(g5,g6,ncol=1,heights=c(1,1))

grid.arrange(g5,g6,nrow=1,widths=c(3,4))
