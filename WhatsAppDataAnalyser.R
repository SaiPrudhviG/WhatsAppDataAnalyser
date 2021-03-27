library(rwhatsapp)
##Reads the whatsapp chat data 
 
v_Chats<-rwa_read("WhatsAppChatwithHAPPY.txt")

##Displays the Data
head(v_Chats,5)

#displays the structure
str(v_Chats)

##Message the data
library(dplyr)
library(lubridate)
v_Chats <- v_Chats %>%
  mutate(message_date=date(time)) %>%
  mutate(message_month=month(time,label=TRUE)) %>%
  mutate(message_month=factor(message_month)) %>%
  mutate(message_weekday_number=mday(message_date)) %>% 
  mutate(message_weekday_name=weekdays(message_date))%>%
  mutate(message_weekday_name=factor(message_weekday_name)) %>%
  mutate(message_hour=hour(time)) %>%
  filter(!is.na(author))
head(v_Chats,10)

##load ggplot2 and realted libraries
library(ggplot2)
library(ggthemes)
library(viridis)

v_Chats %>%
  group_by(message_month) %>%
  count(message_date) %>%
  ggplot(aes(x=message_month,y=n,fill=message_month))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete=TRUE)+
  labs(x="Date",y="Number of Messages",fill="Month- ")+
  ggtitle("Messages per day","Frequency of Message/Day \n Grouped by Month")+
  theme_minimal()+
  theme(legend.title = element_blank(),legend.position = "bottom")

v_Chats %>%
  group_by(message_month)%>%
  count(message_date)%>%
  ggplot(aes(x=message_month,y=n,fill=message_month))+
  geom_bar(stat="identity")+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Date",y="Number of Messages",fill="Month- ")+
  theme_minimal()+
  theme(legend.title =element_text(color="blue",size=9,face="bold"),
        legend.text=element_text(color="red",size=7),
        legend.position = "bottom",
        legend.key.size = unit(0.3,"cm"),
        legend.key.width = unit(0.3,"cm"),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7),
        axis.title = element_text(size=9),
        plot.title=element_text(size=16),
        plot.subtitle = element_text(size=10))
