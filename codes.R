library(tidyverse)
library(dplyr)
happiness_data <- read_csv(("D:\\Courses\\MATH 208\\Final Project\\Project_Happiness_data.csv"))
data_2019 <- happiness_data %>% filter(Year == 2019)
mean_for_each_region <- data_2019 %>% 
  group_by(Region) %>%  summarise(Mean_GDPperCap = mean(GDPperCap),
                                  Mean_TrustGov = mean(TrustGov),
                                  Mean_Family = mean(Family),
                                  Mean_Freedom = mean(Freedom),
                                  Mean_HealthLifeExp = mean(HealthLifeExp),
                                  Mean_Generosity = mean(Generosity))
mean_for_each_region <- mean_for_each_region %>% 
  pivot_longer(cols = c("Mean_GDPperCap", "Mean_TrustGov",
                        "Mean_Family","Mean_Freedom",
                        "Mean_HealthLifeExp","Mean_Generosity"),
               names_to = "Scores")
mean_for_each_region %>% ggplot(aes(x = Region, fill = Region))+
  geom_bar(position = "dodge")  + facet_wrap(~Scores)+ 
  coord_cartesian(ylim = c(0, 0.001))+
  scale_fill_viridis_d() + ylab("Mean Scores")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



#Task1
new_data_2019 <- data_2019 %>% 
  select( c("Region", "GDPperCap","TrustGov", "Family","Freedom",
            "HealthLifeExp","Generosity"))%>%
  pivot_longer(cols = c("GDPperCap", "TrustGov",
                        "Family","Freedom",
                        "HealthLifeExp","Generosity"),
               names_to = "Scores")
new_data_2019 %>% ggplot(aes(x=Region,fill = Region, y = value))+
  geom_boxplot()+facet_wrap(~Scores) + labs(x="")+
  theme(axis.text.x = element_blank())

#Task2
total_happiness <- happiness_data %>% group_by(Country,Year)%>% 
  summarise(Region,TotalScore = sum(GDPperCap,TrustGov,Family,
            Freedom,HealthLifeExp,Generosity))
extreme <- total_happiness %>% group_by(Region,Year)%>%
  summarise(Max = max(TotalScore), Min = min(TotalScore))
extreme_for_plot <- extreme %>% 
  pivot_longer(cols=c(Max, Min), names_to = "Variable")
extreme_for_plot %>% group_by(Region) %>%
  ggplot(aes(x=Year,y=value,col= Variable))+
  geom_line(na.rm=TRUE)+geom_point(na.rm=TRUE)+facet_wrap(~Region)
 
average_by_country<-total_happiness %>% group_by(Country) %>% 
  mutate(Average = mean(TotalScore))%>% ungroup()%>%
  summarise(Country,Average)%>%arrange(desc(Average))%>%
  unique()


tbl_for_diff <- total_happiness %>% 
  filter(Year == 2015 | Year == 2019)%>%
  pivot_wider(id_cols=Country,names_from=Year,
              values_from=TotalScore)
diff_happiness <- tbl_for_diff %>%
  mutate(Diff = pull(tbl_for_diff,"2019") - 
           pull(tbl_for_diff,"2015")) %>%
  summarise(Country,Diff)%>%arrange(desc(Diff))



