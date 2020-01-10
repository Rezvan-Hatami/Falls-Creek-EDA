
# Libraries
library(dplyr)
library(ggplot2)

# Dataset
max_temp <- read.csv(here::here("data-raw/Max-temp.csv"))
sapply(max_temp, function(x) sum(is.na(x)))

max_temp_year <- max_temp %>%
  filter(Year > 2014, Year <= 2019) %>%
  filter(Month > 9, Month <12) %>%
  rename(Max_tem = Maximum.temperature..Degree.C.) %>%
  dplyr::select(Year, Month, Day, Max_tem)

sapply(max_temp_year , function(x) sum(is.na(x)))
max_temp_year <- subset(max_temp_year, !is.na(Max_tem))

max_temp_year$Month <- factor(max_temp_year$Month, levels=c('10','11'),
                            labels=c('Oct',
                                     'Nov'))

max_temp_year$Year <-  factor(max_temp_year$Year, levels=c('2015','2016','2017','2018','2019'))

pdf(here::here("results/Max-temp-per-year.pdf"))
ggplot(max_temp_year,
       aes(x = Day, y = Max_tem, colour = Month )) +
  geom_line() + ggtitle("Maximum daily temperature at Falls Creek")+
  facet_wrap(~ Year, nrow = 5)+ 
  xlab("Days")+ ylab("Maximum temperature (°C)") +
  guides(color=guide_legend("Months"))
  #geom_text(aes(label = Max_tem))
dev.off()

pdf(here::here("results/Max-temp-per-Month.pdf"))
ggplot(max_temp_year,
       aes(x = Month, y = Max_tem, colour = Month)) +
  geom_boxplot(fatten = NULL) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid" )+
  ggtitle("Boxplot of the maximum temperature at Falls Creek")+
  facet_wrap(~ Year, nrow = 2)+ 
  xlab("Months")+ ylab("Maximum temperature (°C)") +
  guides(color=guide_legend("Months"))+
  geom_text(aes(label = Day), size = 2, position=position_jitter(width=0.3,height=0.3))
dev.off()
