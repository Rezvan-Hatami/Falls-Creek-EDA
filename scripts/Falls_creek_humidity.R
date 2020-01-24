
# Libraries
library(dplyr)
library(ggplot2)

# Dataset
humid <- read.csv(here::here("data-raw/humidity.csv"))
sapply(humid, function(x) sum(is.na(x)))

humid_year <- humid %>%
  rename(hum_percen = Relative.humidity.in.percentage..,
         Year = Year.Month.Day.Hour.Minutes.in.YYYY,
         Month = MM,
         Day = DD
         ) %>%
  filter(Year > 2014, Year <= 2019) %>%
  filter(Month > 9, Month <12) %>%
  dplyr::select(Year, Month, Day, hum_percen)

sapply(humid_year , function(x) sum(is.na(x)))
humid_year <- subset(humid_year, !is.na(hum_percen))

humid_day_ave <-
  with(humid_year,
       aggregate(x = list(hum_ave = hum_percen),
                 by = list(Year = Year,
                           Month = Month,
                           Day = Day),
                 FUN = mean))


humid_day_ave$Month <- factor(humid_day_ave$Month, levels=c('10','11'),
                            labels=c('Oct',
                                     'Nov'))

humid_day_ave$Year <-  factor(humid_day_ave$Year, levels=c('2015','2016','2017','2018','2019'))

tiff(here::here("results/humidity-per-year.jpeg"),width=10,height=14,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
#pdf(here::here("results/humidity.pdf"))
ggplot(humid_day_ave,
       aes(x = Day, y = hum_ave, colour = Month )) +
  geom_line() + ggtitle("Average daily humidity at Falls Creek during October and November of 2015-2019")+
  facet_wrap(~ Year, nrow = 5)+ 
  xlab("Days")+ ylab("Humidity (%)") +
  guides(color=guide_legend("Months"))+
  #geom_text(aes(label = hum_ave))+
  scale_color_manual(values = c('Oct' = 'dark green', 'Nov' = 'blue'))
dev.off()

#pdf(here::here("results/Humidity-per-Month.pdf"))
tiff(here::here("results/Boxplot-Humidity-per-year.jpeg"),width=10,height=12,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
ggplot(humid_day_ave,
       aes(x = Month, y = hum_ave, colour = Month)) +
  geom_boxplot(fatten = NULL) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid" )+
  ggtitle("Boxplot of the humidity percentage at Falls Creek during October and November of 2015-2019")+
  facet_wrap(~ Year, nrow = 2)+ 
  xlab("Months")+ ylab("Humididty (%)") +
  guides(color=guide_legend("Months"))+
  geom_text(aes(label = Day), size = 2, position=position_jitter(width=0.3,height=0.3))+
  scale_color_manual(values = c('Oct' = 'dark green', 'Nov' = 'blue'))
dev.off()
