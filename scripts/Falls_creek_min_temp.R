
# Libraries
library(dplyr)
library(ggplot2)

# Dataset
min_temp <- read.csv(here::here("data-raw/Min-temp.csv"))
sapply(min_temp, function(x) sum(is.na(x)))

min_temp_year <- min_temp %>%
  filter(Year > 2014, Year <= 2019) %>%
  filter(Month > 9, Month <12) %>%
  rename(Min_tem = Minimum.temperature..Degree.C.) %>%
  dplyr::select(Year, Month, Day, Min_tem)

sapply(min_temp_year , function(x) sum(is.na(x)))
min_temp_year <- subset(min_temp_year, !is.na(Min_tem))

min_temp_year$Month <- factor(min_temp_year$Month, levels=c('10','11'),
                            labels=c('Oct',
                                     'Nov'))

min_temp_year$Year <-  factor(min_temp_year$Year, levels=c('2015','2016','2017','2018','2019'))

tiff(here::here("results/Min-temp-per-year.jpeg"),width=10,height=12,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
#pdf(here::here("results/Min-temp-per-year.pdf"))
ggplot(min_temp_year,
       aes(x = Day, y = Min_tem, colour = Month )) +
  geom_line() + ggtitle("Minimum daily temperature at Falls Creek during October and November of 2015-2019")+
  facet_wrap(~ Year, nrow = 5)+ 
  xlab("Days")+ ylab("Minimum temperature (°C)") +
  guides(color=guide_legend("Months"))+
  scale_color_manual(values = c('Oct' = 'pink', 'Nov' = 'blue'))+
  scale_y_continuous(breaks = seq(-10, 20, by = 5))
  #geom_text(aes(label = Min_tem))
dev.off()

tiff(here::here("results/Boxplot-Min-temp-per-year.jpeg"),width=10,height=12,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
#pdf(here::here("results/Min-temp-per-Month.pdf"))
ggplot(min_temp_year,
       aes(x = Month, y = Min_tem, colour = Month)) +
  geom_boxplot(fatten = NULL) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid" )+
  ggtitle("Boxplot of the minimum temperature at Falls Creek during October and November of 2015-2019")+
  facet_wrap(~ Year, nrow = 2)+ 
  xlab("Months")+ ylab("Minimum temperature (°C)") +
  guides(color=guide_legend("Months"))+
  geom_text(aes(label = Day), size = 3,
            position=position_jitter(width=0.3,height=0.3))+
  scale_color_manual(values = c('Oct' = 'pink', 'Nov' = 'blue'))
dev.off()



