
# Libraries
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(psych)
library(MASS)

source("scripts/Falls_creek_max_temp.R")
source("scripts/Falls_creek_min_temp.R")

# Dataset
Solar_exposure <- read.csv(here::here("data-raw/Solar-exposure.csv"))
sapply(Solar_exposure, function(x) sum(is.na(x)))

Solar_exposure_year <- Solar_exposure %>%
  filter(Year > 2014, Year <= 2019) %>%
  filter(Month > 9, Month <12) %>%
  rename(solar = Daily.global.solar.exposure..MJ.m.m.) %>%
  dplyr::select(Year, Month, Day, solar)

sapply(Solar_exposure_year  , function(x) sum(is.na(x)))
Solar_exposure_year <- subset(Solar_exposure_year , !is.na(solar))

Solar_exposure_year$Month <- factor(Solar_exposure_year$Month, levels=c('10','11'),
                            labels=c('Oct',
                                     'Nov'))

Solar_exposure_year$Year <-  factor(Solar_exposure_year$Year, levels=c('2015','2016','2017','2018','2019'))

#pdf(here::here("results/solar.pdf"))
tiff(here::here("results/solar-per-year.jpeg"),width=10,height=12,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
ggplot(Solar_exposure_year,
       aes(x = Day, y = solar, colour = Month )) +
  geom_line() + ggtitle("Daily global solar exposure at Falls Creek during October and November of 2015-2019")+
  facet_wrap(~ Year, nrow = 5)+ 
  xlab("Days")+ ylab("Daily global solar exposure (MJ/m2)") +
  guides(color=guide_legend("Months"))+
  scale_color_manual(values = c('Oct' = 'purple', 'Nov' = 'gold'))
  #geom_text(aes(label = Max_tem))
dev.off()

#pdf(here::here("results/solar-boxplot.pdf"))
tiff(here::here("results/solar-boxplotr.jpeg"),width=10,height=12,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
ggplot(Solar_exposure_year,
       aes(x = Month, y = solar, colour = Month)) +
  geom_boxplot(fatten = NULL) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid" )+
  ggtitle("Boxplot of the daily global solar exposure at Falls Creek during October and November of 2015-2019")+
  facet_wrap(~ Year, nrow = 2)+ 
  xlab("Months")+ ylab("Daily global solar exposure (MJ/m2)") +
  guides(color=guide_legend("Months"))+
  geom_text(aes(label = Day), size = 2,
            position=position_jitter(width=0.3,height=0.3))+
  scale_color_manual(values = c('Oct' = 'purple', 'Nov' = 'orange'))
dev.off()


## Explore potential relationship between solar radiation and temperature

combine <- full_join(min_temp_year, max_temp_year, by = c("Year", "Month", "Day"))
combine1 <- left_join(combine, Solar_exposure_year, by = c("Year", "Month", "Day"))
sapply(combine1 , function(x) sum(is.na(x)))
combine1 <- combine1[complete.cases(combine1), ]

png(here::here("results/correlation.png"))
my_data <- combine1[, c(4,5,6)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
#pairs.panels(my_data)
dev.off()

## Scatterplot (Compare max temperature with minimum temperature)
df <- combine1 
y <- combine1$Max_tem
x <- combine1$Min_tem
m <- lm(Max_tem ~ Min_tem, data = combine1)

#pdf(here::here("results/Overal-correlation-max-min.pdf"))
tiff(here::here("results//Overal-correlation-max-min.jpeg"),width=10,height=10,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
p1 <- ggplot(df, aes(x, y))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ggtitle("The relationship between maximum and minimum temperature
          at Falls Creek during October and November of 2015-2019")+
  xlab("Minimum temperature (°C)")+ ylab("Maximum temperature (°C)") 

eq <- substitute(italic(r)^2~"="~r2,
                 list(r2 = format(summary(m)$r.squared, digits = 2)))
dftext <- data.frame(x = 11, y = 7, eq = as.character(as.expression(eq)))
p2 <- p1 + geom_text(aes(label = eq), data = dftext, parse = TRUE)
p2
dev.off() 

#pdf(here::here("results/correlation_max-min-year.pdf"))
tiff(here::here("results//correlation_max-min-year.jpeg"),width=10,height=10,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
ggplot(combine1, aes(Min_tem, Max_tem))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ggtitle("The relationship between maximum and minimum temperature per year 
          at Falls Creek during October and November of 2015-2019") +
  facet_wrap(~ Year, nrow = 2)+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  xlab("Minimum temperature (°C)")+ ylab("Maximum temperature (°C)") 
dev.off()

#pdf(here::here("results/correlation-uncertainty_per_year_linear.pdf"))
#ggplot(combine1, aes(Min_tem, Max_tem))+
  #geom_point()+
  #ggtitle("The relationship between maximum and minimum temperature per year \n
          #at Falls Creek during October and November of 2015-2019 (linear regression")+
  #facet_wrap(~ Year, nrow = 2)+
  #geom_smooth(method = "rlm")+
  #xlab("Minimum temperature (°C)")+ ylab("Maximum temperature (°C)") 
#dev.off()

## Compare max temperature with minimum of the previous day
min_temp_year1 <- min_temp_year %>%
  mutate(Day = Day - 1)

df <- inner_join(min_temp_year1, max_temp_year, by = c("Year", "Month", "Day"))  
y <- df$Max_tem
x <- df$Min_tem
m <- lm(Max_tem ~ Min_tem, data = df)

#pdf(here::here("results/overal_correlation_min_before.pdf"))
tiff(here::here("results//overal_correlation_min_before.jpeg"),width=10,height=10,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")

p1 <- ggplot(df, aes(x, y))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ggtitle("The relationship between maximum and previous-day minimum temperature \n
          at Falls Creek during October and November of 2015-2019")+
  xlab("Previous-day minimum temperature (°C)")+ ylab("Maximum temperature (°C)")

eq <- substitute(italic(r)^2~"="~r2,
                 list(r2 = format(summary(m)$r.squared, digits = 2)))
dftext <- data.frame(x = 11, y = 7, eq = as.character(as.expression(eq)))
p2 <- p1 + geom_text(aes(label = eq), data = dftext, parse = TRUE)
p2
dev.off() 

## Compare min temperature with maximum of the previous day
max_temp_year1 <- max_temp_year %>%
  mutate(Day = Day - 1)

df1 <- inner_join(max_temp_year1, min_temp_year, by = c("Year", "Month", "Day"))  
y1 <- df1$Max_tem
x1 <- df1$Min_tem
m1 <- lm(Max_tem ~ Min_tem, data = df1)

#pdf(here::here("results/overal_correlation_max_before.pdf"))
tiff(here::here("results//overal_correlation_max_before.jpeg"),width=10,height=10,
     units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")

p1 <- ggplot(df, aes(x, y))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ggtitle("The relationship between minimum and previous-day maximum temperature \n
          at Falls Creek during October and November of 2015-2019")+
  xlab("Minimum temperature (°C)")+ ylab("Previous-day maximum temperature (°C)")

eq <- substitute(italic(r)^2~"="~r2,
                 list(r2 = format(summary(m)$r.squared, digits = 2)))
dftext <- data.frame(x = 11, y = 7, eq = as.character(as.expression(eq)))
p2 <- p1 + geom_text(aes(label = eq), data = dftext, parse = TRUE)
p2
dev.off() 

