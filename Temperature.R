
# needed packages
library("ggplot2")
library("lubridate")
library("dplyr")
library("scales")

# import file

TempC<-read.table("Temp_Docs/T_log_1.txt", sep="\t",header=T,
                 stringsAsFactors=F)

#transform Date.Time as POSIXct
TempC$Date.Time <- ymd_hms(TempC$Date.Time)

# exclude dates after 12/07/12

# 120/06/1 <- day 153 of the year
# 12/07/12 <- day 193 of the year

# select time period
TempC<-TempC[ yday( TempC$Date.Time ) >= 153 & yday( TempC$Date.Time ) <= 193, ]

# mark different time periods

TempC$Waterbath <- NA
TempC[ yday( TempC$Date.Time ) <= 156, ]$Waterbath <- "NO"
TempC[ yday( TempC$Date.Time ) > 156, ]$Waterbath <- "YES"

TempC$Phase <- NA
TempC[ yday( TempC$Date.Time ) <= 165, ]$Phase <- "Regrowth"
TempC[ yday( TempC$Date.Time ) > 165 & yday( TempC$Date.Time ) <= 179, ]$Phase <- "Phase_1"
TempC[ yday( TempC$Date.Time ) > 179, ]$Phase <- "Phase_2"

TempC$Phase<-factor(TempC$Phase, levels=c("Regrowth","Phase_1","Phase_2"))

TempC_mean <- TempC %>% 
  group_by(Phase) %>% 
  summarise(mean_T = mean(Temp))

TempC <- left_join(TempC,TempC_mean)

SamplDat <- as.numeric( as.POSIXct( c("2012-06-14", "2012-06-28","2012-07-12"), tz = "GMT"))

graphT<-ggplot(TempC, aes(x=Date.Time, y=Temp, linetype=Waterbath,colour=Phase))+
  geom_line()+
  geom_line(data=TempC, aes(x=Date.Time, y=mean_T))+
  labs(y="Temperature in °C", x="")+
  scale_linetype_manual(values=c("dashed","solid"))+ 
  geom_vline( xintercept = SamplDat, colour = "darkgrey", linetype = "dashed")+
  theme_bw(base_size=12)+
  scale_x_datetime(breaks=date_breaks(width = "2 day"),
                   labels = date_format("%b %d"),
                   limits = as.POSIXct(c('2012-06-01 00:00:00','2012-07-13 00:00:00')))+
  theme(legend.position="none", axis.text.x=element_text(angle=-45, hjust=0))

ggsave(plot = graphT, "figures/Figure_S_2.pdf", width = 6, height = 3)
