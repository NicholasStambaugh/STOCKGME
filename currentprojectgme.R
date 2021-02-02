library(quantmod)
library(ggplot2)

#formatting
gme <- getSymbols("GME", src = "yahoo",
                  from = "2021-01-01", to = "2021-02-01",
                  auto.assign = FALSE)
#more formatting
gme[,1] <- as.Date(gme[,1])

gme <- xts(gme)

gme<-gme[,1]

#basic visualizing

head(gme)

tail(gme)

summary(gme)

str(gme)

chart_Series(nee)

#Plot daily price

ggplot(gme, aes(x = index(gme), y = nee[,6])) +
  geom_line(color = "red") +
  ggtitle("GME Price Series Since JAN 04") +
  xlab("Date") + ylab("Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%b/%d")

#moving average

gme_mm <-subset(gme, index(gme) >="2020-01-04")

gme_mm10 <- rollmean(gme_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
gme_mm30 <- rollmean(gme_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")

gme_mm$mm10 <- coredata(gme_mm10)
gme_mm$mm30 <- coredata(gme_mm30)

#Plotting moving average
#Short Run crosses long run going upwards = BUY
#Long Run crosses short downwards = SELL

ggplot(gme_mm, aes(index(gme_mm))) +
  geom_line(aes(y = gme_mm[,6], color = "GME")) + ggtitle("GME Short/Long Run Price Series") +
  geom_line(aes(y= gme_mm$mm10, color = "Short Run")) +
  geom_line(aes(y= gme_mm$mm30, color = "Long Run")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank())+
  scale_x_date(date_labels = "%b/%A") +
  scale_colour_manual("Series", values = c("GME"="gray40", "Short Run"="firebrick4", "Long Run"="darkcyan"))

#Returns
gme_ret <-diff(log(gme[,6]))
gme_ret <- gme_ret[-1,]

sd(gme_ret)
summary(gme_ret)

#plot returns
ggplot(gme_ret, aes(x = index(gme_ret), y = gme_ret)) +
  geom_line(color = "deepskyblue3") +
  ggtitle("Gamestop Return Series since New Year") +
  xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b/%a")

#analyzing weird movement

gme_ret19 <- subset(gme_ret, index(gme_ret) > "2021-01-18")

ggplot(gme_ret19, aes(x = index(gme_ret19), y = gme_ret19)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("Drastic Changes in Returns") + xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b/%a")

summary(gme_ret19)
sd(gme_ret19)

