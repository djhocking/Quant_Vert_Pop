
library(dplyr)
library(lubridate)

sal <- read.csv("Lecture/Data/GSMNP_Salamanders.csv", header = TRUE, stringsAsFactors = FALSE)

str(sal)

sal2 <- sal %>%
  mutate(date = mdy(Date)) %>%
  select(-Date) %>%
  group_by(species = Species, Plot_Trans_Survey, plot = Plot, transect = Transect, Observer) %>%
  summarise(count = trunc(median(Number)),
            humidity = mean(Humidity),
            temp = mean(Temp),
            precip = mean(Precip),
            elev = mean(elev),
            slope = mean(Slope),
            twi = mean(twi_10),
            lat = mean(Lat),
            lon = mean(Long)) %>%
  mutate(presence = ifelse(count > 0, 1, 0)) %>%
  arrange(plot, transect)

str(sal2)
head(sal2, 25)
summary(sal2)

# Just use EWil

ewil <- sal2 %>%
  filter(species == "EWIL")

ewil
summary(ewil)

write.csv(ewil, file = "Lecture/Data/ewilderae.csv", row.names = FALSE)
