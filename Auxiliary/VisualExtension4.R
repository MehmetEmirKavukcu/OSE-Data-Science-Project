#### Visual Extension 4

suppressMessages(library(haven))
suppressMessages(library(plm))
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(stargazer))
suppressMessages(library(tidyverse))



paper_data <- read_dta("data/ImaiTakarabeData.dta")
paper_data_low_frequency <- read_dta("data/ImaiTakarabeDataLowFrequency.dta")

options(repr.plot.width=20, repr.plot.height=10)


lfdata <- paper_data_low_frequency %>% gather(gland_frequency, gland_value, gland2, gland3 ,gland4 )
lfdata_4years <- subset(lfdata, name != "tokyo" & name != "osaka" &  name != "kanagawa" & name != "aichi" & name != "kyoto" & 
                          name != "hyogo" & name != "hokkaido" &year != "1980" &year != "1981" &year != "1982" &
                          year != "1983" &year != "1985" &year != "1986" & year != "1987" &year != "1989" & year != "1989" &  year != "1990" & year != "1991" & year != "1993" & year != "1994" & year != "1995" & 
                          year != "1997" & year != "1998" & year != "1999" & year != "2001" & year != "2002" & year != "2003" )


graph4 <- ggplot(data = lfdata_4years, aes(x = year, y = gland_value*100, fill = gland_frequency)) + 
  geom_col(position  = position_dodge2()) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1)) + 
  labs(y = "Change in Local Land Price (%)", fill = "Change in Land Prices in") +
  scale_fill_manual(labels = c("2 year Frequency", "3 year Frequency", "4 year Frequency"), values = c("blue", "red", "green"))


print(graph4)
