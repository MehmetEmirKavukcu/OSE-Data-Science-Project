#### Visaul Extension 5



suppressMessages(library(haven))
suppressMessages(library(plm))
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(stargazer))
suppressMessages(library(tidyverse))



paper_data <- read_dta("data/ImaiTakarabeData.dta")
paper_data_subset <- subset(paper_data, 
                              name != "tokyo" & 
                              name != "osaka" & 
                              name != "kanagawa" & 
                              name != "aichi" & 
                              name != "kyoto" & 
                              name != "hyogo" & 
                              name != "hokkaido")


options(repr.plot.width=20, repr.plot.height=10)
graph5 <- ggplot(paper_data_subset) + 
  geom_line(aes(year, LOAN*100,color = "LOAN")) +
  facet_wrap(vars(name)) +
  geom_line(aes(year, GDP*100, color = "GDP")) + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1)) +
  labs(y = "Change in Loan and GDP (%)") +
  scale_color_manual(name = "", values = c("LOAN" = "red", "GDP" = "blue"))


print(graph5)
