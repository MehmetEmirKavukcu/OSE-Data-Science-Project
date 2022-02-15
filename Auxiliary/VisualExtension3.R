### VISUAL EXTENSION 3 

suppressMessages(library(haven))
suppressMessages(library(plm))
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(stargazer))
suppressMessages(library(tidyverse))



paper_data <- read_dta("data/ImaiTakarabeData.dta")


paper_data4 <- paper_data %>% mutate(year = between(year, 1980, 2003)) %>% 
  group_by(name) %>% summarise(avg_cityshare1  = mean(cityshare1))


options(repr.plot.width=20, repr.plot.height=15)
graph3 <- ggplot(paper_data4, aes(x = name,
                        y = avg_cityshare1*100,
                        fill = name)) + 
  geom_col(show.legend = FALSE) +
  ylim(0, 100) +
  scale_fill_manual(values = c("purple", "grey", "grey", "grey", "grey", "grey",
                               "grey", "grey", "grey", "grey", "grey", "grey",
                               "yellow", "grey", "grey", "grey", "grey", "grey",
                               "green", "grey", "grey", "blue", "grey", "grey",
                               "grey", "grey", "grey", "grey", "grey", "grey",
                               "grey", "grey", "orange", "grey", "grey", "grey",
                               "grey", "grey", "grey", "grey", "red", "grey",
                               "grey", "grey", "grey", "grey", "grey")) +
  coord_flip() + 
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1)) + 
  labs(y = "Average Share of City Banks Between 1980 and 2003 (%)", x = "Name of the Prefectures")


print(graph3)
