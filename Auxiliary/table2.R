###TABLE 2 

library(haven)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(AER)
library(ggplot2)
library(tidyverse)
suppressMessages(library(haven))
suppressMessages(library(plm))
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(stargazer))
suppressMessages(library(tidyverse))
suppressMessages(library(AER))
suppressMessages(library(ggplot2))


paper_data <- read_dta("data/ImaiTakarabeData.dta")
paper_data_low_frequency <- read_dta("data/ImaiTakarabeDataLowFrequency.dta")
paper_data_low_frequency_subset <- subset(paper_data_low_frequency,
                                          name != "tokyo" & 
                                            name != "osaka" & 
                                            name != "kanagawa" & 
                                            name != "aichi" & 
                                            name != "kyoto" & 
                                            name != "hyogo" & 
                                            name != "hokkaido" &
                                            year != "1980" &
                                            year != "1981" &
                                            year != "1983" &
                                            year != "1985" &
                                            year != "1987" &
                                            year != "1989" &
                                            year != "1991" &
                                            year != "1993" &
                                            year != "1995" &
                                            year != "1997" &
                                            year != "1999" &
                                            year != "2001" &
                                            year != "2003")


reg7 <- plm(GDP2 ~ gland2 + year,
            data = paper_data_low_frequency_subset, 
            index = c("name", "year"),
            model = "within")
cov7         <- vcovHC(reg7, type = "sss")
robust_se7   <- sqrt(diag(cov7))


reg8 <- plm(GDP2 ~ gland2 + share_gland_city2 + cityshare12 + year,
            data = paper_data_low_frequency_subset, 
            index = c("name", "year", "prefid"),
            model = "within")
cov8         <- vcovHC(reg8, type = "sss")
robust_se8   <- sqrt(diag(cov8))

reg9 <- plm(LOAN2 ~ gland2 + year,
            data = paper_data_low_frequency_subset,
            index = c("name", "year", "prefid"),
            model = "within")
cov9         <- vcovHC(reg9, type = "sss")
robust_se9   <- sqrt(diag(cov9))

firststage_reg10 <- plm(LOAN2 ~ gland2 + share_gland_city2 + cityshare12 + year,
                        data = paper_data_low_frequency_subset,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg10  <- vcovHC(firststage_reg10, type = "sss")
robust_se_firststage_reg10   <- sqrt(diag(cov_firststage_reg10))

reg10 <- plm(GDP2 ~ LOAN2 + gland2 + cityshare12 + year |gland2 +  cityshare12 + year + share_gland_city2,
             data = paper_data_low_frequency_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov10         <- vcovHC(reg10, type = "HC0")
robust_se10   <- sqrt(diag(cov10))



##################

paper_data_low_frequency_subset2 <- subset(paper_data_low_frequency,
                                           name != "tokyo" & 
                                             name != "osaka" & 
                                             name != "kanagawa" & 
                                             name != "aichi" & 
                                             name != "kyoto" & 
                                             name != "hyogo" & 
                                             name != "hokkaido" &
                                             year != "1980" &
                                             year != "1981" &
                                             year != "1982" &
                                             year != "1984" &
                                             year != "1985" &
                                             year != "1987" &
                                             year != "1988" &
                                             year != "1990" &
                                             year != "1991" &
                                             year != "1993" &
                                             year != "1994" &
                                             year != "1996" &
                                             year != "1997" &
                                             year != "1999" & 
                                             year != "2000" &
                                             year != "2002" &
                                             year != "2003")


reg10.2 <- plm(GDP3 ~ gland3 + year,
               data = paper_data_low_frequency_subset2,
               index = c("name", "year", "prefid"),
               model = "within")
cov10.2         <- vcovHC(reg10.2, type = "sss")
robust_se10.2   <- sqrt(diag(cov10.2))

reg11 <- plm(GDP3 ~ gland3 + share_gland_city3 + cityshare13 +year,
             data = paper_data_low_frequency_subset2,
             index = c("name", "year", "prefid"),
             model = "within")
cov11        <- vcovHC(reg11, type = "sss")
robust_se11   <- sqrt(diag(cov11))


firststage_reg12 <- plm(LOAN3 ~ gland3 + year,
                        data = paper_data_low_frequency_subset2,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg12  <- vcovHC(firststage_reg12, type = "sss")
robust_se_firststage_reg12   <- sqrt(diag(cov_firststage_reg12))

firststage_reg12.2 <- plm(LOAN3 ~ gland3 + share_gland_city3 + cityshare13  + year,
                          data = paper_data_low_frequency_subset2,
                          index = c("name", "year", "prefid"),
                          model = "within")
cov_firststage_reg12.2  <- vcovHC(firststage_reg12.2, type = "sss")
robust_se_firststage_reg12.2   <- sqrt(diag(cov_firststage_reg12.2))


reg12 <- plm(GDP3 ~ LOAN3 + gland3 + cityshare13 + year |gland3 + cityshare13 + year + share_gland_city3,
             data = paper_data_low_frequency_subset2,
             index = c("name", "year", "prefid"),
             model = "within")
cov12        <- vcovHC(reg12, type = "HC0")
robust_se12   <- sqrt(diag(cov12))


###################


paper_data_low_frequency_subset3 <- subset(paper_data_low_frequency,
                                           name != "tokyo" & 
                                             name != "osaka" & 
                                             name != "kanagawa" & 
                                             name != "aichi" & 
                                             name != "kyoto" & 
                                             name != "hyogo" & 
                                             name != "hokkaido" &
                                             year != "1980" &
                                             year != "1981" &
                                             year != "1982" &
                                             year != "1983" &
                                             year != "1985" &
                                             year != "1986" &
                                             year != "1987" &
                                             year != "1989" &
                                             year != "1989" &
                                             year != "1990" &
                                             year != "1991" &
                                             year != "1993" &
                                             year != "1994" &
                                             year != "1995" & 
                                             year != "1997" &
                                             year != "1998" &
                                             year != "1999" &
                                             year != "2001" &
                                             year != "2002" &
                                             year != "2003" )



reg13 <- plm(GDP4 ~ gland4 + year,
             data = paper_data_low_frequency_subset3,
             index = c("name", "year", "prefid"),
             model = "within")

cov13       <- vcovHC(reg13, type = "sss")
robust_se13   <- sqrt(diag(cov13))

reg14 <- plm(GDP4 ~ gland4 + share_gland_city4 + cityshare14 + year,
             data = paper_data_low_frequency_subset3,
             index = c("name", "year", "prefid"),
             model = "within")
cov14        <- vcovHC(reg14, type = "HC0")
robust_se14  <- sqrt(diag(cov14))

firststage_reg15 <- plm(LOAN4 ~ gland4 + year,
                        data = paper_data_low_frequency_subset3,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg15  <- vcovHC(firststage_reg15, type = "sss")
robust_se_firststage_reg15   <- sqrt(diag(cov_firststage_reg15 ))

firststage_reg15.2 <- plm(LOAN4 ~ share_gland_city4 + gland4 + cityshare14 + year,
                        data = paper_data_low_frequency_subset3,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg15.2  <- vcovHC(firststage_reg15.2, type = "sss")
robust_se_firststage_reg15.2   <- sqrt(diag(cov_firststage_reg15.2))



reg15 <- plm(GDP4 ~ LOAN4 + gland4 + cityshare14 + year |gland4 + cityshare14 + year + share_gland_city4,
             data = paper_data_low_frequency_subset3,
             index = c("name", "year", "prefid"),
             model = "within")

cov15       <- vcovHC(reg15, type = "HC0")
robust_se15  <- sqrt(diag(cov15))


#########


stargazer(reg7, reg8, reg9, firststage_reg10,
          se = list(robust_se7, robust_se8, robust_se9, robust_se_firststage_reg10),
          title = "Table 2 Part 1.1 Two-year frequency",
          type = "text", 
          column.labels = c("∆GDP", "∆GDP", "∆LOAN", "∆LOAN(IV-1)"),
          covariate.labels = c("∆Local Land Price", "City Bank Share x ∆City Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg10,
          se = list(robust_se10),
          title = "Table 2 Part 1.2 Two-year frequency",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)


stargazer(reg10.2, reg11, firststage_reg12, firststage_reg12.2,
          se = list(robust_se10.2, robust_se11, robust_se_firststage_reg12, robust_se_firststage_reg12.2),
          title = "Table 2 Part 2.1 Three-year frequency",
          type = "text", 
          column.labels = c("∆GDP", "∆GDP", "∆LOAN", "∆LOAN(IV-1)"),
          covariate.labels = c("∆Local Land Price", "City Bank Share x ∆City Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg12,
          se = list(robust_se12),
          title = "Table 2 Part 2.2 Three-year frequency",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)


stargazer(reg13, reg14, firststage_reg15, firststage_reg15.2,
          se = list(robust_se13, robust_se14, robust_se_firststage_reg15, robust_se_firststage_reg15.2),
          title = "Table 2 Part 3.1 Four-year frequency",
          type = "text", 
          column.labels = c("∆GDP", "∆GDP", "∆LOAN", "∆LOAN(IV-1)"),
          covariate.labels = c("∆Local Land Price", "City Bank Share x ∆City Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg15,
          se = list(robust_se15),
          title = "Table 2 Part 3.2 Four-year frequency",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)
