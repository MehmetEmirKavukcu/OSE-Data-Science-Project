#TABLE 3 

library(haven)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(tidyverse)
suppressMessages(library(haven))
suppressMessages(library(plm))
suppressMessages(library(lmtest))
suppressMessages(library(sandwich))
suppressMessages(library(stargazer))
suppressMessages(library(tidyverse))


paper_data <- read_dta("data/ImaiTakarabeData.dta")
paper_data_subset <- subset(paper_data, name != "tokyo" & 
                              name != "osaka" & 
                              name != "kanagawa" & 
                              name != "aichi" & 
                              name != "kyoto" & 
                              name != "hyogo" & 
                              name != "hokkaido")


reg16 <- plm(GDP ~ gland + share0_gland_city + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov16        <- vcovHC(reg16, type = "sss")
robust_se16  <- sqrt(diag(cov16))




firststage_reg17 <- plm(LOAN ~ gland + share0_gland_city + year,
                        data = paper_data_subset,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg17        <- vcovHC(firststage_reg17, type = "sss")
robust_se_firststage_reg17  <- sqrt(diag(cov_firststage_reg17 ))


reg17 <- plm(GDP ~ LOAN + gland + year |gland + year + share0_gland_city,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")

cov17       <- vcovHC(reg17, type = "HC0")
robust_se17  <- sqrt(diag(cov17))


stargazer(reg16, firststage_reg17,
          se = list(robust_se16, robust_se_firststage_reg17),
          title = "Table 3 Part 1 Results based on City Bank Share as of 1979",
          type = "text", 
          column.labels = c("∆GDP", "∆LOAN(IV-1)"),
          covariate.labels = c("∆Local Land Price", "City Bank Share x ∆City Land Price (1979)"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg17,
          se = list(robust_se17),
          title = "Table 3 Part 2 Results based on City Bank Share as of 1979",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)
