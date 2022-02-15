#### TABLE 4 


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




reg18 <- plm(gloan_noncity2 ~ gland + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov18         <- vcovHC(reg18, type = "sss")
robust_se18   <- sqrt(diag(cov18))

reg19 <- plm(gloan_noncity2 ~ gland + share_gland_city + cityshare1 + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov19         <- vcovHC(reg19, type = "sss")
robust_se19   <- sqrt(diag(cov19))

reg20 <- plm(gloan_city2 ~ gland + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov20        <- vcovHC(reg20, type = "sss")
robust_se20   <- sqrt(diag(cov20))

firststage_reg21 <- plm(gloan_city2 ~ gland + share_gland_city + cityshare1 + year,
                        data = paper_data_subset,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg21        <- vcovHC(firststage_reg21, type = "sss")
robust_se_firststage_reg21  <- sqrt(diag(cov_firststage_reg21 ))

reg21 <- plm(GDP ~ gloan_city2 + gland +cityshare1 + year |gland + cityshare1 + year + share_gland_city,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov21        <- vcovHC(reg21, type = "sss")
robust_se21   <- sqrt(diag(cov21))



stargazer(reg18, reg19, reg20, firststage_reg21,
          se = list(robust_se18, robust_se19, robust_se20, robust_se_firststage_reg21),
          title = "Table 4 Part 1 Placebo Test on Non-City Lending",
          type = "text", 
          column.labels = c("∆LOAN(Non-City)", "∆LOAN(Non-City)", "∆LOAN(city)", "∆LOAN(city)"),
          covariate.labels = c("∆Local Land Price", "City Bank Share x ∆City Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)


stargazer(reg21,
          se = list(robust_se21),
          title = "Table 4 Part 2 Placebo Test on Non-City Lending",
          type = "text", 
          column.labels = c("∆GDP(IV-1)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)
