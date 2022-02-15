#### TABLE 5 

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
paper_data_subset <- subset(paper_data, 
                              name != "tokyo" & 
                              name != "osaka" & 
                              name != "kanagawa" & 
                              name != "aichi" & 
                              name != "kyoto" & 
                              name != "hyogo" & 
                              name != "hokkaido")




reg22 <- plm(GDP ~ share_gland_city + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov22       <- vcovHC(reg22, type = "sss")
robust_se22  <- sqrt(diag(cov22))


reg23 <- plm(GDP ~ LOAN + gland +cityshare1 + diff_gpc_gland_city + citybank_diff_city + year |share_gland_city +  gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov23       <- vcovHC(reg23, type = "sss")
robust_se23  <- sqrt(diag(cov23))

firststage_reg23 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + year,
                        data = paper_data_subset,
                        index = c("name", "year"),
                        model = "within")
cov_firststage_reg23  <- vcovHC(firststage_reg23, type = "sss")
robust_se_firststage_reg23   <- sqrt(diag(cov_firststage_reg23))



reg24 <- plm(GDP ~ share_gland_city + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov24       <- vcovHC(reg24, type = "sss")
robust_se24  <- sqrt(diag(cov24))




reg25 <- plm(GDP ~ LOAN + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + year|share_gland_city +  gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov25       <- vcovHC(reg25, type = "HC0")
robust_se25  <- sqrt(diag(cov25))

firststage_reg25 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + year,
                        data = paper_data_subset,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg25  <- vcovHC(firststage_reg25, type = "sss")
robust_se_firststage_reg25   <- sqrt(diag(cov_firststage_reg25))


reg26 <- plm(GDP ~ share_gland_city + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + City_industry_shock + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov26       <- vcovHC(reg26, type = "HC0")
robust_se26  <- sqrt(diag(cov26))


firststage_reg27 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + City_industry_shock + year,
                        data = paper_data_subset,
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg27  <- vcovHC(firststage_reg27, type = "sss")
robust_se_firststage_reg27   <- sqrt(diag(cov_firststage_reg27))



reg27 <- plm(GDP ~ LOAN + gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + City_industry_shock + year|share_gland_city +  gland + cityshare1 + diff_gpc_gland_city + citybank_diff_city + dist_gland_city + citybank_dist_city + City_industry_shock + year,
             data = paper_data_subset,
             index = c("name", "year", "prefid"),
             model = "within")
cov27       <- vcovHC(reg27, type = "sss")
robust_se27  <- sqrt(diag(cov27))


stargazer(reg22, reg23, firststage_reg23, reg24, reg25,
          se = list(robust_se22, robust_se23, robust_se_firststage_reg23, robust_se24, robust_se25),
          title = "Table 5 Part 1  - Controlling For Differential Correlation Between Local Economies and Cities",
          type = "text", 
          column.labels = c("(Reduced-form)∆GDP", "(IV-2)∆GDP", "(IV-1)∆Loan", "(Reduced-form)∆GDP", "(IV-2)∆GDP", "(IV-1)∆Loan", "(Reduced-form)∆GDP", "(2nd Stage IV)∆GDP", "(1st stage IV)∆Loan"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Loan" , "∆Local Land Price",  "City Bank Share", "Income diff x ∆City Land Price", "Income diff x City Bank Share", "Distance x ∆City Land Price", "Distance x City Bank Share", "Industry mix Control"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)


stargazer(firststage_reg25, reg26, reg27, firststage_reg27,
          se = list(robust_se_firststage_reg25, robust_se26, robust_se27, robust_se_firststage_reg27),
          title = "Table 5 Part 2 - Controlling For Differential Correlation Between Local Economies and Cities",
          type = "text", 
          column.labels = c("(IV-1)∆Loan", "(Reduced-form)∆GDP", "(IV-2)∆GDP", "(IV-1)∆Loan"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Loan" , "∆Local Land Price",  "City Bank Share", "Income diff x ∆City Land Price", "Income diff x City Bank Share", "Distance x ∆City Land Price", "Distance x City Bank Share", "Industry mix Control"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)





