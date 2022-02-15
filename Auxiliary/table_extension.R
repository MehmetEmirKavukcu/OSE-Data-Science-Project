##TABLE EXTENSION 

library(haven)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(AER)
library(ggplot2)
library(tidyverse)


paper_data <- read_dta("data/ImaiTakarabeData.dta")
paper_data_subset_extension <- subset(paper_data,
                            name != "tokyo" & 
                              name != "osaka" & 
                              name != "kanagawa" & 
                              name != "aichi" & 
                              name != "kyoto" & 
                              name != "hyogo" & 
                              name != "hokkaido" & 
                              name != "okinawa")



reg1 <- plm(GDP ~ gland + year, 
            data = paper_data_subset_extension,
            index = c("name", "year", "prefid"),
            model = "within")
cov1         <- vcovHC(reg1, type = "sss")
robust_se1    <- sqrt(diag(cov1))


reg2 <- plm(GDP ~ share_gland_city + cityshare1 + year, 
            data = paper_data_subset_extension,
            index = c("name", "year", "prefid"),
            model = "within",
            effect = "individual")
cov2         <- vcovHC(reg2, type = "sss")
robust_se2    <- sqrt(diag(cov2))



reg3 <- plm(GDP ~ gland + share_gland_city + cityshare1 + year, 
            data = paper_data_subset_extension,
            index = c("name", "year", "prefid"),
            model = "within")
cov3         <- vcovHC(reg3, type = "sss")
robust_se3   <- sqrt(diag(cov3))


reg4 <- plm(LOAN ~ gland + year, 
            data = paper_data_subset_extension,
            index = c("name", "year", "prefid"),
            model = "within")
cov4         <- vcovHC(reg4, type = "sss")
robust_se4   <- sqrt(diag(cov4))

firststage_reg5 <- plm(LOAN ~ cityshare1 +share_gland_city + year,
                       data = paper_data_subset_extension,
                       index = c("name", "year", "prefid"),
                       model = "within")
firststage_cov5 <- vcovHC(firststage_reg5, type = "sss")                       
first_stage_robust_se5   <- sqrt(diag(firststage_cov5))


reg5 <- plm(GDP ~ LOAN + cityshare1 + year | cityshare1 + year +share_gland_city,
            data = paper_data_subset_extension, 
            index = c("name", "year", "prefid"),
            model = "within")
cov5         <- vcovHC(reg5, type = "sss")
robust_se5   <- sqrt(diag(cov5))

firststage_reg6 <- plm(LOAN ~ gland + cityshare1 +share_gland_city + year,
                       data = paper_data_subset_extension,
                       index = c("name", "year", "prefid"),
                       model = "within")
first_stage_cov6   <- vcovHC(firststage_reg6  , type = "sss")
first_stage_robust_se6   <- sqrt(diag(first_stage_cov6 ))


reg6 <- plm(GDP ~ LOAN + gland + cityshare1 + year |gland +  cityshare1 + year + share_gland_city,
            data = paper_data_subset_extension, 
            index = c("name", "year", "prefid"),
            model = "within")
cov6         <- vcovHC(reg6, type = "sss")
robust_se6   <- sqrt(diag(cov6))





stargazer(reg1, reg2, reg3, reg4, firststage_reg5, firststage_reg6,
          se = list(robust_se1, robust_se2, robust_se3, robust_se4, first_stage_robust_se5, first_stage_robust_se6),
          title = "Main Regression w/o Okinawa Part 1",
          type = "text", 
          column.labels = c("∆GDP", "∆GDP", "∆GDP", "∆LOAN", "∆LOAN(IV-1)", "∆LOAN(IV-1)"),
          covariate.labels = c("∆Local Land Price", "City Bank Share x ∆City Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg5, reg6,
          se = list(robust_se5, robust_se6),
          title = "Main Regression w/o Okinawa Part 2",
          column.labels = c("∆GDP(IV-2)", "∆GDP(IV-2)"),
          covariate.labels = c("∆LOAN", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          type = "text",
          omit = "year",
          style = "default",
          multicolumn = TRUE,
          omit.stat = "f",
          no.space = TRUE, digits = 4)
