###TABLE 6


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
paper_data_subset <- subset(paper_data, 
                            name != "tokyo" & 
                              name != "osaka" & 
                              name != "kanagawa" & 
                              name != "aichi" & 
                              name != "kyoto" & 
                              name != "hyogo" & 
                              name != "hokkaido")



reg28 <- plm(GDP ~ share_gland_city + gland + cityshare1 + year,
             data = subset(paper_data_subset, dist_closestcity >= 50/1000),
             index = c("name", "year", "prefid"),
             model = "within")
cov28         <- vcovHC(reg28, type = "sss")
robust_se28   <- sqrt(diag(cov28))



firststage_reg29 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + year,
                        data = subset(paper_data_subset, dist_closestcity >= 50/1000),
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg29        <- vcovHC(firststage_reg29, type = "sss")
robust_se_firststage_reg29  <- sqrt(diag(cov_firststage_reg29))



reg29 <- plm(GDP ~ LOAN + gland + cityshare1 + year|share_gland_city +  gland + cityshare1 + year,
             data = subset(paper_data_subset, dist_closestcity >= 50/1000),
             index = c("name", "year", "prefid"),
             model = "within")
cov29         <- vcovHC(reg29, type = "sss")
robust_se29   <- sqrt(diag(cov29))


stargazer(reg28, firststage_reg29,
          se = list(robust_se28, robust_se_firststage_reg29),
          title = "Table 6 Dist > 50km",
          type = "text", 
          column.labels = c("∆GDP", "∆LOAN(IV-1)"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg29,
          se = list(robust_se29),
          title = "Table 6 Dist > 50km",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)


#########



reg30 <- plm(GDP ~ share_gland_city + gland + cityshare1 + year,
             data = subset(paper_data_subset, dist_closestcity >= 100/1000),
             index = c("name", "year", "prefid"),
             model = "within")
cov30        <- vcovHC(reg30, type = "sss")
robust_se30   <- sqrt(diag(cov30))




firststage_reg31 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + year,
                        data = subset(paper_data_subset, dist_closestcity >= 100/1000),
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg31        <- vcovHC(firststage_reg31, type = "sss")
robust_se_firststage_reg31  <- sqrt(diag(cov_firststage_reg31))


reg31 <- plm(GDP ~ LOAN + gland + cityshare1 + year|share_gland_city +  gland + cityshare1 + year,
             data = subset(paper_data_subset, dist_closestcity >= 100/1000),
             index = c("name", "year", "prefid"),
             model = "within")
cov31        <- vcovHC(reg31, type = "sss")
robust_se31   <- sqrt(diag(cov31))


stargazer(reg30, firststage_reg31,
          se = list(robust_se30, robust_se_firststage_reg31),
          title = "Table 6 Dist > 100km",
          type = "text", 
          column.labels = c("∆GDP", "∆LOAN(IV-1)"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg31,
          se = list(robust_se31),
          title = "Table 6 Dist > 100km",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)



#######

reg32 <- plm(GDP ~ share_gland_city + gland + cityshare1 + year,
             data = subset(paper_data_subset, dist_closestcity >= 150/1000),
             index = c("name", "year", "prefid"),
             model = "within")
cov32        <- vcovHC(reg32, type = "sss")
robust_se32   <- sqrt(diag(cov32))



firststage_reg33 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + year,
                        data = subset(paper_data_subset, dist_closestcity >= 150/1000),
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg33        <- vcovHC(firststage_reg33, type = "sss")
robust_se_firststage_reg33 <- sqrt(diag(cov_firststage_reg33))



reg33 <- plm(GDP ~ LOAN + gland + cityshare1 + year|share_gland_city +  gland + cityshare1 + year,
             data = subset(paper_data_subset, dist_closestcity >= 150/1000),
             index = c("name", "year", "prefid"),
             model = "within")
cov33        <- vcovHC(reg33, type = "sss")
robust_se33   <- sqrt(diag(cov33))

stargazer(reg32, firststage_reg33,
          se = list(robust_se32, robust_se_firststage_reg33),
          title = "Table 6 Dist > 150km",
          type = "text", 
          column.labels = c("∆GDP", "∆LOAN(IV-2)"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg33,
          se = list(robust_se33),
          title = "Table 6 Dist > 150km",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

#######


reg34 <- plm(GDP ~ share_gland_city + gland + cityshare1 + year,
             data = subset(paper_data_subset, mean_diff_gpc_city>=0.6),
             index = c("name", "year", "prefid"),
             model = "within")
cov34        <- vcovHC(reg34, type = "sss")
robust_se34   <- sqrt(diag(cov34))



firststage_reg35 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + year,
                        data = subset(paper_data_subset, mean_diff_gpc_city>=0.6),
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg35        <- vcovHC(firststage_reg35, type = "sss")
robust_se_firststage_reg35 <- sqrt(diag(cov_firststage_reg35))



reg35 <- plm(GDP ~ LOAN + gland + cityshare1 + year|share_gland_city +  gland + cityshare1 + year,
             data = subset(paper_data_subset, mean_diff_gpc_city>=0.6),
             index = c("name", "year", "prefid"),
             model = "within")
cov35        <- vcovHC(reg35, type = "sss")
robust_se35   <- sqrt(diag(cov35))

stargazer(reg34, firststage_reg35,
          se = list(robust_se34, robust_se_firststage_reg35),
          title = "Table 6 Income diff. > 0.6m Yen",
          type = "text", 
          column.labels = c("∆GDP", "∆LOAN(IV-1)"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg35,
          se = list(robust_se35),
          title = "Table 6 Income diff. > 0.6m Yen",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)


#############




reg36 <- plm(GDP ~ share_gland_city + gland + cityshare1 + year,
             data = subset(paper_data_subset, mean_diff_gpc_city>=0.8),
             index = c("name", "year", "prefid"),
             model = "within")
cov36        <- vcovHC(reg36, type = "sss")
robust_se36   <- sqrt(diag(cov36))



firststage_reg37 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + year,
                        data = subset(paper_data_subset, mean_diff_gpc_city>=0.8),
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg37        <- vcovHC(firststage_reg37, type = "sss")
robust_se_firststage_reg37 <- sqrt(diag(cov_firststage_reg37))



reg37 <- plm(GDP ~ LOAN + gland + cityshare1 + year|share_gland_city +  gland + cityshare1 + year,
             data = subset(paper_data_subset, mean_diff_gpc_city>=0.8),
             index = c("name", "year", "prefid"),
             model = "within")
cov37        <- vcovHC(reg37, type = "sss")
robust_se37   <- sqrt(diag(cov37))


stargazer(reg36, firststage_reg37,
          se = list(robust_se36, robust_se_firststage_reg37),
          title = "Table 6 Income diff. > 0.8m Yen",
          type = "text", 
          column.labels = c("∆GDP", "∆LOAN(IV-1)"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg37,
          se = list(robust_se37),
          title = "Table 6 Income diff. > 0.8m Yen",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)



#######



reg38 <- plm(GDP ~ share_gland_city + gland + cityshare1 + year,
             data = subset(paper_data_subset, mean_diff_gpc_city>=1),
             index = c("name", "year", "prefid"),
             model = "within")
cov38        <- vcovHC(reg38, type = "sss")
robust_se38  <- sqrt(diag(cov38))



firststage_reg39 <- plm(LOAN ~ share_gland_city + gland + cityshare1 + year,
                        data = subset(paper_data_subset, mean_diff_gpc_city>=1),
                        index = c("name", "year", "prefid"),
                        model = "within")
cov_firststage_reg39        <- vcovHC(firststage_reg39, type = "sss")
robust_se_firststage_reg39  <- sqrt(diag(cov_firststage_reg39))


reg39 <- plm(GDP ~ LOAN + gland + cityshare1 + year|share_gland_city +  gland + cityshare1 + year,
             data = subset(paper_data_subset, mean_diff_gpc_city>=1),
             index = c("name", "year", "prefid"),
             model = "within")
cov39        <- vcovHC(reg39, type = "sss")
robust_se39  <- sqrt(diag(cov39))



stargazer(reg38, firststage_reg39,
          se = list(robust_se38, robust_se_firststage_reg39),
          title = "Table 6 Income diff. > 1m Yen",
          type = "text", 
          column.labels = c("∆GDP", "∆LOAN(IV-1)"),
          covariate.labels = c("City Bank Share x ∆City Land Price", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)

stargazer(reg39,
          se = list(robust_se39),
          title = "Table 6 Income diff. > 1m Yen",
          type = "text", 
          column.labels = c("∆GDP(IV-2)"),
          covariate.labels = c("∆Loan", "∆Local Land Price", "City Bank Share"),
          dep.var.labels.include = FALSE,
          multicolumn = TRUE,
          omit = "year",
          style = "default",
          omit.stat = "f", digits = 4)