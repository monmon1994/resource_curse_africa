library(wbstats)
library(tidyverse)
library(democracyData)
library(ggpubr)
library()

## How does the resource curse work? ##
## Need variables for resource dependence and governance quality and development

wgi_all <- download_wgi_voice_and_accountability()

indicators <- c(gdp_pc = "NY.GDP.PCAP.CD",
                gdp = "NY.GDP.MKTP.CD",
                resource_rents = "NY.GDP.TOTL.RT.ZS",
                fuel_exports =  "TX.VAL.FUEL.ZS.UN",
                imports = 'NE.IMP.GNFS.CD',
                exports = "NE.EXP.GNFS.CD","CC.EST", "GE.EST","RQ.EST", 
                "PV.EST", "RL.EST", "VA.EST")

wb <- wb_data(country = africa_iso, indicator = indicators, start_date = 2018, 
              end_date = 2018, return_wide = TRUE)

wgi_africa <- wgi_all %>% 
  filter(wb_code %in% africa_iso, year == 2018)

#allcountries <- read_csv("data/countryincomelist.csv")

#developing <- subset(allcountries[, c("country_name", "iso3c", "region", 
#                                      "income_class")], income_class != "High income")

africa_iso <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                "TGO", "TUN", "UGA", "ZMB", "ZWE")

# CPIA transparency, accountability, and corruption in the public sector rating (1=low to 6=high)(IQ.CPA.TRAN.XQ)
# Imports of goods and services (current US$)(NE.IMP.GNFS.CD)
# rents per capita Total natural resources rents are the sum of oil rents, natural gas rents, coal rents (hard and soft), mineral rents, and forest rents. NY.GDP.TOTL.RT.ZS
# natural resource exports =  TX.VAL.FUEL.ZS.UN Fuel exports (% of merchandise exports)

### Finding top resource rents

wb %>% 
  slice_max(resource_rents, n = 10)

wb %>% 
  slice_max(fuel_exports, n = 10)

wb %>% 
  slice_min(resource_rents, n = 10)

hist(wb$resource_rents)
hist(wb$fuel_exports)

wb$trade <- (wb$imports + wb$exports) / wb$gdp

wb$trade_resources <- wb$fuel_exports / wb$imports

### Correlation 

cor.test(log(wb$resource_rents), log(wb$gdp_pc), method="pearson")

cor.test(wb$resource_rents, wb$CC.EST, method=c("pearson", "kendall", "spearman"))

cor.test(wb$resource_rents, wb$GE.EST, method=c("pearson", "kendall", "spearman"))

cor.test(wb$fuel_exports, wb$resource_rents, method=c("pearson", "kendall", "spearman"))

cor.test()

ggscatter(wb, x = "resource_rents", y = "gdp_pc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total natural resources rents", ylab = "GDP per capita")

ggscatter(wb, x = "resource_rents", y = "GE.EST", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total natural resources rents", ylab = "Government Effectiveness WGI")

ggscatter(wb, x = "resource_rents", y = "CC.EST", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total natural resources rents", ylab = "Control of Corruption WGI")

#####

countries <- wb %>% 
  slice_max(resource_rents, n = 15) %>% 
  select(country) %>% 
  pull(country)

countries2 <- c("Congo, Rep.","Libya", "Angola","Congo, Dem. Rep.","Chad","Gabon","Sudan","Liberia","Zambia","Algeria","Mauritania",
                "Mozambique","Guinea","Burkina Faso","Ghana", "South Africa")

ggscatter(wb, x = "resource_rents", y = "gdp_pc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xscale = "log10", yscale = "log10",
          xlab = "Total natural resources rents (log)", ylab = "GDP per capita (log)", label = "country",
          label.select = countries2, repel = T)




