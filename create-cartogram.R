library(topogRam)
library(plyr)
data(esRegPop)
df.m<-read_feather('gdp_per_region_1980_to_2016.feather')
table <- dcast(df.m, region~year, value.var = 'gdp_current_price')
table[,-1] <-round(t(t(table[,-1])/colSums(table[,-1]))*100,2)

table$region_code <- esRegPop$region_code
table[,-1]

years <- names(table)[-grep("reg",names(table))]
topogRam(
  data = table,
  key_var=years,
  origin= c(0,40),
  geo_id = "region_code",
  shape='spain-regions',
  geo_lab = "region"
)
