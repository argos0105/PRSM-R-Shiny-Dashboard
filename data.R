library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(lubridate)
library(magrittr)
library(DT)

#df_raw= read.csv('/home/rstudio/barc4_1.csv',stringsAsFactors = FALSE)
##df_raw = read.csv('barc4_1.tsv',sep = '\t')
#df_raw = read.table('barc4_1.tsv',sep = '\t')
#df_raw = read.delim('barc4_1.tsv',quote="",sep ='\t',stringsAsFactors = FALSE)
df_raw = read.delim('/home/ubuntu/barc_5.csv',sep = ',',quote = "",stringsAsFactors = FALSE)
df=df_raw
df = df_raw[df_raw$Direct.Clients.=="Yes",]

#df_c = data.frame(df[,1:7],df$CS.15.40.AB.M+df$CS.15.40.AB.F,df[,8:9],df$CS.22.40.AB.F+df$CS.22.40.AB.M,df[,10:12])
#df = df_c
#rm(df_c)
#names(df) = c("Advertiser","Brand","Month.Year","Channel","Length","CS.15.40.AB.M","CS.15.40.AB.F","CS.15.40.AB.MF","CS.22.40.AB.F","CS.22.40.AB.M","CS.22.40.AB.MF","ER","Genre","Spends")
df_rates_lookup = data.frame(c(unique(df_cpm$tg)),c(300,300,400,400,500,500))
names(df_rates_lookup) = c("tg","rate")
df$date = as.Date(paste(as.character(df$Month.Year)," 01",sep = ""),format = "%B %Y %d")
#df$date = as.Date(df$Month.Year,format = "%m/%y/%d")
df$Month = format(df$date,"%b-%Y")
#df$Month_No = month(as.Date(paste(as.character(df$Month.Year)," 01",sep = ""),format = "%B  %Y  %d"))
df$Month_No = month(df$date)
brand_names = append(as.character(unique(df$Brand)),"All")
advertiser_names = unique(df$Advertiser)
df$Spends = as.numeric(df$Spends)

df_cpm = data.frame(df[,1:7],df[,12:21])
df_cpm$tg = "CS.15.40.AB.M"
df_cpm$cpm = df[,"CS.15.40.AB.M"]

df_cpm_1 = data.frame(df[,1:7],df[,12:21])
df_cpm_1$tg = "CS.15.40.AB.F"
df_cpm_1$cpm = df[,"CS.15.40.AB.F"]


df_cpm_2 = data.frame(df[,1:7],df[,12:21])
df_cpm_2$tg = "CS.22.40.AB.M"
df_cpm_2$cpm = df[,"CS.22.40.AB.M"]

df_cpm_3 = data.frame(df[,1:7],df[,12:21])
df_cpm_3$tg = "CS.22.40.AB.F"
df_cpm_3$cpm = df[,"CS.22.40.AB.F"]

df_cpm_4 = data.frame(df[,1:7],df[,12:21])
df_cpm_4$tg = "CS.15.40.AB.MF"
df_cpm_4$cpm = df[,"CS.15.40.AB.M"] + df[,"CS.15.40.AB.F"]

df_cpm_5 = data.frame(df[,1:7],df[,12:21])
df_cpm_5$tg = "CS.22.40.AB.MF"
df_cpm_5$cpm = df[,"CS.22.40.AB.M"] + df[,"CS.22.40.AB.F"]

df_cpm = rbind(df_cpm,df_cpm_1,df_cpm_2,df_cpm_3,df_cpm_4,df_cpm_5)
df_cpm$cpm[!is.finite(df_cpm$cpm)] = 0

#####
df_tv = read.csv("/home/ubuntu/dash_db_tv_2.csv")
df_hs = read.csv("/home/ubuntu/dash_db_hs_2.csv")
df_hs$incremental = 0.9
a = names(df_hs)
a[5] = "Brand"
names(df_hs) = a
df_hs_merged = merge(df_hs,df_tv, by =c("Brand","Month"),all.y = TRUE)
#df_hs_merged = merge(df_hs,df_tv, by = "Brand",all.y = TRUE)

df_hs_merged$date = as.Date(paste(df_hs_merged$Month,"-01",sep = ""),format = "%y-%b-%d")
df_hs_merged$Month = format(df_hs_merged$date,"%b-%Y")
df_hs_merged$Month_No = month(df_hs_merged$date)

#df_tvhs_1 = df_hs_merged[,c(1,2,3,4,8,12,13,14,15,16,17,19,21,24,38)]
df_tvhs_1 = data.frame(df_hs_merged[,1:26],df_hs_merged[,38:39])
df_tvhs_1$tg = "CS.15.40.AB.M"
df_tvhs_1$imp = df_hs_merged[,"CS.15.40.AB.M"]
df_tvhs_1$cov = df_hs_merged[,"Reach.CS.15.40.AB.M"]

df_tvhs_2 = data.frame(df_hs_merged[,1:26],df_hs_merged[,38:39])
df_tvhs_2$tg = "CS.15.40.AB.F"
df_tvhs_2$imp = df_hs_merged[,"CS.15.40.AB.F"]
df_tvhs_2$cov = df_hs_merged[,"Reach.CS.15.40.AB.F"]

df_tvhs_3 = data.frame(df_hs_merged[,1:26],df_hs_merged[,38:39])
df_tvhs_3$tg = "CS.15.40.AB.MF"
df_tvhs_3$imp = df_hs_merged[,"CS.15.40.AB.MF"]
df_tvhs_3$cov = df_hs_merged[,"Reach.CS.15.40.AB.MF"]

df_tvhs_4 = data.frame(df_hs_merged[,1:26],df_hs_merged[,38:39])
df_tvhs_4$tg = "CS.22.40.AB.M"
df_tvhs_4$imp = df_hs_merged[,"CS.22.40.AB.M"]
df_tvhs_4$cov = df_hs_merged[,"Reach.CS.22.40.AB.M"]

df_tvhs_5 = data.frame(df_hs_merged[,1:26],df_hs_merged[,38:39])
df_tvhs_5$tg = "CS.22.40.AB.F"
df_tvhs_5$imp = df_hs_merged[,"CS.22.40.AB.F"]
df_tvhs_5$cov = df_hs_merged[,"Reach.CS.22.40.AB.F"]

df_tvhs = data.frame(df_hs_merged[,1:26],df_hs_merged[,38:39])
df_tvhs$tg = "CS.22.40.AB.MF"
df_tvhs$imp = df_hs_merged[,"CS.22.40.AB.MF"]
df_tvhs$cov = df_hs_merged[,"Reach.CS.22.40.AB.MF"]

df_tvhs = rbind(df_tvhs,df_tvhs_1,df_tvhs_2,df_tvhs_3,df_tvhs_4,df_tvhs_5)
#df_tvhs$imp[!is.finite(df_tvhs$imp)] = 0
#df_tvhs$cov[!is.finite(df_tvhs$cov)] = 0

df_tvhs$Spends = as.numeric(df_tvhs$Spends)

df_tvhs$imp = as.numeric(as.character(df_tvhs$imp))
df_tvhs$cov = as.numeric(as.character(df_tvhs$cov))
df_tvhs$impression = as.numeric(as.character(df_tvhs$impression))
df_tvhs$unique_impressions = as.numeric(as.character(df_tvhs$unique_impressions))
df_pass = data.frame(0,0)

df_hs$date = as.Date(paste(df_hs$Month,"-01",sep = ""),format = "%y-%b-%d")
df_hs$Month = format(df_hs$date,"%b-%Y")
df_tvhs$tv_freq = round(df_tvhs$imp/df_tvhs$cov,1)
df_tvhs$tv_freq[!is.finite(df_tvhs$tv_freq)] = 0

df_ad_prod = read.csv("/home/ubuntu/ad_products/ad_product.csv",stringsAsFactors = FALSE)

df_campaign = data.frame(matrix(0,7,5))
names(df_campaign) = c("objective","awareness","sustenance","engagement","content_solution")
df_campaign$objective = unique(df_ad_prod$Campaign.objective)

df_reach_limit = data.frame(tg = c(unique(df_tvhs$tg)),value = c(15,21,9,30,10.5,4.5),zapr = c(0.8,0.75,0.9,0.8,0.75,0.9))


