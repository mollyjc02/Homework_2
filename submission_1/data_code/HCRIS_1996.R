if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubricate, stringr, readxl, data.table, gdata, scales)


# create a blank data frame to fill with locational descripters of data we need 
hcris.vars = NULL
hcris.vars = rbind(hcris.vars,c('beds','S300001','01200','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_charges','G300000','00100','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_discounts','G300000','00200','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_operating_exp','G300000','00400','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('ip_charges','G200000','00100','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('icu_charges','G200000','01500','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('ancillary_charges','G200000','01700','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_discharges','S300001','00100','1500','numeric'))
hcris.vars = rbind(hcris.vars,c('mcare_discharges','S300001','00100','1300','numeric'))
hcris.vars = rbind(hcris.vars,c('mcaid_discharges','S300001','00100','1400','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_mcare_payment','E00A18A','01600','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('secondary_mcare_payment','E00A18A','01700','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('street','S200000','00100','0100','alpha'))
hcris.vars = rbind(hcris.vars,c('city','S200000','00101','0100','alpha'))
hcris.vars = rbind(hcris.vars,c('state','S200000','00101','0200','alpha'))
hcris.vars = rbind(hcris.vars,c('zip','S200000','00101','0300','alpha'))
hcris.vars = rbind(hcris.vars,c('county','S200000','00101','0400','alpha'))
colnames(hcris.vars)=c("variable","WKSHT_CD","LINE_NUM","CLMN_NUM","source")

# pulling relevant data out of raw data uploads 
