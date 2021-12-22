d2 <- read.csv('RSA.csv')
d2$date<-as.Date(d2$date,"%Y-%m-%d")
exclude_covar <- c("denom", "A20_B99_excl_bac", "A16", "A17", "A18", "A19", "R00_R09", "R10_R19", "R20_R39", "R40_R49", "R50_R69", "R70_R94", "R95_R99", "D50_D89")      

d2 <- d2[,-which(names(d2) %in% exclude_covar)]

d2 <-
  d2[, c(
    'date',
    'age',
    'Pneum',
    "A16_A19",
    "A20_A48",
    "A39",
    "A50_A79",
    "A80_B34",
    "B05_B06",
    "B20_B24",
    "B35_B49" ,
    "B45",
    "B50_B89",
    "B99" ,
    "C00_D49" ,
    "A20_B99_a_D50_D89" ,
    "E00_E89"      ,
    "E10_E14"  ,
    "E40_E46",
    "F01_F99"   ,
    "G05_G99"   ,
    "H00_H99_excl_cj_om",
    "I00_I99"  ,
    "I60_I64"   ,
    "K00_K95"   ,
    "K35"  ,
    "K80"   ,
    "L00_L99"       ,
    "M00_M99"   ,
    "N00_N99"  ,
    "N39"     ,
    "O00_O99"       ,
    "P00_P15" ,
    "P05_P07",
    "Q00_Q99"      ,
    "R00_R99"   ,
    "S00_T88"  ,
    "V01_Y99"
  )]
d2$one = 1
SAfrica <- d2[d2$age %in% c('1-11 months', '1-4 years','65-79 years'),]
