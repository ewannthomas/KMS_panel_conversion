

library(tidyverse)
library(rvest)


# Scraping and tidying ----------------------------------------------------



blah<-c("tvm", "klm", "pta", "alp", "ktm", "idk", "ekm", "tsr", "pkd",
        "mlp", "kkd", "wyd", "knr", "ksd")

url<-lapply(blah, function(x)
  paste0("http://www.ecostat.kerala.gov.in/index.php/economy-state-", x)
)

url<-set_names(url, nm=blah)

bruf<-function(x){
  x<-rvest::read_html(url[[x]]) %>% 
    html_table(header = F,
               trim = TRUE)
  
  x<-x[[-2]]
  
  bluh<-x %>% slice(-1) %>% 
    slice(1) %>% unlist(use.names = F)
  
  glah<-x %>% slice(1) %>% unlist(use.names = F)
  
  x<-x %>% slice(-1:-2)
  
  colnames(x)<-bluh  
  
  x<-x %>% mutate(District = word(glah[[1]],8))
  
}

GVA<-map(blah, bruf)

rm(blah, bruf, url)

GVA<-map_df(GVA, bind_rows) %>% 
  mutate(District = case_when(
    District=="District" ~ "Kollam",
    TRUE ~ District),
    mutate(across(c(where(is.character),-Item, -District), ~ as.numeric(.)))) %>% 
  rename(
    `2017-18`= `2017-18(P)`,
    `2018-19`= `2018-19(Q)`
  )










# Saving Image ------------------------------------------------------------
save.image(file = "./R Data/GVA.Rdata")

