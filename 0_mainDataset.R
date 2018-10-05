library(dplyr)

## dataset generating #########################################################

yr = '2014'

dat.contri <- candidates_terms[election_year == yr & 
                                 # county == county_tw & 
                                 type == 'councilors' , 
                               .(election_year, county, district, candidate_id, name, party, constituency,
                                 elected, elected_councilor_id, 
                                 votes, votes_percentage, votes_detail, 
                                 politicalcontributions)
                               ][, votes_percentage := gsub('%','',votes_percentage) %>% as.numeric()]

# 取得獻金資料
contri <- function(x) {
  # x <- tmp[elected_councilor_id == '3186',]
  
  dat <- x$politicalcontributions %>% 
    jsonlite::fromJSON(flatten = T) %>% data.frame() %>% 
    filter(election_year == '2014')
  
  if (nrow(dat) != 0){
    dat$candidate_id <- x$candidate_id
    return(dat %>% select(-title, -election_name, -election_year))
  }
}

tmp <- dat.contri[!is.na(politicalcontributions), .(candidate_id, politicalcontributions)]

tmp <- do.call(rbind, lapply(1:nrow(tmp) , 
                             function(x) contri(tmp[x,]))) %>%
  filter(.data = ., pc.in_total != 0)

dat.contri <- merge(dat.contri, tmp, all.x = TRUE, by = 'candidate_id')


## 2018 年參選
dat.thisyr <- candidates_terms[election_year == '2018' & 
                                 # county == county_tw & 
                                 type == 'councilors' , 
                               .(election_year, candidate_id, name)
                               ]

dat.contri$thisyr <- dat.contri$candidate_id %in% dat.thisyr$candidate_id

glimpse(dat.contri)

write.csv(dat.contri, 'data/pc.csv', row.names = FALSE)

## 檢查
# https://councils.g0v.tw/councilors/pc/108f82df04fd49d3aa59f3b42904f380/
# dat.contri[name == '歐陽龍'] %>% str()

# 收：1380萬
# 支：535萬
# 餘：849萬

# $ pc.in_total                : int 13801876
# $ pc.out_total               : int 5351845
# $ pc.balance                 : int 8498421

# 人民團體捐贈：2.0萬
# 政黨捐贈：5.0萬
# 其他：5076
# 營利事業捐贈：514.8萬
# 匿名捐贈：2000
# 個人捐贈：857.6萬

# $ pc.in.in_civil             : int 20000
# $ pc.in.in_party             : int 50000
# $ pc.in.in_others            : int 5076
# $ pc.in.in_profit            : int 5148800
# $ pc.in.in_anonymous         : int 2000
# $ pc.in.in_individual        : int 8576000

# 人事費用：96.9萬
# 返還捐贈：5.0萬
# 租用宣傳車輛：51.0萬
# 宣傳：185.9萬
# 集會：39.8萬
# 雜支：127.2萬
# 租用競選辦事處：16.5萬
# 交通旅運：12.6萬
# 繳庫
# 公共關係費

# $ pc.out.out_personnel       : int 969060
# $ pc.out.out_return          : int 50000
# $ pc.out.out_campaign_vehicle: int 510650
# $ pc.out.out_propagate       : int 1859366
# $ pc.out.out_rally           : int 398638
# $ pc.out.out_miscellaneous   : int 1272758
# $ pc.out.out_campaign_office : int 165348
# $ pc.out.out_travel          : int 126025
# $ pc.out.out_exchequer       : int 0
# $ pc.out.out_public_relation : int 0

## 2014 年議員選舉各區基本資料 #########################################

library(rvest)

page <- read_html('https://zh.wikipedia.org/wiki/2014%E5%B9%B4%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E7%9B%B4%E8%BD%84%E5%B8%82%E8%AD%B0%E5%93%A1%E5%8F%8A%E7%B8%A3%E5%B8%82%E8%AD%B0%E5%93%A1%E9%81%B8%E8%88%89%E9%81%B8%E8%88%89%E5%8D%80%E6%8A%95%E7%A5%A8%E7%B5%90%E6%9E%9C%E5%88%97%E8%A1%A8')

zone <- page %>% html_nodes(".mw-headline") %>% html_text() 
zone <- do.call(rbind, zone[grepl("（", zone)] %>% gsub("）", "", .) %>% strsplit(., "（")) %>% data.frame()
colnames(zone) <- c("mw", "district")
zone <- zone %>% mutate(
  county = substr(mw, 1, 3),
  mw = substr(mw, 4, 15)
) 

zone.info <- page %>% html_nodes("ul>li") %>% html_text() 
zone$seats <- zone.info[grepl("應選出名額：", zone.info)] %>% stringr::str_extract('\\d+.\\d+|\\d+') %>% gsub(",", '', .)
zone$pool <- zone.info[grepl("選舉人數：", zone.info)] %>% stringr::str_extract_all('\\d+.\\d+|\\d+') %>% gsub(",", '', .)
zone$voters <- zone.info[grepl("投票數（投票率）", zone.info)] %>% stringr::str_extract('(\\d+.\\d+)|(\\d+)') %>% gsub("（\\d+|,", '', .)

# 有效票（比率）：305,709（98.12%），無效票（比率）：5,846（1.88%）
zone.tmp <- do.call(rbind, zone.info[grepl("無效票（比率）", zone.info)] %>%
                      gsub("（|）|%", '; ', .) %>% gsub(",", '', .) %>% 
                      stringr::str_extract_all('([0-9]+\\,[0-9]+)|([0-9]+\\.[0-9]+)|([0-9]+)')) %>% data.frame()
colnames(zone.tmp) <- c('valid', 'valid.p', 'invalid', 'invalid.p')
zone <- bind_cols(zone, zone.tmp)

write.csv(zone, file = 'data/zoneInfo.csv', row.names = FALSE)

