library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)

## dataset #############################################################

# 候選人基本資料
candidate <- read.csv('data/pc.csv', stringsAsFactors = FALSE) %>% 
  mutate(county = factor(county, 
                         levels =  c("臺北市","新北市","桃園市","臺中市","臺南市","高雄市",
                                     "基隆市","新竹市","嘉義市","新竹縣","苗栗縣","彰化縣",
                                     "南投縣","雲林縣","嘉義縣","屏東縣","宜蘭縣","花蓮縣",
                                     "臺東縣","澎湖縣","連江縣","金門縣" )),
         elected = factor(elected,
                          levels = c(TRUE, FALSE),
                          labels = c('當選', '落選')))

# 2014 各區基本資料
zoneInfo <- read.csv('data/zoneInfo.csv', stringsAsFactors = FALSE)


dat <- merge(
  # 2014 縣市議員選舉基本資料
  zoneInfo %>% 
    group_by(county) %>%
    summarise(seats = sum(seats, na.rm = TRUE),
              pool = sum(pool, na.rm = TRUE),
              voters = sum(voters, na.rm = TRUE),
              valid = sum(valid, na.rm = TRUE),
              invalid = sum(invalid, na.rm = TRUE)),
  merge(
    # 候選人資訊
    candidate %>%
      group_by(county) %>%
      summarise(candidate_nm = n_distinct(candidate_id),
                pc_info = sum(ifelse(!is.na(pc.in_total), 1, 0), na.rm = TRUE),
                pc_inequality = sum(ifelse(!is.na(pc.in_total), pc.balance < 0, NA), na.rm = TRUE),
                pc_votes = sum(ifelse(!is.na(pc.in_total), votes, 0), na.rm = TRUE),
                votes = sum(votes, na.rm = TRUE)
      ),
    # 候選人政治獻金資訊
    candidate %>%
      select(county, colnames(candidate)[grepl('pc', colnames(candidate))]) %>%
      group_by(county) %>%
      summarise_all(function(x) sum(as.numeric(x), na.rm = TRUE)),
    by = 'county', all = TRUE
  ),
  by = 'county', all = TRUE
) %>% mutate(county = factor(county, 
                             levels =  c("臺北市","新北市","桃園市","臺中市","臺南市","高雄市",
                                         "基隆市","新竹市","嘉義市","新竹縣","苗栗縣","彰化縣",
                                         "南投縣","雲林縣","嘉義縣","屏東縣","宜蘭縣","花蓮縣",
                                         "臺東縣","澎湖縣","連江縣","金門縣" )
))

theme_pc <- function(){
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    # panel.border = element_blank(),
    # panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, face="bold"), 
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = 'cm'),
    legend.title = element_blank()
  )
}

options(scipen = 999)

###
glimpse(dat)

# 各縣市選舉花費比較

dat %>%
  select(county, pc.in_total, pc.out_total, pc.balance, pc_info, candidate_nm) %>%
  mutate(
    county = factor(county, 
                    levels = dat$county[order(dat$pc.out_total, decreasing = FALSE)]),
    pc_perc = round(pc_info/candidate_nm*100, digit = 2)
    ) %>%
  arrange(desc(pc.in_total)) 

dat %>%
  select(county, pc.in_total, pc.out_total, pc.balance, pc_info, candidate_nm) %>%
  mutate(
    county = factor(county, 
                    levels = dat$county[order(dat$pc.out_total, decreasing = FALSE)]),
    pc_perc = round(pc_info/candidate_nm*100, digit = 2)
  ) %>%
  arrange(desc(pc.in_total)) %>%
  select(-county) %>%
  summarise_all(c("min", "max"))

##

p <- dat %>%
  select(county, pc.in_total, pc.out_total, pc.balance, pc_info, candidate_nm) %>%
  mutate(
    county = factor(county, 
                    levels = dat$county[order(dat$pc.out_total, decreasing = FALSE)]),
    perc = round(pc_info/candidate_nm*100, digit = 2)) %>%
  ggplot(aes(x = county, 
             y = pc.out_total/10000, 
             fill = county,
             text = sprintf('%s <br>參選人數: %s<br>申報人數: %s (%s %%)<br>總金額: %s', 
                            county, candidate_nm, pc_info, perc, pc.out_total/10000))) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(dat$county))) +
  xlab("縣市") + ylab("支出總額(萬元)") +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 20),
        legend.title = element_blank(),
        legend.position = "none"
  )

p <- ggplotly(p, tooltip = "text") %>%
  layout(font = list(family = "微軟正黑體")) 

# fix plot_ly bar chart issue
for (i in 1:length(p$x$data)){
  p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "") 
}

p




# 每票取得成本
dat %>%
  select(county, pool, voters, valid, pc_votes, pc.out_total) %>%
  mutate(
    acquisition_cost = pc.out_total/pc_votes)


# 政治獻金來源  #####
dat %>%
  select(county, colnames(candidate)[grepl('pc.in', colnames(candidate))]) %>%
  select(-pc.in_total) %>%
  melt() %>%
  group_by(county) %>%
  mutate(
    variable = factor(variable, 
                      levels = c("pc.in.in_individual", "pc.in.in_party", "pc.in.in_civil","pc.in.in_profit", "pc.in.in_anonymous", "pc.in.in_others"),
                      labels = c("個人捐贈","政黨捐贈","人民團體捐贈","營利事業捐贈","匿名捐贈","其他")
    ),
    perc = round(value/sum(value), digits = 4)) %>%
  arrange(county) %>% 
  ggplot(aes(x = "", y = perc, fill = variable)) +
  geom_bar(stat = 'identity', width = 1 ) +
  facet_wrap(~county, ncol = 6) + 
  coord_polar(theta = "y") + theme_pc()


p <- dat %>%
  select(county, colnames(candidate)[grepl('pc.in', colnames(candidate))]) %>%
  select(-pc.in_total) %>%
  melt() %>%
  group_by(county) %>%
  mutate(
    variable = factor(variable, 
                      levels = c("pc.in.in_individual", "pc.in.in_party", "pc.in.in_civil", "pc.in.in_anonymous", "pc.in.in_others","pc.in.in_profit"),
                      labels = c("個人捐贈","政黨捐贈","人民團體捐贈","匿名捐贈","其他","營利事業捐贈")
    ),
    perc = round(value/sum(value), digits = 4)) %>%
  arrange(county) %>% 
  ggplot(aes(x = county, y = perc, fill = variable,
             text = sprintf('%s <br>總金額: %s<br> 佔比: %s%%', variable, value, perc*100))) +
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(dat$county))) +
  theme_pc()
p <- ggplotly(p, tooltip = "text")
p



# 選舉經費 by 政黨
p <- candidate %>% 
  mutate(
    party = factor(party, levels = c("民主進步黨","無黨籍","親民黨","臺灣團結聯盟","中華民主向日葵憲政改革聯盟","臺灣建國黨","人民民主陣線","樹黨","無黨團結聯盟","新黨","綠黨",
                                     "中華統一促進黨","臺灣主義黨","臺灣第一民族黨","人民最大黨","華聲黨","臺灣民族黨","聯合黨","勞動黨","大道人民黨","中國國民黨"))
  ) %>%
  group_by(county, party) %>%
  summarise(candidate_nm = n_distinct(candidate_id),
            candidate_pc = sum(ifelse(is.na(pc.balance), 1, 0)),
            pc_perc = round(candidate_pc/candidate_nm*100, digits = 4),
            elected = sum(ifelse(elected == '當選', 1, 0)),
            pc.in_total = sum(pc.out_total, na.rm = TRUE),
            pc.out_total = sum(pc.out_total, na.rm = TRUE)
  ) %>% ungroup() %>%
  group_by(county) %>%
  mutate(perc = round(pc.in_total/sum(pc.in_total, na.rm = TRUE)*100, digits = 4)) %>% ungroup() %>% 
  ggplot(aes(x = county, y = perc, fill = party, 
             text = sprintf('%s <br>參選人數: %s <br>未申報政治獻金人數: %s <br>未申報率: %s %% <br>總金額: %s', party, candidate_nm, candidate_pc, pc_perc, pc.in_total))
         ) +
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(dat$county))) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(n = 8, name = "Set2"), 
                               RColorBrewer::brewer.pal(n = 5, name = "Set3"), 
                               "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#FDB462", "#FCCDE5", "#B3DE69", "#80B1D3"))
p <- ggplotly(p, tooltip = "text")
p

## 政黨捐獻
p <- candidate %>% 
  mutate(
    party = factor(party, levels = c("民主進步黨","無黨籍","親民黨","臺灣團結聯盟","中華民主向日葵憲政改革聯盟","臺灣建國黨","人民民主陣線","樹黨","無黨團結聯盟","新黨","綠黨",
                                     "中華統一促進黨","臺灣主義黨","臺灣第一民族黨","人民最大黨","華聲黨","臺灣民族黨","聯合黨","勞動黨","大道人民黨","中國國民黨"))
  ) %>%
  group_by(county, party) %>%
  summarise(candidate_nm = n_distinct(candidate_id),
            candidate_pc = sum(ifelse(is.na(pc.balance), 1, 0)),
            pc_perc = round(candidate_pc/candidate_nm*100, digits = 4),
            elected = sum(ifelse(elected == '當選', 1, 0)),
            pc.in_total = sum(pc.out_total, na.rm = TRUE),
            pc.out_total = sum(pc.out_total, na.rm = TRUE),
            pc.in.in_party = sum(pc.in.in_party, na.rm = TRUE)
  ) %>%
  filter(pc.in.in_party != 0) %>%
  ggplot(aes(x = '', y = pc.in.in_party/10000, fill = party,
             text = sprintf('%s <br>參選人數: %s<br>總金額: %s', party, candidate_nm, pc.in.in_party))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~county, ncol = 6, scales="free_x") +
  xlab("政黨") + ylab("捐贈總額(萬元)") +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 20),
        legend.title = element_blank(),
        legend.position = "none"
  )

p <- ggplotly(p, tooltip = "text") %>%
  layout(font = list(family = "微軟正黑體")) 

# fix plot_ly bar chart issue
for (i in 1:length(p$x$data)){
  p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "") 
}

p

## 企業捐贈
p <- candidate %>% 
  mutate(
    party = factor(party, levels = c("民主進步黨","無黨籍","親民黨","臺灣團結聯盟","中華民主向日葵憲政改革聯盟","臺灣建國黨","人民民主陣線","樹黨","無黨團結聯盟","新黨","綠黨",
                                     "中華統一促進黨","臺灣主義黨","臺灣第一民族黨","人民最大黨","華聲黨","臺灣民族黨","聯合黨","勞動黨","大道人民黨","中國國民黨"))
  ) %>%
  group_by(county, party) %>%
  summarise(candidate_nm = n_distinct(candidate_id),
            candidate_pc = sum(ifelse(is.na(pc.balance), 1, 0)),
            pc_perc = round(candidate_pc/candidate_nm*100, digits = 4),
            elected = sum(ifelse(elected == '當選', 1, 0)),
            pc.in_total = sum(pc.out_total, na.rm = TRUE),
            pc.out_total = sum(pc.out_total, na.rm = TRUE),
            pc.in.in_party = sum(pc.in.in_party, na.rm = TRUE),
            pc.in.in_profit = sum(pc.in.in_profit, na.rm = TRUE)
  ) %>%
  filter(pc.in.in_party != 0) %>%
  ggplot(aes(x = '', y = pc.in.in_profit/10000, fill = party,
             text = sprintf('%s <br>參選人數: %s<br>總金額: %s', party, candidate_nm, pc.in.in_profit))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~county, ncol = 6, scales="free_x") +
  xlab("政黨") + ylab("捐贈總額(萬元)") +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 20),
        legend.title = element_blank(),
        legend.position = "none"
  )

p <- ggplotly(p, tooltip = "text") %>%
  layout(font = list(family = "微軟正黑體")) 

# fix plot_ly bar chart issue
for (i in 1:length(p$x$data)){
  p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "") 
}

p


# 政治獻金使用 #####
dat %>%
  select(county, colnames(candidate)[grepl('pc.out', colnames(candidate))]) %>%
  select(-pc.out_total) %>%
  melt() %>%
  group_by(county) %>%
  mutate(
    variable = factor(variable, 
                      levels = c("pc.out.out_personnel","pc.out.out_return","pc.out.out_campaign_vehicle","pc.out.out_propagate","pc.out.out_rally",
                                 "pc.out.out_miscellaneous","pc.out.out_campaign_office","pc.out.out_travel","pc.out.out_exchequer","pc.out.out_public_relation"),
                      labels = c("人事費用","返還捐贈","租用宣傳車輛","宣傳","集會","雜支","租用競選辦事處","交通旅運","繳庫","公共關係費")
    ),
    perc = round(value/sum(value), digits = 4)) %>%
  arrange(county) %>% 
  ggplot(aes(x = "", y = perc, fill = variable)) +
  geom_bar(stat = 'identity', width = 1 ) +
  facet_wrap(~county, ncol = 6) + 
  coord_polar(theta = "y") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    panel.border = element_blank(),
    # panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, face="bold"),
    legend.title = element_blank()
  ) 


p <- dat %>%
  select(county, colnames(candidate)[grepl('pc.out', colnames(candidate))]) %>%
  select(-pc.out_total) %>%
  melt() %>%
  group_by(county) %>%
  mutate(
    variable = factor(variable, 
                      levels = c("pc.out.out_propagate","pc.out.out_personnel","pc.out.out_return","pc.out.out_campaign_vehicle","pc.out.out_rally",
                                 "pc.out.out_campaign_office","pc.out.out_travel","pc.out.out_exchequer","pc.out.out_public_relation","pc.out.out_miscellaneous"
                                 ),
                      labels = c("宣傳","人事費用","返還捐贈","租用宣傳車輛","集會","租用競選辦事處","交通旅運","繳庫","公共關係費","雜支")
    ),
    perc = round(value/sum(value), digits = 4)) %>%
  arrange(county) %>% 
  ggplot(aes(x = county, y = perc, fill = variable,
             text = sprintf('%s <br>總金額: %s<br> 佔比: %s%%', variable, value, perc*100))
             ) +
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(dat$county))) +
  theme_pc()
p <- ggplotly(p, tooltip = "text")
p

# options(scipen = 999)
# 
# do.call(rbind,
#         lapply(dat %>% select(-county), 
#                function(x) c( sum = sum(x),
#                               mean = mean(x),
#                               sd = sd(x),
#                               median = median(x),
#                               minimum = min(x),
#                               maximum = max(x)) )) %>% t()
# 
# dat %>%
#   select(county, seats, candidate_nm, pc_info, pc_inequality) %>%
#   mutate(
#     declare.p = round(pc_info/candidate_nm*100, digits = 2),
#     inequality.p = round(pc_inequality/pc_info*100, digits = 2)
#   ) %>% 
#   select(county, seats, candidate_nm, pc_info, declare.p, pc_inequality, inequality.p) %>%
#   DT::datatable(options = list(pageLength = 50, scrollY =  1000, paging = FALSE, searching = FALSE))

## candidate level
# candidate %>% filter(!is.na(pc.balance)) %>% 
#   group_by(county, party, district, elected) %>%
#   summarise(candidate_nm = n_distinct(candidate_id),
#             pc_info = sum(ifelse(!is.na(pc.in_total), 1, 0), na.rm = TRUE),
#             pc_inequality = sum(ifelse(!is.na(pc.in_total), pc.balance < 0, NA), na.rm = TRUE),
#             pc_votes = sum(ifelse(!is.na(pc.in_total), votes, 0), na.rm = TRUE),
#             votes = sum(votes, na.rm = TRUE),
#             pc.out_total = sum(pc.out_total, na.rm = TRUE)
#   ) %>% 
#   filter(county == '臺北市' & district == '中山區、大同區') %>%
#   mutate(
#     acquisition_cost = pc.out_total/pc_votes) %>% 
#   dcast(data = ., formula = county + party + district ~ elected, fun.aggregate = median, na.rm = TRUE, value.var = 'acquisition_cost') %>%
#   arrange(county, district, party)


## 3d plot
# library(plotly)
# p <- plot_ly(candidate, x = ~pc.in_total, y = ~pc.out_total, z = ~pc.balance, color = ~party) %>%
#   add_markers() %>%
#   layout()
# p

# candidate %>% 
#   filter(!is.na(pc.balance)) %>%
#   ggplot(aes(county, pc.in_total)) +
#   geom_boxplot(outlier.fill = NULL, outlier.colour = '#CCCCCC', outlier.alpha = 0.1) 

## pc.in_total
p <- candidate %>% 
  filter(!is.na(pc.balance)) %>%
  ggplot(aes(county, pc.in_total,
             text = sprintf("%s, %s, %s <br>%s%s <br>收入: %s <br>支出: %s <br>盈餘: %s <br>得票數: %s <br>每票取得成本: %s", 
                            name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, round(pc.out_total/votes, digits = 2)))) +
  geom_boxplot(outlier.shape = NA, width=1) +  # NO OUTLIERS
  geom_jitter(aes(colour= elected), width = 0.2, size = 0.8) +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  theme(plot.title = element_text(size=14, face="bold"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.title = element_blank()
  )
p <- ggplotly(p, tooltip = "text") %>%
  layout(title="2014 年議員候選人政治獻金分佈",
         font=list(family = "微軟正黑體"))
p$x$data[[1]]$marker$opacity <- "0"
p

## pc.out_total
p <- candidate %>% 
  filter(!is.na(pc.balance)) %>%
  ggplot(aes(county, pc.out_total,
             text = sprintf("%s, %s, %s <br>%s%s <br>收入: %s <br>支出: %s <br>盈餘: %s <br>得票數: %s <br>每票取得成本: %s", 
                            name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, round(pc.out_total/votes, digits = 2)))) +
  geom_boxplot(outlier.shape = NA, width=1) +  # NO OUTLIERS
  geom_jitter(aes(colour= elected), width = 0.2, size = 0.8) +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  theme(plot.title = element_text(size=14, face="bold"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.title = element_blank()
  )
p <- ggplotly(p, tooltip = "text")　%>%
  layout(title="2014 年議員候選人政治獻金支出分佈",
         font=list(family = "微軟正黑體"))
p$x$data[[1]]$marker$opacity <- "0"
p

## pc.balance
p <- candidate %>% 
  filter(!is.na(pc.balance)) %>%
  ggplot(aes(county, pc.balance, 
             text = sprintf("%s, %s, %s <br>%s%s <br>收入: %s <br>支出: %s <br>盈餘: %s <br>得票數: %s <br>每票取得成本: %s",
                            name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, round(pc.out_total/votes, digits = 2)))) +
  geom_boxplot(outlier.shape = NA, width=1) +  # NO OUTLIERS
  geom_jitter(aes(colour = elected), width = 0.2, size = 0.8) +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  theme(plot.title = element_text(size=14, face="bold"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.title = element_blank()
        )
p <- ggplotly(p, tooltip = "text") %>%
  layout(title="2014 年議員候選人政治獻金滕餘分佈",
         font=list(family = "微軟正黑體"))
p$x$data[[1]]$marker$opacity <- "0"
p


#####


p <- candidate %>% 
  filter(county == "新北市",
         !is.na(pc.balance)) %>%
  ggplot(aes(x = pc.out_total/10000, y = pc.in_total/10000, color = elected,
             text = sprintf("%s, %s, %s <br>%s%s <br>收入: %s <br>支出: %s <br>盈餘: %s <br>得票數: %s (%s %%)<br>2018 參選: %s", 
                            name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, votes_percentage, thisyr))) +
  geom_point() + geom_abline(intercept = 0, slope = 1, colour='rgba(0,0,0,0.1)', size = 0.5) +
  xlab("支出總額(萬元)") + ylab("受贈總額(萬元)") +
  theme(plot.title = element_text(size=14, face="bold"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        legend.title = element_blank()
  )

ggplotly(p, tooltip="text") %>%
  layout(font = list(family = "微軟正黑體")) 
