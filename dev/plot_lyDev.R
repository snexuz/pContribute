library(dplyr)

candidate %>% 
  select(name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, votes_percentage, thisyr) %>%
  filter(!is.na(pc.balance) & county %in% nms[1:4]) %>%
  plot_ly(
    type = 'scatter', 
    x = ~pc.out_total, 
    y = ~pc.in_total,
    color = ~elected,
    colors = c("#00BFC4", "#F8766D"),
    # text = ~sprintf("%s, %s, %s <br>%s%s <br>受贈: %s <br>支出: %s <br>滕餘: %s <br>得票數: %s (%s %%)<br>2018 參選: %s", 
    #                name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, votes_percentage, thisyr), 
    text = ~county,
    hoverinfo = 'text',
    mode = 'markers', 
    transforms = list(
      list(
        type = 'filter',
        target = ~county,
        operation = '=',
        value = nms[1]
      )
    )) %>% layout(
      # font = list(family = "微軟正黑體"),
      # xaxis = list(title = "支出總額(萬元)"),
      # yaxis = list(title = "受贈總額(萬元)"),
      # shapes = list(
      #   type = 'line',
      #   xref = 'x', x0 = 0, x1 = 2000,
      #   yref = 'y', y0 = 0, y1 = 2000,
      #   line = list(color = 'rgb(0,0,0)'),
      #   opacity = 0.1),
      updatemenus = list(
        list(
          type = 'dropdown',
          active = 0,
          buttons = list(
            list(method = "restyle", args = list("transforms[0].value", nms[1]), label = nms[1]),
            list(method = "restyle", args = list("transforms[0].value", nms[2]), label = nms[2]),
            list(method = "restyle", args = list("transforms[0].value", nms[3]), label = nms[3]),
            list(method = "restyle", args = list("transforms[0].value", nms[4]), label = nms[4])
            # list(method = "restyle", args = list("transforms[0].value", nms[5]), label = nms[5]),
            # list(method = "restyle", args = list("transforms[0].value", nms[6]), label = nms[6]),
            # list(method = "restyle", args = list("transforms[0].value", nms[7]), label = nms[7]),
            # list(method = "restyle", args = list("transforms[0].value", nms[8]), label = nms[8]),
            # list(method = "restyle", args = list("transforms[0].value", nms[9]), label = nms[9]),
            # list(method = "restyle", args = list("transforms[0].value", nms[10]), label = nms[10]),
            # list(method = "restyle", args = list("transforms[0].value", nms[11]), label = nms[11]),
            # list(method = "restyle", args = list("transforms[0].value", nms[12]), label = nms[12]),
            # list(method = "restyle", args = list("transforms[0].value", nms[13]), label = nms[13]),
            # list(method = "restyle", args = list("transforms[0].value", nms[14]), label = nms[14]),
            # list(method = "restyle", args = list("transforms[0].value", nms[15]), label = nms[15]),
            # list(method = "restyle", args = list("transforms[0].value", nms[16]), label = nms[16]),
            # list(method = "restyle", args = list("transforms[0].value", nms[17]), label = nms[17]),
            # list(method = "restyle", args = list("transforms[0].value", nms[18]), label = nms[18]),
            # list(method = "restyle", args = list("transforms[0].value", nms[19]), label = nms[19]),
            # list(method = "restyle", args = list("transforms[0].value", nms[20]), label = nms[20]),            
            # list(method = "restyle", args = list("transforms[0].value", nms[21]), label = nms[21]),
            # list(method = "restyle", args = list("transforms[0].value", nms[22]), label = nms[22])
          )
        )
      )
    )
