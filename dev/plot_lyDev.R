candidate %>% filter(county %in% levels(candidate$county)[1:3]) %>%
  plot_ly(
    type = 'scatter', 
    x = ~pc.out_total/10000, 
    y = ~pc.in_total/10000,
    color = ~elected,
    colors = c("#00BFC4", "#F8766D"),
    text = ~sprintf("%s, %s, %s <br>%s%s <br>受贈: %s <br>支出: %s <br>滕餘: %s <br>得票數: %s (%s %%)<br>2018 參選: %s", 
                   name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, votes_percentage, thisyr), 
    hoverinfo = 'text',
    mode = 'markers', 
    transforms = list(
      list(
        type = 'filter',
        target = ~county,
        operation = '=',
        value = levels(candidate$county)[1]
      )
    )) %>% layout(
      font = list(family = "微軟正黑體"),
      xaxis = list(title = "支出總額(萬元)"),
      yaxis = list(title = "受贈總額(萬元)"),
      shapes = list(
        type = 'line',
        xref = 'x', x0 = 0, x1 = 2000,
        yref = 'y', y0 = 0, y1 = 2000,
        line = list(color = 'rgb(0,0,0)'),
        opacity = 0.1),
      updatemenus = list(
        list(
          type = 'dropdown',
          active = 0,
          buttons = list(
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[1]), label = levels(candidate$county)[1]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[2]), label = levels(candidate$county)[2]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[3]), label = levels(candidate$county)[3]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[4]), label = levels(candidate$county)[4]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[5]), label = levels(candidate$county)[5]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[6]), label = levels(candidate$county)[6]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[7]), label = levels(candidate$county)[7]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[8]), label = levels(candidate$county)[8]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[9]), label = levels(candidate$county)[9]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[10]), label = levels(candidate$county)[10]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[11]), label = levels(candidate$county)[11]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[12]), label = levels(candidate$county)[12]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[13]), label = levels(candidate$county)[13]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[14]), label = levels(candidate$county)[14]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[15]), label = levels(candidate$county)[15]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[16]), label = levels(candidate$county)[16]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[17]), label = levels(candidate$county)[17]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[18]), label = levels(candidate$county)[18]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[19]), label = levels(candidate$county)[19]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[20]), label = levels(candidate$county)[20]),            
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[21]), label = levels(candidate$county)[21]),
            list(method = "restyle", args = list("transforms[0].value", levels(candidate$county)[22]), label = levels(candidate$county)[22])
          )
        )
      )
    )
