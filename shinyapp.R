library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
options(scipen = 999)

candidate <- read.csv('data/pc.csv', stringsAsFactors = FALSE) %>% 
  mutate(county = factor(county, 
                         levels =  c("臺北市","新北市","桃園市","臺中市","臺南市","高雄市",
                                     "基隆市","新竹市","嘉義市","新竹縣","苗栗縣","彰化縣",
                                     "南投縣","雲林縣","嘉義縣","屏東縣","宜蘭縣","花蓮縣",
                                     "臺東縣","澎湖縣","連江縣","金門縣" )),
         elected = factor(elected,
                          levels = c(TRUE, FALSE),
                          labels = c('當選', '落選')))
nms <- levels(candidate$county)

ui <- fluidPage(
  
  headerPanel("2014 議員政治獻金分布 (受贈 vs 支出)"),
  # sidebarPanel(
  #   selectInput('county', '請選擇縣市', choices = nms, selected = "臺北市"),
  #   tags$head(tags$style(".selectize-control.single { width: 100%; z-index: 1; }"))
  # ),
  # 
  # mainPanel(
  #   plotlyOutput('trendPlot', height = "800px", width = "1000px"),
  #   tags$head(tags$style("#trendPlot { margin-top: -60px; }"))
  # )
  
  fluidRow(
    column(12,
           inputPanel(
             selectInput('county', '請選擇縣市', choices = nms, selected = "臺北市"),
             tags$head(tags$style(".selectize-control.single { width: 400px; z-index: 1; }
                                  .shiny-input-panel { padding: 15px 20px 5px 35px;  width: 1030px;}
                                  "))
           )
    ),
    column(12,
           plotlyOutput('trendPlot', height = "800px", width = "1000px")
           )
  )

)

server <- function(input, output) {
  
  dataset <- reactive({
    candidate[candidate$county == input$county,]
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes(x = pc.out_total/10000, y = pc.in_total/10000, color = elected,
                 text = sprintf("%s, %s, %s <br>%s%s <br>受贈: %s <br>支出: %s <br>滕餘: %s <br>得票數: %s (%s %%)<br>2018 參選: %s", 
                                name, party, elected, county, district, pc.in_total, pc.out_total, pc.balance, votes, votes_percentage, thisyr))) +
      geom_point() + 
      geom_abline(intercept = 0, slope = 1, colour='rgba(0,0,0,0.1)', size = 0.5) +
      xlab("支出總額(萬元)") + ylab("受贈總額(萬元)") +
      theme(plot.title = element_text(size=14, face="bold"),
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
            legend.title = element_blank()
      )
    
    ggplotly(p, tooltip="text") %>%
      layout(font = list(family = "微軟正黑體")) 

  })
  
}

shinyApp(ui, server)
