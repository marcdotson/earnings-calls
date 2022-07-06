library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
dash_counts <- read_rds(here::here("Data", "dash_counts.rds")) |> ungroup() |> 
  select(id:revenue, overall_count:clmd_prop, constraining_loughran:superfluous_loughran) |> 
  mutate(loughran_positive_negative_prop = round(positive_loughran/negative_loughran, 4),
         loughran_valenced_overall_ratio = round(sum(across(contains("_loughran")))/overall_count, 4))

d <- unique(dash_counts$year) |> sort()
sec <- unique(dash_counts$sector) |> sort()
gru <- unique(dash_counts$group) |> sort()
ind <- unique(dash_counts$industry) |> sort()
sub <- unique(dash_counts$sub_industry) |> sort()
gics_select <- dash_counts |> select(sector:sub_industry)

var_select <- dash_counts |> select(revenue:loughran_valenced_overall_ratio)

library(shiny)




ui <- fluidPage(
  
  titlePanel("Earnings Call Dashboard"),
  tabsetPanel(
    tabPanel("Word Count",
  sidebarLayout(
    
    sidebarPanel(
      
      varSelectInput(
        inputId = "dictionary1",
        label = "Select Variables--Graph 1",
        data = var_select
      ),
      
      varSelectInput(
      inputId = "varlevel1",
      label = "Select GICS Level--Graph 1",
      data = gics_select),
      
      selectInput(
        inputId = "func1",
        label = "Select Function--Graph 1",
        choices = c("sum", "mean")
      ),
      
      # Select group.
      selectInput(
        inputId = "subset_gics1",
        label = "Select GICS Subset--Graph 1",
        choices = c(sec, gru, ind, sub),
        multiple = TRUE),
      
      
      # Select start year
      selectInput(
        inputId = "from_year1",
        label = "From--Graph 1",
        choices = d),
      
      # Select end year
      selectInput(
        inputId = "to_year1",
        label = "To--Graph 1",
        choices = d,
        selected = "2021"),
      
      
      varSelectInput(
        inputId = "dictionary2",
        label = "Select Variables--Graph 2",
        data = var_select
      ),
      
      
      varSelectInput(
        inputId = "varlevel2",
        label = "Select GICS Level--Graph 2",
        data = dash_select),
      
      
      selectInput(
        inputId = "func2",
        label = "Select Function--Graph 2",
        choices = c("sum", "mean")
      ),
      
      # Select group.
      selectInput(
        inputId = "subset_gics2",
        label = "Select GICS Subset--Graph 2",
        choices = c(sec, gru, ind, sub),
        multiple = TRUE),
      
      
      # Select start year
      selectInput(
        inputId = "from_year2",
        label = "From--Graph2",
        choices = d),
      
      # Select end year
      selectInput(
        inputId = "to_year2",
        label = "To--Graph2",
        choices = d,
        selected = "2021")
    ),
  
  mainPanel(
    uiOutput(c("subset_gics1", "subset_gics2", "dictionary1", "dictionary2", "func1", "func2")),
    plotlyOutput("plot1"),
    plotlyOutput("plot2")
    # dataTableOutput("table1"),
    # dataTableOutput("table2")
    # 
  )
)
)))

server <- function(input, output, session) {

  
  
varlevel1 <- reactive({
  req(input$varlevel1)
  dash_counts |> mutate(
    org = .data[[input$varlevel1]]
  )
})
observeEvent(varlevel1(), {
  choices1 <- unique(varlevel1()$org) |> sort()
  updateSelectInput(inputId = "subset_gics1", choices = choices1)
})

varlevel2 <- reactive({
  req(input$varlevel2)
  dash_counts |> mutate(
    org = .data[[input$varlevel2]]
  )
})
observeEvent(varlevel2(), {
  choices1 <- unique(varlevel2()$org) |> sort()
  updateSelectInput(inputId = "subset_gics2", choices = choices1)
})

dictionary1 <- reactive({
  req(input$dictionary1)
  varlevel1() |> mutate(
    variable1 = .data[[input$dictionary1]]
  )
})

dictionary2 <- reactive({
  req(input$dictionary2)
  varlevel2() |> mutate(
    variable2 = .data[[input$dictionary2]]
  )
})

  
  output$plot1 <- renderPlotly({
    req(input$subset_gics1, input$func1, input$dictionary1)
   if(input$func1 == "sum") {ggplotly(dictionary1() |> filter(org==input$subset_gics1) |> 
      filter(
        year >= input$from_year1,
        year <= input$to_year1) |> 
        group_by(year_quarter) |> 
        summarise(variable1 = sum(variable1)) |> 
      ggplot(aes(x = year_quarter)) +
      geom_col(aes(y = variable1), fill = "blue", alpha = 0.5))}
    else {ggplotly(dictionary1() |> filter(org==input$subset_gics1) |> 
                      filter(
                        year >= input$from_year1,
                        year <= input$to_year1) |> 
                      group_by(year_quarter) |> 
                      summarise(variable1 = mean(variable1)) |> 
                      ggplot(aes(x = year_quarter)) +
                      geom_col(aes(y = variable1), fill = "blue", alpha = 0.5 ))}
      
    })
  
  
  output$plot2 <- renderPlotly({
    req(input$subset_gics2, input$func2, input$dictionary2)
    if(input$func2 == "sum") {ggplotly(dictionary2() |> filter(org==input$subset_gics2) |> 
               filter(
                 year >= input$from_year2,
                 year <= input$to_year2) |> 
               group_by(year_quarter) |> 
               summarise(variable2 = sum(variable2)) |> 
               ggplot(aes(x = year_quarter)) +
               geom_col(aes(y = variable2), fill = "blue", alpha = 0.5)) }
    else {ggplotly(dictionary2() |> filter(org==input$subset_gics2) |> 
                     filter(
                       year >= input$from_year2,
                       year <= input$to_year2) |> 
                     group_by(year_quarter) |> 
                     summarise(variable2 = mean(variable2)) |> 
                     ggplot(aes(x = year_quarter)) +
                     geom_col(aes(y = variable2), fill = "blue", alpha = 0.5)) }
  })
  
  
  
 # #  
 # output$table1 <- renderDataTable(varlevel() |> filter(org==input$subset_gics) |>
 #  filter(
 #    year >= input$from_year,
 #    year <= input$to_year) |>  )
 #  #

}

shinyApp(ui, server)


