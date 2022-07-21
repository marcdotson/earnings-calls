# Load Packages.

library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
library(rsconnect)
library(corrr)

# Load data, I need to make a separate file with all this at some point.
dash_counts <- read_rds(here::here("Data", "dash_counts_new.rds"))

# Presorting choices so they're alphabetical later.
d <- unique(dash_counts$year) |> sort()
sec <- unique(dash_counts$sector) |> sort()
gru <- unique(dash_counts$group) |> sort()
ind <- unique(dash_counts$industry) |> sort()
sub <- unique(dash_counts$sub_industry) |> sort()

# Choices to select for GICS subset.
gics_select <- dash_counts |> select(sector:sub_industry)

# Variable choices.
var_select <- dash_counts |> select(revenue, earnings, forecast, difference, n_lnm:sentiment_index)

# Reloading the package because they did it in a book.
library(shiny)

# Set UI.
ui <- fluidPage(
  
  # Title.
  titlePanel("Earnings Call Dashboard"),
  
  # Making a Tab, I could make more.
  tabsetPanel(
    
    # Name of Tab.
    tabPanel("Word Count",
 
  # Sidebar format.           
  sidebarLayout(
    
    sidebarPanel(
      
      # For Graph/Table 1.
      
      # Select variable of interest.
      varSelectInput(
        inputId = "dictionary1",
        label = "Select Variables--Graph 1",
        data = var_select
      ),
      
      # Select number of lag periods for variable of interest.
      # numericInput(
      #   inputId = "lag1",
      #   label = "Select Variable Lag in Quarters--Table 1",
      #   value = 0,
      #   min=0,
      #   max=8
      #   ),
      # 
      # Select GICS organization level.
      varSelectInput(
        inputId = "varlevel1",
        label = "Select GICS Level--Graph 1",
        data = gics_select
        ),
      
      # Select either sum or mean of word counts.
      selectInput(
        inputId = "func1",
        label = "Select Function--Graph 1",
        choices = c("sum", "mean")
      ),
      
      # Select subset(s) within GICS organization level.
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
      
      # For Graph/Table2
      
      # Select variable of interest.
      varSelectInput(
        inputId = "dictionary2",
        label = "Select Variables--Graph 2",
        data = var_select
      ),
      
      # # Select number of lag periods for variable of interest.
      # numericInput(
      #   inputId = "lag2",
      #   label = "Select Variable Lag in Quarters--Table 2",
      #   value = 0,
      #   min=0,
      #   max=8
      # ),
      # 
      # Select GICS organization level.
      varSelectInput(
        inputId = "varlevel2",
        label = "Select GICS Level--Graph 2",
        data = gics_select),
      
      # Select either sum or mean of word counts.
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
      
      
      # Select start year.
      selectInput(
        inputId = "from_year2",
        label = "From--Graph2",
        choices = d),
      
      # Select end year.
      selectInput(
        inputId = "to_year2",
        label = "To--Graph2",
        choices = d,
        selected = "2021")
    ),
  
  mainPanel(
    # Not actually sure what this does.
    uiOutput(c("subset_gics1", "subset_gics2", "dictionary1", "dictionary2", "func1", "func2")),
    textOutput("text1"),
    # Specify what plot 1 is in the server function.
    plotlyOutput("plot1"),
    # Table 1.
    plotOutput("table1"),
    # Table 1.2.
    # tableOutput("table1"),
    # Plot 2.
    textOutput("text2"),
    plotlyOutput("plot2"),
    # Table 2.
    plotOutput("table2")
  )
)
)))

# Second part of Shiny App, need to specify a server function.
server <- function(input, output, session) {

  
# I don't understand this well enough to explain it, mostly figured it out
# through trial and error. The varleve1/2 blocks basically make the choices
# for the GICS subset input dependent on what was selected for level.
# E.g., If I select sector, it will show all the sectors, but not groups.
# Chapter 10 of Mastering Shiny, "Dynamic UI".
varlevel1 <- reactive({
  req(input$varlevel1)
  dash_counts |> mutate(
    org = .data[[input$varlevel1]] # In order to mess with stuff in data, you need
    # the .data[[]] wrapper. This is for most tidyverse functions, see
    # Chapter 12 "Tidy evaluation" of Mastering Shiny by Hadley Wickham.
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

# Dictionary1/2 refers to the column names in dash_counts that we're interested
# in plotting.
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



output$text1 <- renderText({ 
  req(input$subset_gics1, input$func1, input$dictionary1)
  str_c(
    "Number of Firms: ",
    dictionary1() |> 
    filter(org==input$subset_gics1) |> 
    filter(year >= input$from_year1,
           year <= input$to_year1) |> 
    distinct(gvkey) |> 
    count() |> 
    pull())})

output$plot1 <- renderPlotly({
  # Required inputs before app tries to produce output.
  req(input$subset_gics1, input$func1, input$dictionary1)
  # There's probably a better way to alternate summarizing between
  # sum and mean, but this was the first solution I found.
  if(input$func1 == "sum") {
    # Start with dictionary1(), which is just a slightly changed dash_counts.
    ggplotly(dictionary1() |> 
               # Filter by subset, year, group by year_quarter, summarise by variable
               # of interest, and plot!
               filter(org==input$subset_gics1) |> 
               filter(year >= input$from_year1,
                      year <= input$to_year1) |> 
               group_by(year_quarter) |> 
               summarise(variable1 = sum(variable1)) |> 
               ggplot(aes(x = year_quarter)) +
               geom_col(aes(y = variable1), fill = "blue", alpha = 0.5))}
  # Exact same thing as above except for summarizing by mean().
  else {ggplotly(dictionary1() |>
                   filter(org==input$subset_gics1) |> 
                   filter(
                     year >= input$from_year1,
                     year <= input$to_year1) |> 
                   group_by(year_quarter) |> 
                   summarise(variable1 = mean(variable1)) |> 
                   ggplot(aes(x = year_quarter)) +
                   geom_col(aes(y = variable1), fill = "blue", alpha = 0.5 ))}
  })
 


output$table1 <- renderPlot({
  req(input$subset_gics1, input$func1, input$dictionary1)
  # Start with dictionary1()
  dictionary1() |>
    filter(org==input$subset_gics1) |> 
    select(revenue, earnings, forecast, difference, n_lnm:sentiment_index) |> 
    correlate() |> 
    stretch() |>
    ggplot(aes(x = x, y = y, fill = r)) +
    geom_tile() +
    geom_text(aes(label = round(r, 2))) +
    scale_fill_gradient2(
      low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
      limits = c(-1, 1)
    ) +
    scale_x_discrete(expand=c(0.001,0.001)) +
    scale_y_discrete(expand=c(0.001,0.001)) +
    labs(
      title = "Correlation Matrix",
      x = "", y = ""
    )
    # filter(org==input$subset_gics1) |>
    # filter(year >= input$from_year1,
    #        year <= input$to_year1) |>
    # # For lagged variable, we need to worry about properly ordering the
    # # calls. Here, we sort by company and arrange by year_quarter.
    # group_by(gvkey) |> 
    # arrange(year_quarter, .by_group = TRUE) |> 
    # # Lagging variable by inputted number of lags
    # mutate(variable1 = lag(variable1, input$lag1)) |>
    # filter(is.na(variable1) == FALSE) |>
    # ungroup() |> 
    # select(revenue:clmd_count, positive_negative_prop, valenced_overall_ratio, variable1) |> 
    # # No row names which is a bummer, haven't looked much into it though.
    # cor() |> 
    # as.data.frame()|> 
    # rownames_to_column() 
    # 
})


output$text2 <- renderText({ 
  req(input$subset_gics2, input$func2, input$dictionary2)
  str_c(
    "Number of Firms: ",
    dictionary2() |> 
      filter(org==input$subset_gics2) |> 
      filter(year >= input$from_year2,
             year <= input$to_year2) |> 
      distinct(gvkey) |> 
      count() |> 
      pull())})

# Exact same as plot 1.
output$plot2 <- renderPlotly({
  req(input$subset_gics2, input$func2, input$dictionary2)
  if(input$func2 == "sum") {
    ggplotly(dictionary2() |>
               filter(org==input$subset_gics2) |> 
               filter(
                 year >= input$from_year2,
                 year <= input$to_year2) |> 
               group_by(year_quarter) |> 
               summarise(variable2 = sum(variable2)) |> 
               ggplot(aes(x = year_quarter)) +
               geom_col(aes(y = variable2), fill = "blue", alpha = 0.5)) }
  else {ggplotly(dictionary2() |>
                   filter(org==input$subset_gics2) |> 
                   filter(
                       year >= input$from_year2,
                       year <= input$to_year2) |> 
                   group_by(year_quarter) |> 
                   summarise(variable2 = mean(variable2)) |> 
                   ggplot(aes(x = year_quarter)) +
                   geom_col(aes(y = variable2), fill = "blue", alpha = 0.5)) }
  })
  
  

# Ditto as above
output$table2 <- renderPlot({
  req(input$subset_gics2, input$func2, input$dictionary2)
  dictionary2() |>
    filter(org==input$subset_gics2) |> 
    select(revenue, earnings, forecast, difference, n_lnm:sentiment_index) |> 
    correlate() |> 
    stretch() |>
    ggplot(aes(x = x, y = y, fill = r)) +
    geom_tile() +
    geom_text(aes(label = round(r, 2))) +
    facet_wrap(
      ~ .data[[name]], scales = "free",
      nrow = round(length(unique(id_counts[[name]])) / 3),
      ncol = round(length(unique(id_counts[[name]])) / 4)
    ) +
    scale_fill_gradient2(
      low = "#FF0000", mid = "#FFFFFF", high = "#56B1F7",
      limits = c(-1, 1)
    ) +
    scale_x_discrete(expand=c(0.001,0.001)) +
    scale_y_discrete(expand=c(0.001,0.001)) +
    labs(
      title = "Correlation Matrix",
      subtitle = str_c("Correlation by ", str_to_title(name)),
      x = "", y = ""
    )
  
  })
 
 
 



# Ez Money
}
shinyApp(ui, server)

