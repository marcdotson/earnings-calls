# Load Packages.

library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)


# Load data, I need to make a separate file with all this at some point.
dash_counts <- read_rds(here::here("Data", "dash_counts.rds")) |> ungroup() |> 
  select(id:revenue, overall_count:clmd_prop, constraining_loughran:superfluous_loughran) |> 
  mutate(loughran_positive_negative_prop = round(positive_loughran/negative_loughran, 4),
         loughran_valenced_overall_ratio = round(sum(across(contains("_loughran")))/overall_count, 4))


# Presorting choices so they're alphabetical later.
d <- unique(dash_counts$year) |> sort()
sec <- unique(dash_counts$sector) |> sort()
gru <- unique(dash_counts$group) |> sort()
ind <- unique(dash_counts$industry) |> sort()
sub <- unique(dash_counts$sub_industry) |> sort()

# Choices to select for GICS subset.
gics_select <- dash_counts |> select(sector:sub_industry)

# Variable choices.
var_select <- dash_counts |> select(revenue:loughran_valenced_overall_ratio)

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
      numericInput(
        inputId = "lag1",
        label = "Select Variable Lag in Quarters--Table 1",
        value = 0,
        min=0,
        max=8
        ),
      
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
      
      # Select number of lag periods for variable of interest.
      numericInput(
        inputId = "lag2",
        label = "Select Variable Lag in Quarters--Table 2",
        value = 0,
        min=0,
        max=8
      ),
      
      # Select GICS organization level.
      varSelectInput(
        inputId = "varlevel2",
        label = "Select GICS Level--Graph 2",
        data = dash_select),
      
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
    # Specify what plot 1 is in the server function.
    plotlyOutput("plot1"),
    # Plot 2.
    plotlyOutput("plot2"),
    # Table 1.
    tableOutput("table1"),
    # Table 2.
    tableOutput("table2")
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

# Plotly allows for some cool interactivity, at the expense of performance.

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
  
  
  

output$table1 <- renderTable(
  # Start with dictionary1()
  dictionary1() |>
    filter(org==input$subset_gics1) |>
      filter(year >= input$from_year1,
             year <= input$to_year1) |>
      # For lagged variable, we need to worry about properly ordering the
      # calls. Here, we sort by company and arrange by year_quarter.
      group_by(gvkey) |> 
      arrange(year_quarter, .by_group = TRUE) |> 
      # Lagging variable by inputted number of lags
      mutate(lagged_variable1 = lag(variable1, input$lag1)) |>
      filter(is.na(lagged_variable1) == FALSE) |>
      ungroup() |> 
      select(revenue:clmd_count, loughran_positive_negative_prop, loughran_valenced_overall_ratio, variable1, lagged_variable1) |> 
      # No row names which is a bummer, haven't looked much into it though.
      cor()
    )
  

# Ditto as above
output$table2 <- renderTable(
  dictionary2() |>
    filter(org==input$subset_gics2) |>
      filter(year >= input$from_year2,
             year <= input$to_year2) |> 
      group_by(gvkey) |> 
      arrange(year_quarter, .by_group = TRUE) |> 
      mutate(lagged_variable2 = lag(variable2, input$lag2)) |>
      filter(is.na(lagged_variable2) == FALSE) |>
      ungroup() |> 
      select(revenue:clmd_count, loughran_positive_negative_prop, loughran_valenced_overall_ratio, variable2, lagged_variable2) |> 
      cor()
                                  
 )
 
 
 
}


# Ez Money
shinyApp(ui, server)


