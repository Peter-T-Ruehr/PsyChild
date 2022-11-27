library(dplyr)
library(ggplot2)
library(gsheet)
library(shiny)

# get PsyChild data
# PS.data <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/15HCv7XFoWN9qnOVeg8tY1Gc2hH-b8yRawTaXDRyPb9Y/edit?usp=sharing') %>%
#   arrange(year, country, dummy_var)

# create PsyChild data
no_of_rows <- 120
authors = c("Hammer", "Nail", "Tweezer", "Drill", "Screw")
year.range <- c(1900, 2022)
dummy_vars <- c("L", "k", "c") # c("L", "k", "c")
countries <- c("US", "Germany", "Namibia", "Denmark")
PS.data <- tibble(author = authors[round(runif(no_of_rows, 1, length(authors)))],
                  year = round(runif(no_of_rows, year.range[1], year.range[2])),
                  country = countries[round(runif(no_of_rows, 1, length(countries)))],
                  dummy_var = dummy_vars[round(runif(no_of_rows, 1, length(dummy_vars)))]) %>% 
  arrange(year, country, dummy_var)

# create dummy rows for dummy_var = all
PS.data <- rbind(PS.data, PS.data %>% 
        mutate(dummy_var = "all")) %>% 
  mutate(dummy_var = factor(dummy_var, levels = c("all", "L", "k", "c")))


# User interface ----
ui <- fluidPage(
  titlePanel("Test data"), # "Clinical psychedelics in children"
  
  sidebarLayout(
    sidebarPanel(
      helpText("Test data through time and space"), # "Visualize psychedelic drug use in children through time and space"
      
      selectInput("dummy_var", 
                  label = "Choose one or more dummy_vars to display",
                  choices = c(levels(PS.data$dummy_var)),
                  selected = "all"),
      
      sliderInput("range_years",
                  label = "Years of interest:",
                  min = min(PS.data$year),
                  max = max(PS.data$year),
                  value = c(min(PS.data$year),
                            max(PS.data$year)),
                  step = 1,
                  sep = '')
    ),
    
    mainPanel(
      plotOutput(outputId = "studies_over_year_plot"),
      
      dataTableOutput("table_print")
    )
  )
)

# Server logic ----
server <- function(input, output) {
  output$studies_over_year_plot <- renderPlot({
    # # testing
    # input=list(dummy_var = "all", range_years = c(1900, 2022))
    
    PS.data.years <- PS.data %>% 
      filter(year >= input$range_years[1],
             year <= input$range_years[2]) %>%
      arrange(year) %>% 
      mutate(one = 1,
             cumul_years_all = row_number(), 
             cumul_years_all = cumsum(one)) %>% 
      group_by(dummy_var) %>% 
      mutate(cumul_year_dummy_var = cumsum(one)) %>% 
      ungroup()
    
    data.plot = switch(input$dummy_var,
                       "L" = PS.data.years %>% 
                         filter(dummy_var == "L"),
                       "k" = PS.data.years %>% 
                         filter(dummy_var == "k"),
                       "c" = PS.data.years %>% 
                         filter(dummy_var == "c"),
                       "all" = PS.data.years)
    
    s = dummy_vars[2]
    for(s in unique(data.plot$dummy_var)){
      if(nrow(data.plot %>%
              filter(dummy_var == s,
                     year == input$range_years[1])) == 0){
        data.plot <- data.plot %>%
          add_row(year = input$range_years[1],
                  dummy_var = s,
                  one = 0,
                  cumul_years_all = 0,
                  cumul_year_dummy_var = 0)
      }
    }
    
    cols <- c("all"="black", "L"="orange", "k"="blue", "c"="red")
    
    ggplot(data = data.plot,
           aes(x=year, y=cumul_year_dummy_var,
               group = dummy_var, col = dummy_var, fill = dummy_var)) + 
      geom_step() + 
      geom_bar(data = data.plot %>% 
                 filter(dummy_var != "all"),
               aes(x=year, y=one,
                   group = dummy_var, col = "black"),
               stat="identity", show.legend = FALSE) + 
      scale_color_manual(values = cols, drop = FALSE) +
      scale_fill_manual(values = cols, drop = FALSE) +
      xlim(input$range_years) +
      labs(x = "Year", y = "Cumulative references") +
      theme_bw()
  })
  
  output$table_print <- renderDataTable(PS.data %>% 
                                          arrange(year),
                                        options = list(pageLength = 1000))
}

# Run app ----
shinyApp(ui, server)
