library(dplyr)
library(tidyr)
library(ggplot2)
library(gsheet)
library(shiny)
library(viridisLite)
library(grid)
# library(ggrepel)

# # create PsyChild data
# no_of_rows <- 120
# authors = c("Hammer", "Nail", "Tweezer", "Drill", "Screw")
# year.range <- c(1900, 2022)
# compound <- c("L", "k", "c") # c("L", "k", "c")
# countries <- c("US", "Germany", "Namibia", "Denmark")
# PS.data <- tibble(author = authors[round(runif(no_of_rows, 1, length(authors)))],
#                   year = round(runif(no_of_rows, year.range[1], year.range[2])),
#                   country = countries[round(runif(no_of_rows, 1, length(countries)))],
#                   dummy_var = compound[round(runif(no_of_rows, 1, length(compound)))]) %>% 
#   arrange(year, country, dummy_var)

# # create dummy rows for dummy_var = all
# PS.data <- rbind(PS.data, PS.data %>% 
#         mutate(dummy_var = "all")) %>% 
#   mutate(dummy_var = factor(dummy_var, levels = c("all", "L", "k", "c")))


# get PsyChild data
PS.data <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/edit?usp=sharing')

# change current to 2023
PS.data$Date[PS.data$Date == "Current"] <- 2023

# change Date to numeric
PS.data$Date <- as.numeric(PS.data$Date)

# rename some columns
PS.data <- rename(PS.data, Class = 'Substance class')
PS.data <- rename(PS.data, Compound = 'Psychedlic Compound(s) in children/adolescents')
PS.data <- rename(PS.data, Indication = 'Indication (for children/adolescents)/Field of Application')
PS.data <- rename(PS.data, Indication_ICD11 = 'Indication (Current terminology according to ICD-11)')
PS.data <- rename(PS.data, Psychotherapy = 'Adjacent psychotherapy?')
PS.data <- rename(PS.data, Psychiatric_indication = 'Psychiatric indication?')

# replace "Cannabinoid receptor agonist" with "Can. rec. ant."
PS.data$Class[PS.data$Class == "Cannabinoid receptor agonist"] <- "Can. rec. ant."

# remove Date == NA columns
PS.data <- PS.data %>% 
  filter(!is.na(Date))

# arrange PS_data
PS.data <- PS.data %>% 
  arrange(Date, Author, Class)

# get unique classes
classes <- sort(unique(unlist(strsplit(PS.data$Class, split = "; "))))

# User interface ----
ui <- navbarPage("My Application",
                 tabPanel("Classes and compounds",
                          sidebarLayout(
                            sidebarPanel(
                              helpText(h3("Classes through time and space")), # "Visualize psychedelic drug use in children through time and space"
                              
                              # selectInput("Class",
                              #             label = "Choose one or more class(es) to display",
                              #             choices = classes,
                              #             selected = classes[7]),
                              
                              checkboxGroupInput("Class",
                                                 # h3("Class"),
                                                 label = "Choose one or more class(es) to display",
                                                 choices = list("Deliriant",
                                                                "Dissociative",
                                                                "Endocannabinoids",
                                                                "Entactogen",
                                                                "Harmala alkaloids",
                                                                "Phytocannbinoids",
                                                                "Psychedelic",
                                                                "Synthetic cannabinoids",
                                                                "Can. rec. ant.",
                                                                "all"),
                                                 selected = "all"),
                              
                              sliderInput("range",
                                          label = "Years of interest:",
                                          min = min(PS.data$Date),
                                          max = max(PS.data$Date),
                                          value = c(min(PS.data$Date),
                                                    max(PS.data$Date)),
                                          step = 1,
                                          sep = '')),
                            
                            mainPanel(
                              verbatimTextOutput("class_selected"),
                              plotOutput("studies_over_year_plot"),
                              # plotOutput("test_plot"),
                              dataTableOutput("table_print")
                            )
                          )),
                 tabPanel("something else"),
                 tabPanel("Map")
)

# Server logic ----
server <- function(input, output) {
  # output$test_plot <- renderPlot({
  #   # plot(1:10)
  #   ggplot(data = data.frame(x=1:10, y=1:10), aes(x=x, y=y)) + 
  #     geom_point()
  # })
  output$class_selected <-renderText({
    paste0("Classes selected: ", input$Class, ". Note that the labels of 'Harmala alkaloids', 'Deliriants', and 'Can. rec. ant.s' may overlap du to similar values.")
  })
  
  output$studies_over_year_plot <- renderPlot({
    # # testing
    # input=list(range_years = c(1839, 2023),
    #            Class = "all") # "Dissociative" "all"
    
    # filter by input range
    PS.data.plot <- PS.data %>%
      filter(Date >= input$range[1],
             Date <= input$range[2])
    
    # arrange by date and add cumulative columns
    PS.data.plot <- PS.data.plot %>%
      arrange(Date) %>%
      mutate(one = 1,
             cumul_years_all = cumsum(one)) %>%
      group_by(Class) %>%
      mutate(cumul_year_class = cumsum(one)) %>%
      ungroup() %>%
      select((-one))
    
    # select input classes
    if("all" %in% input$Class == FALSE){
      PS.data.plot <- PS.data.plot %>%
        filter(Class %in% input$Class)
    } else {
      PS.data.plot <- PS.data.plot
    }
    
    # add all missing years
    classes.selected <- unique(PS.data.plot$Class)
    for(i in classes.selected){
      # i <- classes.selected[1]
      missing_years <- setdiff(input$range[1]:input$range[2], PS.data.plot$Date[PS.data.plot$Class == i])
      PS.data.plot <- PS.data.plot %>% 
        add_row(Class = i,
                Date = missing_years)
      PS.data.plot <- PS.data.plot %>% 
        arrange(Class, Date)
    }
    
    # add line at first year of current selection table
    first_year <- input$range[1]
    for(i in classes.selected){
      # i <- classes.selected[1]
      curr_author <- PS.data.plot %>%
        filter(Date == first_year, Class == i) %>%
        pull(Author)
      if(is.na(curr_author)){
        PS.data.plot$cumul_year_class[PS.data.plot$Class == i & PS.data.plot$Date == first_year] <- 0
      }
    }
    
    # fill empty years with previous cumul_year_class value
    PS.data.plot <- PS.data.plot %>% 
      group_by(Class) %>% 
      fill(cumul_year_class)
    
    p <- ggplot(data = PS.data.plot,
                aes(x=Date, y=cumul_year_class,
                    group = Class, col = Class, fill = Class)) +
      # geom_bar(data = PS.data.plot %>%
      #            filter(Class != "all"),
      #          aes(x=Date, y=one,
      #              group = Class),
      #          stat="identity", show.legend = FALSE) +
      geom_step(linewidth = 2, alpha = 0.75) +
      # xlim(input$range[1]-1, input$range[2]+10) +
      # expand_limits(y=-1) +
      labs(x = "Date", y = "Cumulative references") +
      scale_colour_viridis_d() +
      theme_bw()  +
      geom_text(data = PS.data.plot %>% 
                  filter(Date == input$range[2]) %>% 
                  slice(max(row_number())), 
                aes(label = Class, colour = Class, x = Inf, y = cumul_year_class), 
                hjust = -.1,
                size=5) +
      # geom_label_repel(aes(label = Class),
      #                  nudge_x = 1,
      #                  na.rm = TRUE)
      # geom_text(data = PS.data.plot %>%
      #             group_by(Class) %>%
      #             filter(Date == median(Date, na.rm = TRUE)) %>%
      #             slice(1),
      #           aes(label = Class,
      #               x = Date + 0.0,
      #               y = cumul_year_class+1,
      #               color = Class),
    #           size = 5) +
    theme(legend.position="none",
          plot.margin = unit(c(1,10,1,1), "lines")) 
      # theme(plot.margin = unit(c(1,3,1,1), "lines")) 
    
    gt <- ggplotGrob(p)
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)
  })
  
  output$table_print <- renderDataTable(PS.data %>% 
                                          arrange(Date) %>% 
                                          filter(Date >= input$range[1],
                                                 Date <= input$range[2]),
                                        options = list(pageLength = 1000,
                                                       searching = FALSE))
}

# Run app ----
shinyApp(ui, server)
