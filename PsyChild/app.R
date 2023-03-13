library(dplyr)
library(tidyr)
library(ggplot2)
library(gsheet)
library(shiny)
library(viridisLite)
# library(grid)
library(ggrepel)
library(googlesheets4)
gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/"
PS.data <- read_sheet(sheet_id, sheet = 1)
# remove unnamed columns
PS.data <- PS.data %>% 
  select(-contains('...'))
# get PsyChild data
# PS.data <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/edit?usp=sharing')

# remove NULL rows
nulls <- NULL
for(i in 1:nrow(PS.data)){
  curr_row <- unlist(PS.data$Date[i])
  if(is.null(curr_row)){
    nulls <- c(nulls, i)
  }
}
PS.data <- PS.data[-nulls, ]

# change Date from list to readable character
PS.data$Date <- unlist(PS.data$Date)

# change current to 2023
PS.data$Date[PS.data$Date == "Current"] <- 2023

# change Date to numeric
PS.data$Date <- as.numeric(PS.data$Date)

# rename some columns
PS.data <- rename(PS.data, Class = 'Substance class')
PS.data <- rename(PS.data, Compound = 'Compound(s)') # was before: 'Psychedelic Compound(s) in children/adolescents')
# PS.data <- rename(PS.data, Indication = 'Indication (for children/adolescents)/Field of Application')
# PS.data <- rename(PS.data, Indication_ICD11 = 'Indication (Current terminology according to ICD-11)')
# PS.data <- rename(PS.data, Psychotherapy = 'Adjacent psychotherapy?')
# PS.data <- rename(PS.data, Psychiatric_indication = 'Psychiatric indication?')

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

# get unique compounds
compounds <- sort(unique(unlist(strsplit(PS.data$Compound, split = "; "))))

# Class and compound: arrange by date and add cumulative columns
PS.data <- PS.data %>%
  arrange(Date) %>%
  mutate(one = 1,
         cumul_years_all = cumsum(one)) %>%
  group_by(Class) %>%
  mutate(cumul_year_class = cumsum(one)) %>%
  ungroup()

# save for printing
PS.data.print_Class <- PS.data
PS.data.print_Compound <- PS.data

# save for later processing
PS.data.compounds <- PS.data

# add all missing years
# Class
for(i in classes){
  # i <- classes.selected[1]
  missing_years <- setdiff(min(PS.data$Date):max(PS.data$Date), PS.data$Date[PS.data$Class == i])
  PS.data <- PS.data %>%
    add_row(Class = i,
            Date = missing_years) %>% 
    arrange(Class, Date)
}

# separate multiple compounds from each other
# i=1
# i=34
for(i in 1:nrow(PS.data.compounds)){
  # if compound cell contains ";"
  if(grepl(";", PS.data.compounds$Compound[i])){
    curr_compounds <- unlist(strsplit(PS.data.compounds$Compound[i], "; "))
    for(k in curr_compounds){
      PS.data.compounds <- PS.data.compounds %>% 
        # add_row(Author = PS.data.compounds$Author[i],
        #         Date = PS.data.compounds$Date[i],
        #         Class = PS.data.compounds$Class[i],
        #         Compound = k)
        add_row(PS.data.compounds[i, ])
      # replace original compounds with new compounds
      PS.data.compounds$Compound[nrow(PS.data.compounds)] <- k
    }
    # remove original row
    PS.data.compounds <- PS.data.compounds[-i, ]
  }
}

PS.data.compounds <- PS.data.compounds %>%
  arrange(Date) %>%
  group_by(Compound) %>%
  mutate(cumul_year_compound = cumsum(one)) %>%
  ungroup() %>%
  select((-one))

# class -------------------------------------------------------------------
# add line at first year of current selection table
first_year <- min(PS.data$Date) # input$range[1]
for(i in classes){
  # i <- classes.selected[1]
  curr_author <- PS.data %>%
    filter(Date == first_year, Class == i) %>%
    pull(Author)
  if(is.na(curr_author)){
    PS.data$cumul_year_class[PS.data$Class == i & PS.data$Date == first_year] <- 0
  }
}

# fill empty years with previous cumul_year_class value
PS.data <- PS.data %>% 
  group_by(Class) %>% 
  fill(cumul_year_class)

# define constant viridis colours and add to PS.data
cols_class <- tibble(Class = unique(PS.data$Class), col_class = viridis(n=length(unique(PS.data$Class))))
PS.data <- PS.data %>% 
  left_join(cols_class, by = "Class")
# plot(1:nrow(cols), col = unique(PS.data$col), pch = 16, cex = 5)
# plot(1:nrow(cols), col = unique(PS.data.plot.Class$col), pch = 16, cex = 5)

# compound -------------------------------------------------------------------
# add all missing years
for(i in compounds){
  # i <- classes.selected[1]
  missing_years <- setdiff(min(PS.data.compounds$Date):max(PS.data.compounds$Date), PS.data.compounds$Date[PS.data.compounds$Compound == i])
  PS.data.compounds <- PS.data.compounds %>%
    add_row(Compound = i,
            Date = missing_years)
  # PS.data <- PS.data %>%
  #   arrange(Class, Date)
}

# add line at first year of current selection table
first_year <- min(PS.data.compounds$Date) # input$range[1]
i = "2-AG"
for(i in compounds){
  # i <- classes.selected[1]
  curr_author <- PS.data.compounds %>%
    filter(Date == first_year, Compound == i) %>%
    pull(Author)
  if(is.na(curr_author)){
    PS.data.compounds$cumul_year_compound[PS.data.compounds$Compound == i & PS.data.compounds$Date == first_year] <- 0
  }
}

# arrange before filling
PS.data.compounds <- PS.data.compounds %>% 
  arrange(Compound, Date) 

# fill empty years with previous cumul_year_compound value
PS.data.compounds <- PS.data.compounds %>% 
  group_by(Compound) %>% 
  fill(cumul_year_compound)

# define constant viridis colours and add to PS.data
cols_compound <- tibble(Compound = unique(PS.data.compounds$Compound), col_compound = viridis(n=length(unique(PS.data.compounds$Compound))))
PS.data.compounds <- PS.data.compounds %>% 
  left_join(cols_compound, by = "Compound")
# plot(1:nrow(cols_compound), col = unique(PS.data.compounds$col_compound), pch = 16, cex = 5)

# User interface ----
ui <- navbarPage("PsyChild",
                 # Classes -----------------------------------------------------------------
                 tabPanel("Classes",
                          sidebarLayout(
                            sidebarPanel(
                              helpText(h3("Tracking clinical psychedelics in children and adolescents.")), # "Visualize psychedelic drug use in children through time and space"
                              
                              # selectInput("Class",
                              #             label = "Choose one or more class(es) to display",
                              #             choices = classes,
                              #             selected = classes[7]),
                              
                              checkboxGroupInput("Class",
                                                 # h3("Class"),
                                                 label = "Choose one or more class(es) to display",
                                                 choices = list("Deliriants",
                                                                "Dissociatives",
                                                                "Entactogens",
                                                                "MAOIs",
                                                                "Phytocannabinoids",
                                                                "Psychedelics",
                                                                "Synthetic cannabinoids",
                                                                "all"),
                                                 selected = "all"),
                              
                              sliderInput("range",
                                          label = "Years of interest:",
                                          min = min(PS.data$Date),
                                          max = max(PS.data$Date),
                                          value = c(min(PS.data$Date), # c(min(PS.data$Date), 1950,
                                                    max(PS.data$Date)),
                                          step = 1,
                                          sep = '')),
                            
                            mainPanel(
                              verbatimTextOutput("class_selected"),
                              plotOutput("studies_over_year_plot_Class"),
                              # plotOutput("test_plot"),
                              div(dataTableOutput("table_print_Class"), style = "font-size:80%")
                            )
                          )),
                 
                 # Compounds --------------------------------------------------------------
                 tabPanel("Compounds",
                          sidebarLayout(
                            sidebarPanel(
                              helpText(h3("Tracking clinical psychedelics in children and adolescents.")),
                              
                              # # testing
                              # selectInput("Compound",
                              #             label = "Choose one or more Compound(es) to display",
                              #             choices = Compounds,
                              #             selected = compounds[7]),
                              
                              checkboxGroupInput("Compound",
                                                 # h3("Compound"),
                                                 label = "Choose one or more compound(s) to display",
                                                 choices = list(" 2-AG (2-Arachidonylglycerol)",
                                                                "AA",
                                                                "AEA (Anandamid)",
                                                                "Delta-8-tetrahydrocannabinol (delta-8-THC)",
                                                                "Dexanabinol",
                                                                "Dronabinol",
                                                                "Esketamine",
                                                                "Harmaline",
                                                                "Harmine",
                                                                "Iofetamine",
                                                                "Ketamine",
                                                                "Ketodex",
                                                                "Ketofol",
                                                                "LAE-32 (D-Lysergic acid ethylamide)",
                                                                "Lenabasum",
                                                                "Levonantradol",
                                                                "LSD (Lysergic acid diethylamide)",
                                                                "Marinol",
                                                                " mCPP (meta-Chlorphenylpiperazin)",
                                                                "Mescaline",
                                                                "Methysergide",
                                                                "Nabilone",
                                                                "Nabiximols",
                                                                "Naboline",
                                                                "OEA (Oleoylethanolamide)",
                                                                "PCP (Phencyclidin)",
                                                                "PEA (Palmitoylethanolamid)",
                                                                "Physostigmine",
                                                                "Phytocannabinoids",
                                                                "Psilocybin",
                                                                "Scopolamine",
                                                                "THC",
                                                                "αET (alpha-Ethyltryptamine)",
                                                                "all"),
                                                 selected = "all"),
                              
                              sliderInput("range_compounds",
                                          label = "Years of interest:",
                                          min = min(PS.data$Date),
                                          max = max(PS.data$Date),
                                          value = c(min(PS.data$Date), # c(min(PS.data$Date), 1950,
                                                    max(PS.data$Date)),
                                          step = 1,
                                          sep = '')),
                            
                            mainPanel(
                              verbatimTextOutput("compound_selected"),
                              plotOutput("studies_over_year_plot_Compound"),
                              # plotOutput("test_plot"),
                              
                              div(dataTableOutput("table_print_Compound"), style = "font-size:80%")
                            )
                          )),
                 tabPanel("Map")
)


# Server logic ------------------------------------------------------------
server <- function(input, output) {
  # Class -------------------------------------------------------------------  
  
  # output$test_plot <- renderPlot({
  #   # plot(1:10)
  #   ggplot(data = data.frame(x=1:10, y=1:10), aes(x=x, y=y)) + 
  #     geom_point()
  # })
  
  output$class_selected <-renderText({
    paste0("Classes selected: ", paste(input$Class, collapse = ", "), ".")
  })
  
  output$studies_over_year_plot_Class <- renderPlot({
    
    # # testing
    # input=list(range = c(1839, 2023), # 1839 1950 2023 1980
    #            range_compounds = c(1950, 1980), # 1839 1950 2023 1980
    #            Class = c("Dissociatives, Entactogens"), # "Dissociatives" "all"
    #            Compound = c("LSD"))
    
    
    
    # filter by input range
    PS.data.plot.Class <- PS.data %>%
      filter(Date >= input$range[1],
             Date <= input$range[2])
    
    # select input classes
    if("all" %in% input$Class == FALSE){
      PS.data.plot.Class <- PS.data.plot.Class %>%
        filter(Class %in% unlist(strsplit(input$Class, split = ", ")))
    } else {
      PS.data.plot.Class <- PS.data.plot.Class
    }
    
    
    p <- ggplot(data = PS.data.plot.Class,
                aes(x=Date, y=cumul_year_class,
                    col = Class)) +
      geom_step(linewidth = 2, alpha = 0.75) +
      ylim(0, max(PS.data$cumul_year_class)) +
      labs(x = "Date", y = "Cumulative references") +
      scale_color_manual(values = c(unique(PS.data.plot.Class$col_class))) +
      theme_bw() +
      scale_x_continuous(breaks = round(seq(1840, max(PS.data.plot.Class$Date), by = 10),1)) #+
      # scale_y_continuous(breaks = round(seq(min(PS.data.plot.Class$cumul_year_class), max(PS.data.plot.Class$cumul_year_class), by = 10),1))
    p +
      theme(legend.position="bottom",
            axis.text.x = element_text(angle = 45, hjust=1)) # vjust = 0.5, 
  })
  
  output$table_print_Class <- renderDataTable({df <- reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <- PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2])
  
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    PS.data.print_Class <- PS.data.print_Class %>%
      filter(Class %in% unlist(strsplit(input$Class, split = ", ")))
  } else {
    PS.data.print_Class <- PS.data.print_Class
  }
  
  PS.data.print_Class <- PS.data.print_Class %>% 
    arrange(Date, Class) %>% 
    rename(`Substance class` = Class,
           `Compound(s)` = Compound) %>% 
    select(c(# `Date`,
      `Author`,
      `Location`,
      # `Location photo`,
      `Title`,
      `Type`,
      `Compound(s)`,
      `Substance class`,
      `ICD-11 Indication or field of application`,
      `ICD-11 Indication as Groups or field of application`,
      # `Psychiatric indication?`,
      `Adjunct psychotherapy`,
      # `Adjacent psychotherapy?`,
      `Subjects`,
      # `Only children and adolescents?`,
      `Main psychiatric outcomes`,
      # `Reported side effects/adverse events`,
      `Side effects (MedDRA)`,
      `Consent`,
      `in/out patient`,
      # `Route of administration`,
      `Regimen (route of administration, dose, frequency)`,
      `Concomitant Medications`,
      `Comment`#,
      # `comment 1`,
      # `comment 2`
    ))
  
  PS.data.print_Class},
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE))
  
  # Compound -------------------------------------------------------------------  
  
  # output$test_plot <- renderPlot({
  #   # plot(1:10)
  #   ggplot(data = data.frame(x=1:10, y=1:10), aes(x=x, y=y)) + 
  #     geom_point()
  # })
  
  output$compound_selected <-renderText({
    paste0("Compounds selected: ", paste(input$Compound, collapse = ", "), ".")
  })
  
  output$studies_over_year_plot_Compound <- renderPlot({
    
    # # testing
    # input=list(range = c(1839, 2023), # 1839 1950 2023 1980
    #            range_compounds = c(1839, 2023), # 1839 1950 2023 1980
    #            Class = c("Dissociatives, Entactogens"), # "Dissociatives" "all"
    #            Compound = c("αET (alpha-Ethyltryptamine)")) # LSD Phytocannabinoids αET (alpha-Ethyltryptamine) all OEA (Oleoylethanolamide)
    c <- 1
    for (c in length(input$Compound)) {
      if(grepl("\\(", input$Compound[c])){
        input$Compound[c] <- gsub(".*\\((\\w+)\\).*", "\\1", input$Compound[c])
      }
    }
    
    
    # filter by input range
    PS.data.compounds.plot <- PS.data.compounds  %>% 
      ungroup() %>%
      filter(Date >= input$range_compounds[1],
             Date <= input$range_compounds[2])
    
    # select input compounds
    if("all" %in% input$Compound == FALSE){
      # i=2
      tmp <- NULL
      for(i in 1:length(unlist(strsplit(input$Compound, split = ", ")))){
        tmp <- rbind(tmp, PS.data.compounds.plot %>%
                       ungroup() %>%
                       # filter(Compound %in% unlist(strsplit(input$Compound, split = ", ")))
                       filter(grepl(unlist(strsplit(input$Compound[i], split = ", ")), Compound))) # "Oleoylethanolamide"
      }
      PS.data.compounds.plot <- tmp
      rm(tmp)
    } else {
      PS.data.compounds.plot <- PS.data.compounds.plot
    }
    
    
    p <- ggplot(data = PS.data.compounds.plot,
                aes(x=Date, y=cumul_year_compound,
                    col = Compound)) +
      geom_step(linewidth = 2, alpha = 0.75) +
      ylim(0, max(PS.data.compounds$cumul_year_compound)) +
      labs(x = "Date", y = "Cumulative references") +
      scale_color_manual(values = c(unique(PS.data.compounds.plot$col_compound))) +
      theme(legend.position="bottom") +
      theme_bw() +
      scale_x_continuous(breaks = round(seq(1840, max(PS.data.compounds.plot$Date), by = 10),1))
    p +
      theme(legend.position="bottom")
  })
  
  output$table_print_Compound <- renderDataTable({df <- reactiveVal(PS.data.print_Compound)
  
  PS.data.print_Compound <- PS.data.print_Compound %>% 
    arrange(Date) %>% 
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2])
  
  
  # select input compounds
  # if("all" %in% input$Compound == FALSE){
  #   PS.data.print_Compound <- PS.data.print_Compound %>%
  #     filter(grepl(unlist(strsplit(input$Compound, split = ", ")), Compound))
  # } else {
  #   PS.data.print_Compound <- PS.data.print_Compound
  # }
  if("all" %in% input$Compound == FALSE){
    # i=2
    tmp <- NULL
    for(i in 1:length(unlist(strsplit(input$Compound, split = ", ")))){
      tmp <- rbind(tmp, PS.data.print_Compound %>%
                     # filter(Compound %in% unlist(strsplit(input$Compound, split = ", ")))
                     filter(grepl(unlist(strsplit(input$Compound[i], split = ", ")), Compound)))
    }
    PS.data.print_Compound <- tmp
    rm(tmp)
  } else {
    PS.data.print_Compound <- PS.data.print_Compound
  }
  
  PS.data.print_Compound %>% 
    arrange(Date, Compound) %>% 
    rename(`Substance class` = Class,
           `Compound(s)` = Compound) %>% 
    select(c(# `Date`,
      `Author`,
      `Location`,
      # `Location photo`,
      `Title`,
      `Type`,
      `Compound(s)`,
      `Substance class`,
      `ICD-11 Indication or field of application`,
      `ICD-11 Indication as Groups or field of application`,
      # `Psychiatric indication?`,
      `Adjunct psychotherapy`,
      # `Adjacent psychotherapy?`,
      `Subjects`,
      # `Only children and adolescents?`,
      `Main psychiatric outcomes`,
      # `Reported side effects/adverse events`,
      `Side effects (MedDRA)`,
      `Consent`,
      `in/out patient`,
      # `Route of administration`,
      `Regimen (route of administration, dose, frequency)`,
      `Concomitant Medications`,
      `Comment`#,
      # `comment 1`,
      # `comment 2`
    ))},
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE))
}



# Run app ----
shinyApp(ui, server)
