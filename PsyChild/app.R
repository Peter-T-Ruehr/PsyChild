library(dplyr)
library(tidyr)
library(shiny)
library(viridisLite)
library(googlesheets4)
gs4_deauth()
library(plotly)
library(DT)

# Add directory of static resources to Shiny's web server
addResourcePath(prefix = "images", directoryPath = "./images/")

PS.data<<- NULL
iso_codes<<- NULL
PS.data.print_Class<<- NULL
PS.data.compounds<<- NULL
PS.data.print_Compound<<- NULL
PS.data.map.reduced<<- NULL
further_reading<<- NULL

readData <<- function(session) {
  progress <<- Progress$new(session)
  progress$set(value = 0.1, message = 'Loading...')
  
  sheet_id <<- "https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/"
  PS.data <<- read_sheet(sheet_id, sheet = "PsychChild")
  
  progress$set(value = 0.25, message = 'Loading...')
  
  # # get Author names of date before and after Current/Ongoing/Discontinued Trials
  # Current_Ongoing_Discontinued_row <<- which(PS.data$Location == "Current/Ongoing/Discontinued Trials")
  # Current_Ongoing_Discontinued_row <- which(PS.data$Location == "Current/Ongoing/Discontinued Trials")
  # 
  # Authors_before <<- PS.data$Author[1:(Current_Ongoing_Discontinued_row-1)]
  # Authors_before <<- Authors_before[!is.na(Authors_before)]
  # 
  # 
  # Authors_after <<- PS.data$Author[(Current_Ongoing_Discontinued_row+1):nrow(PS.data)]
  # Authors_after <<- Authors_after[!is.na(Authors_after)]
  
  
  
  # remove unnamed columns
  PS.data <<- PS.data %>% 
    select(-contains('...'))
  # PS.data <- PS.data %>%
  #   select(-contains('...'))
  
  # get iso codes
  iso_codes <<- read_sheet(sheet_id, sheet = "iso_codes")
  
  # remove NULL rows
  nulls <<- NULL
  for(i in 1:nrow(PS.data)){
    curr_row <<- unlist(PS.data$Date[i])
    if(is.null(curr_row)){
      nulls <<- c(nulls, i)
    }
  }
  if(!is.null(nulls)){
    PS.data <<- PS.data[-nulls, ]
  }
  
  # change Date from list to readable character
  PS.data$Date <<- unlist(PS.data$Date)
  # PS.data$Date <- unlist(PS.data$Date)
  
  # # change current to 2023 - deprecated
  # PS.data$Date[PS.data$Date == "Current"] <<- 2023
  # # PS.data$Date[PS.data$Date == "Current"] <- 2023
  
  # change Date to numeric
  PS.data$Date <<- as.numeric(PS.data$Date)
  # PS.data$Date <- as.numeric(PS.data$Date)
  
  # remove Date == NA columns
  PS.data <<- PS.data %>% 
    filter(!is.na(Date))
  # PS.data <- PS.data %>%
  #   filter(!is.na(Date))
  
  # create old new names for Compounds list
  compound_translation <<- tibble(old = c("2-AG \\(2-Arachidonoylglycerol\\)",
                                          "AA Arachidonic acid",
                                          "AEA \\(Anandamide\\)",
                                          "LAE-32 \\(D-Lysergic acid diethylamide\\)",
                                          "LSD \\(Lysergic acid diethylamide\\)",
                                          "OEA \\(Oleoylethanolamide\\)",
                                          "PCP \\(Phencyclidine\\)",
                                          "mCPP \\(meta-Chlorophenylpiperazine\\)",
                                          "PEA \\(Palmitoylethanolamide\\)",
                                          "THC \\(Delta-8-THC\\)",
                                          "THC \\(Delta-9 THC\\)",
                                          "THC \\(THC-homologs, Numbers 122 and 125A\\)",
                                          "αET \\(alpha-Ethyltryptamine\\)"),
                                  new = c("2-AG \\(2-Arachidonoylglycerol\\)",
                                          "AA \\(Arachidonic acid\\)",
                                          "AEA \\(Anandamide\\)",
                                          "LAE-32 \\(D-Lysergic acid diethylamide\\)",
                                          "LSD \\(Lysergic acid diethylamide\\)",
                                          "OEA \\(Oleoylethanolamide\\)",
                                          "PCP \\(Phencyclidine\\)",
                                          "mCPP \\(meta-Chlorophenylpiperazine\\)",
                                          "PEA \\(Palmitoylethanolamide\\)",
                                          "THC \\(Delta-8-THC\\)",
                                          "THC \\(Delta-9 THC\\)",
                                          "THC \\(THC-homologs, Numbers 122 and 125A\\)",
                                          "αET \\(alpha-Ethyltryptamine\\)"))
  
  
  
  # arrange PS_data
  PS.data <<- PS.data %>% 
    arrange(Date, Author, `Substance class`)
  
  # get unique classes
  classes<<- sort(unique(unlist(strsplit(PS.data$`Substance class`, split = "; "))))
  
  # Class and compound: arrange by date and add cumulative columns
  PS.data <<- PS.data %>%
    arrange(Date) %>%
    mutate(one = 1) %>% # ,
    # cumul_years_all = cumsum(one)) %>%
    group_by(`Substance class`) %>%
    mutate(cumul_years_class = cumsum(one)) %>%
    ungroup()
  
  # save for later processing
  PS.data.compounds <<- PS.data
  # PS.data.compounds <- PS.data
  
  # change input compounds and data compounds to workable strings
  PS.data.compounds$Compound_new_name <<- PS.data.compounds$`Compound(s)`
  for(i in 1:nrow(compound_translation)){
    PS.data.compounds$Compound_new_name <<- gsub(compound_translation$old[i], compound_translation$new[i], PS.data.compounds$Compound_new_name)
  }
  
  # get unique compounds
  compounds<<- sort(unique(unlist(strsplit(PS.data.compounds$Compound_new_name, split = "; "))))
  
  # get countries
  PS.data$Country <<- NA
  for(i in 1:nrow(PS.data)){
    curr_Location<<- unlist(strsplit(PS.data$Location[i], split = "; "))
    PS.data$Country[i] <<- curr_Location[length(curr_Location)]
  }
  PS.data$Country <<- gsub("USA", "United States of America", PS.data$Country)
  PS.data$Country <<- gsub("^United States$", "United States of America", PS.data$Country)
  PS.data$Country <<- gsub("Czechoslovakia", "Czech Republic", PS.data$Country)
  PS.data$Country <<- gsub("England", "United Kingdom", PS.data$Country)
  PS.data$Country <<- gsub("Scotland", "United Kingdom", PS.data$Country)
  PS.data$Country <<- gsub("Iran", "Iran, Islamic Republic of", PS.data$Country)
  PS.data$Country <<- gsub("Russia", "Russian Federation", PS.data$Country)
  
  # save for printing
  PS.data.print_Class <<- PS.data
  PS.data.print_Compound <<- PS.data %>% 
    ungroup()
  
  # separate multiple compounds from each other
  rows_to_remove<<- NULL
  for(i in 1:nrow(PS.data.compounds)){
    # if compound cell contains ";"
    if(grepl(";", PS.data.compounds$Compound_new_name[i])){
      curr_compounds<<- unlist(strsplit(PS.data.compounds$Compound_new_name[i], "; "))
      for(k in curr_compounds){
        PS.data.compounds <<- PS.data.compounds %>% 
          add_row(PS.data.compounds[i, ])
        # replace original compounds with new compounds
        PS.data.compounds$Compound_new_name[nrow(PS.data.compounds)] <<- k
      }
      rows_to_remove<<- c(rows_to_remove, i)
    }
  }
  # remove original rows
  PS.data.compounds <<- PS.data.compounds[-rows_to_remove, ]
  
  # cumulative years of compounds
  PS.data.compounds <<- PS.data.compounds %>%
    arrange(Date) %>%
    mutate(one = 1) %>% 
    group_by(Compound_new_name) %>%
    mutate(cumul_years_compound = cumsum(one)) %>%
    ungroup() %>%
    select((-one))
  
  progress$set(value = 0.5, message = 'Loading...')
  
  # class -------------------------------------------------------------------
  # add all missing years
  for(i in classes){
    # i<<- classes.selected[1]
    missing_years<<- setdiff(min(PS.data$Date):max(PS.data$Date), PS.data$Date[PS.data$`Substance class` == i])
    PS.data<<- PS.data %>%
      add_row(`Substance class` = i,
              Date = missing_years) %>% 
      arrange(`Substance class`, Date)
  }
  
  # add line at first year of current selection table
  first_year<<- min(PS.data$Date) # input$range[1]
  for(i in classes){
    # i<<- classes.selected[1]
    curr_author<<- PS.data %>%
      filter(Date == first_year, `Substance class` == i) %>%
      pull(Author)
    if(is.na(curr_author)){
      PS.data$cumul_years_class[PS.data$`Substance class` == i & PS.data$Date == first_year]<<- 0
    }
  }
  
  # fill empty years with previous cumul_years_class value
  PS.data<<- PS.data %>% 
    group_by(`Substance class`) %>% 
    fill(cumul_years_class)
  
  # define constant viridis colours and add to PS.data
  cols_class<<- tibble(`Substance class` = unique(PS.data$`Substance class`), col_class = viridis(n=length(unique(PS.data$`Substance class`))))
  PS.data<<- PS.data %>% 
    left_join(cols_class, by = "Substance class")
  
  
  progress$set(value = 0.75, message = 'Loading...')
  # compound -------------------------------------------------------------------
  # add all missing years
  for(i in compounds){
    # i<<- classes.selected[1]
    missing_years<<- setdiff(min(PS.data.compounds$Date):max(PS.data.compounds$Date), PS.data.compounds$Date[PS.data.compounds$Compound_new_name == i])
    PS.data.compounds <<- PS.data.compounds %>%
      add_row(Compound_new_name = i,
              Date = missing_years)
  }
  
  # add line at first year of current selection table
  first_year<<- min(PS.data.compounds$Date) # input$range[1]
  # i = "2-AG"
  for(i in compounds){
    # i<<- classes.selected[1]
    curr_author<<- PS.data.compounds %>%
      filter(Date == first_year, Compound_new_name == i) %>%
      pull(Author)
    if(is.na(curr_author)){
      PS.data.compounds$cumul_years_compound[PS.data.compounds$Compound_new_name == i & PS.data.compounds$Date == first_year] <<- 0
    }
  }
  
  # arrange before filling
  PS.data.compounds <<- PS.data.compounds %>% 
    arrange(Compound_new_name, Date) 
  
  
  # fill empty years with previous cumul_years_compound value
  PS.data.compounds <<- PS.data.compounds %>% 
    group_by(Compound_new_name) %>% 
    fill(cumul_years_compound)
  
  # define constant viridis colours and add to PS.data
  cols_compound<<- tibble(Compound_new_name = unique(PS.data.compounds$Compound_new_name), 
                          col_compound = viridis(n=length(unique(PS.data.compounds$Compound_new_name))))
  PS.data.compounds <<- PS.data.compounds %>% 
    left_join(cols_compound, by = "Compound_new_name")
  
  
  # Map ---------------------------------------------------------------------
  PS.data.map <<- PS.data
  PS.data.map['ISO3'] <<- iso_codes$ISO3[match(PS.data.map$Country, iso_codes$Country)]
  PS.data.map <<- PS.data.map %>% 
    filter(Country != "Unknown")
  
  PS.data.map.reduced <<- PS.data.map %>%
    select(Author, Location, Country, ISO3) %>% 
    group_by(Country) %>% 
    summarise(n = n(),
              Publications = paste(Author, collapse = "\n")) %>% 
    left_join(iso_codes %>% 
                select(Country, ISO3)) %>% 
    mutate(log_n = log(n))
  
  PS.data.map.reduced$Publications[PS.data.map.reduced$Country == "United States of America"] <<- 
    paste(PS.data.map$Author[PS.data.map$Country == "United States of America"], collapse = ";")
  
  
  
  progress$set(value = 0.85, message = 'Loading...')
  
  # get Further Reading data
  further_reading <<- read_sheet(sheet_id, sheet = "Further reading")
  
  progress$set(value = 1, message = 'Loading...')
  progress$close()
}



# User interface ----
ui<<- navbarPage(windowTitle = "PsyChild. Tracking clinical psychedelics in minors.",
                 tags$script(HTML("var header = $('.navbar > .container-fluid');
  header.append('<div style=\"float:left\"><img src=\"images/logo_header.svg\" alt=\"alt\" style=\"float:left;height:50px;padding-top:1px;\"></div>');
      console.log(header)")),
                 
                 tags$head(tags$link(rel = "icon", type = "image/svg", sizes = "16x16", href = "images/logo_header.svg")),
                 
                 tabPanel(span("Home", style="color:#1e9273ff"),
                          helpText(
                            h3("PsyChild. Tracking clinical psychedelics in minors."),
                            tags$hr(style="border-color: #28BF97;"),
                            p(),
                            p("PsyChild is a database of clinical research with psychedelics and related compounds in minors. 
                              Its main aim is to provide a growing bibliography on this multidisciplinary field for researchers, 
                              research subjects, patients, guardians, clinicians, critics, and external experts. Some of the provided 
                              records contain accounts of violence, homophobia, and unethical conduct, underscoring the urgent need to 
                              grapple with the difficult history of this field. Due to the cross-pollination of different research 
                              strands in this multidisciplinary field, PsyChild not only includes “classic” psychedelics but also 
                              related compounds such as entactogens like mCPP and αET, deliriants like atropine and scopolamine, 
                              and dissociatives like ketamine and PCP. While PsyChild takes a neutral stance on the question whether 
                              psychedelic-assisted psychotherapy (PAP) should be provided to minors, we do call for evidence-based, 
                              harm-reduction-oriented PAP protocols designed for minors in case research or treatment should be 
                              carried out. Inclusion in the database does not equal endorsement. PsyChild is committed to an 
                              open science approach and welcomes suggestions and submissions."),
                            HTML('<img src="https://live.staticflickr.com/65535/52865944604_18ee6790c7_o.jpg" width="30%">'),
                            p(),
                            p("On mobile devices, we recommend using the Desktop site instead of the default mobile version for 
                              better viewing experience."),
                            p(),
                            HTML("<a href='https://twitter.com/ChewingGinger'  target='_blank'>Philipp Rühr</a> 
               is responsible for curating new data for PsyChild, while this webpage is written and maintened by 
               <a href='https://twitter.com/Peter_Th_R'  target='_blank'>Peter T. Rühr</a>. Issues can be reported at PsyChild's <a href='https://github.com/Peter-T-Ruehr/PsyChild/issues'  target='_blank'>GitHub page</a>.<br><br>"),
                            HTML("Please cite this website as:<br>
                                     <em>PsyChild. Tracking Clinical Psychedelics in Minors</em> (<strong>2023)</strong>. Retrieved &lt;yyyy&#92;mm&#92;dd&gt; from http://ruehr.org/PsyChild. doi: <a href='https://zenodo.org/doi/10.5281/zenodo.10020023'  target='_blank'>10.5281/zenodo.10020023</a>.")
                          )
                 ),
                 
                 # PsyChild data -----------------------------------------------------------------
                 tabPanel(span("Data", style="color:#1e9273ff"),
                          HTML("<strong>The data tables of PsyChild.</strong>"),
                          p("Download buttons are provided below (clipboard, csv, or Excel)."),
                          p("Mobile visibility of PsyChild on cell phones is improved when you enable the 'Desktop version' manually on your phone browser."),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Published", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_PsyChild_Completed"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Archival", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_PsyChild_Archival"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Publication pending", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_PsyChild_Pending"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Discontinued/Uncertain", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_PsyChild_Discontinued"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Current/Ongoing", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_PsyChild_Ongoing"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          h6(HTML(paste0("*", tags$sup("1")," For multicenter-studies, only the main site is listed."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("2")," Due to the large number of available studies with ketamine and ketofol, 
                           references to the use of these compounds have only been included if they 
                           either address symptoms of pediatric anesthesia emergence delirium (PAED), 
                           or if they have been conducted in a psychiatric context."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("3")," Studies with cannabinoids have only been included if the ratio of 
                           psychoactive cannabinoids vs. non psychoactive cannabinoids has been  higher than 1:20."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("4")," As Treatment-Resistant Depression (TRD) is not clearly defined, 
                                         it is labeled as MDD in the indication columns.")))
                 ),
                 
                 
                 # Classes -----------------------------------------------------------------
                 tabPanel(span("Substance Classes", style="color:#1e9273ff"),
                          
                          # mainPanel(
                          # helpText(h3("Tracking clinical psychedelics in minors.")), # "Visualize psychedelic drug use in children through time and space"
                          verbatimTextOutput("class_selected"),
                          plotlyOutput("studies_over_year_plot_Class"), # plotOutput
                          # plotOutput("test_plot"),
                          sliderInput("range",
                                      label = "Years of interest:",
                                      min = 1839, # min(PS.data$Date)
                                      max = 2023, # max(PS.data$Date)
                                      # value = c(min(PS.data$Date), 
                                      #           max(PS.data$Date)),
                                      value = c(1839, 
                                                2023),
                                      step = 1,
                                      sep = ''),
                          
                          checkboxGroupInput("Class",
                                             # h3("Class"),
                                             label = "Choose one or more substance class(es) to display",
                                             choices = list("all",
                                                            "Psychedelics",
                                                            "Entactogens",
                                                            "Dissociatives",
                                                            "Deliriants",
                                                            "Harmala alkaloids",
                                                            "Phytocannabinoids",
                                                            "Synthetic cannabinoids"),
                                             selected = "Psychedelics"), # all
                          
                          HTML("<strong>The underlying data of the above plot.</strong>"),
                          p("Download buttons are provided below (clipboard, csv, or Excel)."),
                          
                          HTML("<br/><br/>"),
                          tags$hr(style="border-color: #28BF97;"),
                          
                          tags$h4("Published", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Class_Completed"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Archival", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Class_Archival"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Publication pending", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Class_Pending"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Discontinued/Uncertain", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Class_Discontinued"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Current/Ongoing", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Class_Ongoing"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          h6(HTML(paste0("*", tags$sup("1")," For multicenter-studies, only the main site is listed."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("2")," Due to the large number of available studies with ketamine and ketofol, 
                           references to the use of these compounds have only been included if they 
                           either address symptoms of pediatric anesthesia emergence delirium (PAED), 
                           or if they have been conducted in a psychiatric context."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("3")," Studies with cannabinoids have only been included if the ratio of 
                           psychoactive cannabinoids vs. non psychoactive cannabinoids has been  higher than 1:20."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("4")," As Treatment-Resistant Depression (TRD) is not clearly defined, 
                                         it is labeled as MDD in the indication columns.")))
                 ),
                 
                 # Compounds --------------------------------------------------------------
                 tabPanel(span("Compounds", style="color:#1e9273ff"),
                          
                          # mainPanel(
                          verbatimTextOutput("compound_selected"),
                          plotlyOutput("studies_over_year_plot_Compound"), # plotOutput
                          
                          sliderInput("range_compounds",
                                      label = "Years of interest:",
                                      min = 1839, # min(PS.data$Date)
                                      max = 2023, # max(PS.data$Date)
                                      # value = c(min(PS.data$Date), 
                                      #           max(PS.data$Date)),
                                      value = c(1839, 
                                                2023),
                                      step = 1,
                                      sep = ''),
                          checkboxGroupInput("All",
                                             label = "Choose one or more compound(s) to display",
                                             choices = c("all"),
                                             selected = NULL),
                          checkboxGroupInput("Psychedelics",
                                             label = "Psychedelics",
                                             choices = c("LAE-32 (D-Lysergic acid ethylamide)",
                                                         "LSD (Lysergic acid diethylamide)",
                                                         "Mescaline",
                                                         "Methysergide",
                                                         "Psilocybin"),
                                             selected = c("LAE-32 (D-Lysergic acid ethylamide)",
                                                          "LSD (Lysergic acid diethylamide)",
                                                          "Mescaline",
                                                          "Methysergide",
                                                          "Psilocybin")),
                          checkboxGroupInput("Entactogens",
                                             label = "Entactogens",
                                             choices = c("Iofetamine",
                                                         "mCPP (meta-Chlorophenylpiperazine)",
                                                         "αET (alpha-Ethyltryptamine)"),
                                             selected = NULL),
                          checkboxGroupInput("Dissociatives",
                                             label = "Dissociatives",
                                             choices = c("Esketamine",
                                                         "Ketamine",
                                                         "Ketodex",
                                                         "Ketofol",
                                                         "PCP (Phencyclidine)"),
                                             selected = NULL),
                          checkboxGroupInput("Deliriants",
                                             label = "Deliriants",
                                             choices = c("Atropine",
                                                         "Homatropine",
                                                         "Physostigmine",
                                                         "Scopolamine"),
                                             selected = NULL),
                          checkboxGroupInput("Harmala_alkaloids",
                                             label = "Harmala alkaloids",
                                             choices = c("Harmaline",
                                                         "Harmine"),
                                             selected = NULL),
                          checkboxGroupInput("Phytocannabinoids",
                                             label = "Phytocannabinoids",
                                             choices = c("2-AG (2-Arachidonoylglycerol)",
                                                         "AA (Arachidonic acid)",
                                                         "AEA (Anandamide)",
                                                         "OEA (Oleoylethanolamide)",
                                                         "PEA (Palmitoylethanolamide)",
                                                         "THC (Delta-8-THC)",
                                                         "THC (Delta-9 THC)",
                                                         "THC (THC-homologs, Numbers 122 and 125A)"),
                                             selected = NULL),
                          checkboxGroupInput("Synthetic_cannabinoids",
                                             label = "Synthetic cannabinoids",
                                             choices = c("Dexanabinol",
                                                         "Dronabinol",
                                                         "Levonantradol",
                                                         "Nabilone",
                                                         "Nabiximols"),
                                             selected = NULL),
                          checkboxGroupInput("Cannabinoid_receptor_agonists",
                                             label = "Cannabinoid receptor agonists",
                                             choices = c("Lenabasum"),
                                             selected = NULL),
                          
                          
                          
                          HTML("<strong>The underlying data of the above plot.</strong>"),
                          p("Download buttons are provided below (clipboard, csv, or Excel)."),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Published", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Compound_Completed"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Archival", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Compound_Archival"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Publication pending", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Compound_Pending"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Discontinued/Uncertain", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Compound_Discontinued"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          tags$hr(style="border-color: #28BF97;"),
                          tags$h4("Current/Ongoing", style="color: #28BF97;"),
                          div(dataTableOutput("table_print_Compound_Ongoing"), style = "font-size:80%"),
                          HTML("<br/><br/>"),
                          
                          
                          tags$hr(style="border-color: #28BF97;"),
                          h6(HTML(paste0("*", tags$sup("1")," For multicenter-studies, only the main site is listed."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("2")," Due to the large number of available studies with ketamine and ketofol, 
                           references to the use of these compounds have only been included if they 
                           either address symptoms of pediatric anesthesia emergence delirium (PAED), 
                           or if they have been conducted in a psychiatric context."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("3")," Studies with cannabinoids have only been included if the ratio of 
                           psychoactive cannabinoids vs. non psychoactive cannabinoids has been  higher than 1:20."))),
                          p(),
                          h6(HTML(paste0("*", tags$sup("4")," As Treatment-Resistant Depression (TRD) is not clearly defined, 
                                         it is labeled as MDD in the indication columns.")))
                 ),
                 
                 tabPanel(span("Publication Map", style="color:#1e9273ff"),
                          plotlyOutput("map_plot")),
                 
                 tabPanel(span("Further Reading", style="color:#1e9273ff"),
                          div(dataTableOutput("table_print_further_reading"), style = "font-size:80%")),
                 
                 tabPanel(span("Imprint/Contact", style="color:#1e9273ff"),
                          helpText(
                            p(),
                            h4("Site Notice"),
                            HTML("<small>Information provided according to Sec. 5 German Telemedia Act (TMG)<br>
Philipp Rühr<br>
Schlesische Straße 5<br>
10997 Berlin</small>"),
                            
                            h4("Contact"),
                            HTML("<small>Telephone: +49 163 843 4522<br>
Email: philippruehr@gmail.com</small>"),
                            
                            h4("Preferred mention for citations: "),
                            HTML("<small><em>PsyChild. Tracking Clinical Psychedelics in Minors</em> (<strong>2023)</strong>. Retrieved &lt;yyyy&#92;mm&#92;dd&gt; from http://ruehr.org/PsyChild. doi: <a href='https://zenodo.org/doi/10.5281/zenodo.10020023'  target='_blank'>10.5281/zenodo.10020023</a>.</small>"),
                            
                            h4("Dispute Resolution"),
                            HTML("<small>The European Commission provides a platform for online dispute resolution (OS): https://ec.europa.eu/consumers/odr. Please find our email in the impressum/legal notice.
We do not take part in online dispute resolutions at consumer arbitration boards.</small>"),
                            
                            h4("Liability for Contents"),
                            HTML("<small>As service providers, we are liable for own contents of these websites according to Sec. 7, paragraph 1 German Telemedia Act (TMG). However, according to Sec. 8 to 10 German Telemedia Act (TMG), service providers are not obligated to permanently monitor submitted or stored information or to search for evidences that indicate illegal activities.<br>
Legal obligations to removing information or to blocking the use of information remain unchallenged. In this case, liability is only possible at the time of knowledge about a specific violation of law. Illegal contents will be removed immediately at the time we get knowledge of them.</small>"),
                            
                            h4("Liability for Links"),
                            HTML("<small>Our offer includes links to external third party websites. We have no influence on the contents of those websites, therefore we cannot guarantee for those contents. Providers or administrators of linked websites are always responsible for their own contents.<br>
The linked websites had been checked for possible violations of law at the time of the establishment of the link. Illegal contents were not detected at the time of the linking. A permanent monitoring of the contents of linked websites cannot be imposed without reasonable indications that there has been a violation of law. Illegal links will be removed immediately at the time we get knowledge of them.</small>"),
                            
                            h4("Copyright"),
                            HTML("<small>Contents and compilations published on these websites by the providers are subject to German copyright laws. Reproduction, editing, distribution as well as the use of any kind outside the scope of the copyright law require a written permission of the author or originator. Downloads and copies of these websites are permitted for private use only. The commercial use of our contents without permission of the originator is prohibited.<br>
Copyright laws of third parties are respected as long as the contents on these websites do not originate from the provider. Contributions of third parties on this site are indicated as such. However, if you notice any violations of copyright law, please inform us. Such contents will be removed immediately.</small>")
                            
                            
                            # HTML("Please use the tabs above to access PsyChild's functionalities.<br><br>"),
                            # HTML("<a href='https://twitter.com/ChewingGinger'  target='_blank'>Philipp Rühr</a> 
                            #   is responsible for curating new data for PsyChild, while this webpage is written and maintened by 
                            #   <a href='https://twitter.com/Peter_Th_R'  target='_blank'>Peter T. Rühr</a>. Issues can be reported at <a href='https://github.com/Peter-T-Ruehr/PsyChild/issues'  target='_blank'>PsyChild's GitHub page</a>."),
                            # HTML("If you use this website, please cite it as:<br>
                            #                         Rühr, P. & Rühr, P. (<b>2023</b>): <em>PsyChild</em>, accessed yyyy&#92;mm&#92;dd, &lt;http://ruehr.org/PsyChild&gt;.<br><br>"),
                            # HTML('<center><img src="https://live.staticflickr.com/65535/52838364217_569cc496f3_o.jpg" width="35%"></center>')
                          )
                 )
)

# Server logic ------------------------------------------------------------
server <-  function(input, output, session) {
  if(is.null(PS.data)){
    readData(session)
  }
  # all PsyChild data -------------------------------------------------------
  # Published - Completed
  output$table_print_PsyChild_Completed <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  data_Completed <-  PS.data.print_Class %>%
    # filter(Author %in% Authors_before) %>%
    arrange(Date, `Substance class`) %>%
    distinct(Title, .keep_all = TRUE) %>%
    drop_na(Title) %>%
    filter(`Status` == "Published") %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>%
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  
  data_Completed},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Current/Ongoing - Ongoing
  output$table_print_PsyChild_Ongoing <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  data_Ongoing <-  PS.data.print_Class %>%
    # filter(Author %in% Authors_before) %>%
    arrange(Date, `Substance class`) %>%
    distinct(Title, .keep_all = TRUE) %>%
    drop_na(Title) %>%
    filter(`Status` == "Current/Ongoing") %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>%
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  data_Ongoing},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Discontinued/Uncertain - Discontinued
  output$table_print_PsyChild_Discontinued <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  data_Discontinued <-  PS.data.print_Class %>%
    # filter(Author %in% Authors_before) %>%
    arrange(Date, `Substance class`) %>%
    distinct(Title, .keep_all = TRUE) %>%
    drop_na(Title) %>%
    filter(is.na(`Status`) |
             `Status` %in% c("Discontinued/Uncertain")) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>%
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  data_Discontinued},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Publication pending - Pending
  output$table_print_PsyChild_Pending <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  data_Pending <-  PS.data.print_Class %>%
    # filter(Author %in% Authors_before) %>%
    arrange(Date, `Substance class`) %>%
    distinct(Title, .keep_all = TRUE) %>%
    drop_na(Title) %>%
    filter(is.na(`Status`) |
             `Status` %in% c("Publication pending")) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>%
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  data_Pending},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Archival - Archival
  output$table_print_PsyChild_Archival <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  data_Archival <-  PS.data.print_Class %>%
    # filter(Author %in% Authors_before) %>%
    arrange(Date, `Substance class`) %>%
    distinct(Title, .keep_all = TRUE) %>%
    drop_na(Title) %>%
    filter(is.na(`Status`) |
             `Status` %in% c("Archival")) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>%
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  data_Archival},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  
  # # Before
  # output$table_print_PsyChild_reports <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  # PS.data.print_PsyChild_before <-  PS.data.print_Class %>%
  #   filter(Author %in% Authors_before) %>%
  #   arrange(Date, `Substance class`) %>%
  #   distinct(Title, .keep_all = TRUE) %>%
  #   drop_na(Title) %>%
  #   select(c(# `Date`,
  #     `Author`,
  #     `Location`,
  #     # `Location photo`,
  #     `Title`,
  #     `Type`,
  #     `Compound(s)`,
  #     `Substance class`,
  #     `ICD-11 Indication or field of application`,
  #     `ICD-11 Indication as Groups or field of application`,
  #     # `Psychiatric indication?`,
  #     `Adjunct psychotherapy`,
  #     # `Adjacent psychotherapy?`,
  #     `Subjects`,
  #     # `Only minors?`,
  #     `Main psychiatric outcomes`,
  #     # `Reported side effects/adverse events`,
  #     `Side effects (MedDRA)`,
  #     `Consent`,
  #     `in/out patient`,
  #     # `Route of administration`,
  #     `Regimen (route of administration, dose, frequency)`,
  #     `Concomitant Medications`,
  #     `Comment`#,
  #     # `comment 1`,
  #     # `comment 2`,
  #     # `Compound_new_name,
  #     # Country
  #   )) %>%
  #   rename(`Location (*1)` = Location,
  #          `Compound (*2, *3)` = `Compound(s)`)
  # 
  # PS.data.print_PsyChild_before},
  # 
  # extensions = 'Buttons',
  # 
  # options = list(pageLength = 1000,
  #                searching = FALSE,
  #                lengthChange = FALSE,
  #                dom = 'tB',
  #                autoWidth = TRUE,
  #                buttons = c('copy', 'csv', 'excel')
  # ))
  # 
  # # After
  # output$table_print_PsyChild_after <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  # PS.data.print_PsyChild_after <-  PS.data.print_Class %>% 
  #   filter(Author %in% Authors_after) %>% 
  #   arrange(Date, `Substance class`) %>% 
  #   distinct(Title, .keep_all = TRUE) %>% 
  #   drop_na(Title) %>% 
  #   select(c(# `Date`,
  #     `Author`,
  #     `Location`,
  #     # `Location photo`, 
  #     `Title`,
  #     `Type`,
  #     `Compound(s)`,
  #     `Substance class`,
  #     `ICD-11 Indication or field of application`,
  #     `ICD-11 Indication as Groups or field of application`,
  #     # `Psychiatric indication?`,
  #     `Adjunct psychotherapy`,
  #     # `Adjacent psychotherapy?`,
  #     `Subjects`,
  #     # `Only minors?`,
  #     `Main psychiatric outcomes`,
  #     # `Reported side effects/adverse events`,
  #     `Side effects (MedDRA)`,
  #     `Consent`,
  #     `in/out patient`,
  #     # `Route of administration`,
  #     `Regimen (route of administration, dose, frequency)`,
  #     `Concomitant Medications`,
  #     `Comment`#,
  #     # `comment 1`,
  #     # `comment 2`,
  #     # `Compound_new_name,
  #     # Country
  #   )) %>% 
  #   rename(`Location (*1)` = Location,
  #          `Compound (*2, *3)` = `Compound(s)`)
  # 
  # PS.data.print_PsyChild_after},
  # 
  # extensions = 'Buttons',
  # 
  # options = list(pageLength = 1000,
  #                searching = FALSE,
  #                lengthChange = FALSE,
  #                dom = 'tB',
  #                autoWidth = TRUE,
  #                buttons = c('copy', 'csv', 'excel')
  # ))
  
  # Class -------------------------------------------------------------------  
  
  output$class_selected <-renderText({
    paste0("Classes selected: ", paste(input$Class, collapse = ", "), ".")
  })
  
  output$studies_over_year_plot_Class <-  renderPlotly({ 
    
    # filter by input range
    PS.data.plot.Class  <-  PS.data %>%
      filter(Date >= input$range[1],
             Date <= input$range[2])
    
    # select input classes
    if("all" %in% input$Class == FALSE){
      if(length(input$Class) > 1){
        PS.data.plot.Class  <-  PS.data.plot.Class %>%
          filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
      } else if(length(input$Class) == 1){
        PS.data.plot.Class  <-  PS.data.plot.Class %>%
          filter(`Substance class` %in% unlist(input$Class))
      } else if(length(input$Class) == 0){
        PS.data.plot.Class  <-  PS.data.plot.Class %>%
          filter(`Substance class` %in% "Nothing selected!")
      }
    } else {
      PS.data.plot.Class <-  PS.data.plot.Class
    }
    
    globalcolors <-  PS.data.plot.Class$col_class
    opacity <-  0.75
    fig_classes <-  plot_ly(data = PS.data.plot.Class, x=~Date, y=~cumul_years_class,
                            type="scatter",
                            color=~`Substance class`, 
                            mode="lines", 
                            colors = globalcolors, 
                            opacity=opacity, 
                            line = list(width=4))
    
    fig_classes %>% layout(legend = list(orientation = 'h', y=-0.25),
                           xaxis = list(
                             dtick = 10,
                             # tick0 = 10, 
                             tickmode = "linear"),
                           yaxis = list(
                             title = 'Cumulative Publications',
                             range = list(0, max(PS.data$cumul_years_class))))
  })
  
  
  # # class print outputs
  # output$table_print_Class <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  # 
  # PS.data.print_Class <-  PS.data.print_Class %>% 
  #   arrange(Date) %>% 
  #   filter(Date >= input$range[1],
  #          Date <= input$range[2])
  # 
  # # select input classes
  # if("all" %in% input$Class == FALSE){
  #   if(length(input$Class) > 1){
  #     PS.data.print_Class <-  PS.data.print_Class %>%
  #       filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
  #   } else if(length(input$Class) == 1){
  #     PS.data.print_Class <-  PS.data.print_Class %>%
  #       filter(`Substance class` %in% unlist(input$Class))
  #   } else if(length(input$Class) == 0){
  #     PS.data.print_Class <-  PS.data.print_Class %>%
  #       filter(`Substance class` %in% "Nothing selected!")
  #   }
  # } else {
  #   PS.data.print_Class <-  PS.data.print_Class
  # }
  # 
  # PS.data.print_Class <-  PS.data.print_Class %>% 
  #   arrange(Date, `Substance class`) %>% 
  #   distinct(Title, .keep_all = TRUE) %>% 
  #   drop_na(Title) %>% 
  #   select(c(# `Date`,
  #     `Author`,
  #     `Location`,
  #     # `Location photo`,
  #     `Title`,
  #     `Type`,
  #     `Compound(s)`,
  #     `Substance class`,
  #     `ICD-11 Indication or field of application`,
  #     `ICD-11 Indication as Groups or field of application`,
  #     # `Psychiatric indication?`,
  #     `Adjunct psychotherapy`,
  #     # `Adjacent psychotherapy?`,
  #     `Subjects`,
  #     # `Only minors?`,
  #     `Main psychiatric outcomes`,
  #     # `Reported side effects/adverse events`,
  #     `Side effects (MedDRA)`,
  #     `Consent`,
  #     `in/out patient`,
  #     # `Route of administration`,
  #     `Regimen (route of administration, dose, frequency)`,
  #     `Concomitant Medications`,
  #     `Comment`#,
  #     # `comment 1`,
  #     # `comment 2`,
  #     # `Compound_new_name,
  #     # Country
  #     
  #   )) %>% 
  #   rename(`Location (*1)` = Location,
  #          `Compound (*2, *3)` = `Compound(s)`,
  #          `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
  #          `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  # 
  # PS.data.print_Class},
  # 
  # extensions = 'Buttons',
  # 
  # options = list(pageLength = 1000,
  #                searching = FALSE,
  #                lengthChange = FALSE,
  #                dom = 'tB',
  #                autoWidth = TRUE,
  #                buttons = c('copy', 'csv', 'excel')
  # ))
  
  # class print outputs: Completed
  output$table_print_Class_Completed <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2]) %>%
    filter(`Status` == "Published")
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    if(length(input$Class) > 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
    } else if(length(input$Class) == 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(input$Class))
    } else if(length(input$Class) == 0){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% "Nothing selected!")
    }
  } else {
    PS.data.print_Class <-  PS.data.print_Class
  }
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date, `Substance class`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
      
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  PS.data.print_Class},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  
  # class print outputs: Archival
  output$table_print_Class_Archival <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2]) %>%
    filter(`Status` == "Archival")
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    if(length(input$Class) > 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
    } else if(length(input$Class) == 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(input$Class))
    } else if(length(input$Class) == 0){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% "Nothing selected!")
    }
  } else {
    PS.data.print_Class <-  PS.data.print_Class
  }
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date, `Substance class`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
      
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  PS.data.print_Class},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # class print outputs: Pending
  output$table_print_Class_Pending <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2]) %>%
    filter(`Status` == "Pending")
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    if(length(input$Class) > 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
    } else if(length(input$Class) == 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(input$Class))
    } else if(length(input$Class) == 0){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% "Nothing selected!")
    }
  } else {
    PS.data.print_Class <-  PS.data.print_Class
  }
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date, `Substance class`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
      
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  PS.data.print_Class},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # class print outputs: Discontinued
  output$table_print_Class_Discontinued <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2]) %>%
    filter(`Status` == "Discontinued/Uncertain")
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    if(length(input$Class) > 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
    } else if(length(input$Class) == 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(input$Class))
    } else if(length(input$Class) == 0){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% "Nothing selected!")
    }
  } else {
    PS.data.print_Class <-  PS.data.print_Class
  }
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date, `Substance class`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
      
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  PS.data.print_Class},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # class print outputs: "Current/Ongoing"
  output$table_print_Class_Ongoing <-  renderDataTable({df <-  reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2]) %>% 
    filter(`Status` == "Current/Ongoing")
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    if(length(input$Class) > 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
    } else if(length(input$Class) == 1){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(input$Class))
    } else if(length(input$Class) == 0){
      PS.data.print_Class <-  PS.data.print_Class %>%
        filter(`Substance class` %in% "Nothing selected!")
    }
  } else {
    PS.data.print_Class <-  PS.data.print_Class
  }
  
  PS.data.print_Class <-  PS.data.print_Class %>% 
    arrange(Date, `Substance class`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
      
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  
  PS.data.print_Class},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Compound -------------------------------------------------------------------  
  
  Compounds <-  reactive({c(input$All,
                            input$Phytocannabinoids,
                            input$Synthetic_cannabinoids,
                            input$Cannabinoid_receptor_agonists,
                            input$Psychedelics,
                            input$Dissociatives,
                            input$Harmala_alkaloids,
                            input$Deliriants,
                            input$Entactogens)})
  
  output$compound_selected <-  renderText({
    paste0("Compounds selected: ", paste(Compounds(), collapse = ", "), ".")
  })
  
  
  output$studies_over_year_plot_Compound <-  renderPlotly({
    # filter by input range
    PS.data.compounds.plot <-  PS.data.compounds  %>% 
      ungroup() %>%
      filter(Date >= input$range_compounds[1],
             Date <= input$range_compounds[2])
    
    # select input compounds
    if("all" %in% Compounds() == FALSE){
      # i=2
      tmp <-  NULL
      i=1
      for(i in 1:length(Compounds())){
        tmp <-  rbind(tmp, PS.data.compounds.plot %>%
                        ungroup() %>%
                        filter(Compound_new_name %in% Compounds()[i]))
      }
      PS.data.compounds.plot <-  tmp
      rm(tmp)
    } else {
      PS.data.compounds.plot <-  PS.data.compounds.plot
    }
    
    globalcolors <-  PS.data.compounds.plot$col_compound
    opacity <-  0.75
    fig_compounds <-  plot_ly(data = PS.data.compounds.plot, x=~Date, y=~cumul_years_compound,
                              type="scatter",
                              color=~Compound_new_name, 
                              mode="lines", 
                              colors = globalcolors, 
                              opacity=opacity, 
                              line = list(width=4))
    fig_compounds %>% layout(legend = list(orientation = 'h', y=-0.25),
                             xaxis = list(
                               dtick = 10,
                               # tick0 = 10, 
                               tickmode = "linear"),
                             yaxis = list(
                               title = 'Cumulative Publications',
                               range = list(0, max(PS.data.compounds$cumul_years_compound))))
  })
  
  # output$table_print_Compound <-  renderDataTable({df <-  reactiveVal(PS.data.print_Compound)
  # PS.data.print_Compound  <-  PS.data.compounds %>%
  #   arrange(Date) %>%
  #   filter(Date >= input$range_compounds[1],
  #          Date <= input$range_compounds[2])
  # 
  # if("all" %in% Compounds() == FALSE){
  #   # i=2
  #   tmp <-  NULL
  #   for(i in 1:length(Compounds())){
  #     tmp <-  rbind(tmp, PS.data.print_Compound %>%
  #                     filter(Compound_new_name %in% Compounds()[i]))
  #   }
  #   PS.data.print_Compound  <-  tmp
  #   rm(tmp)
  # } else {
  #   PS.data.print_Compound  <-  PS.data.print_Compound
  # }
  # 
  # PS.data.print_Compound  <-  PS.data.print_Compound %>% 
  #   ungroup() %>% 
  #   arrange(Date, `Compound(s)`) %>% 
  #   distinct(Title, .keep_all = TRUE) %>% 
  #   drop_na(Title) %>% 
  #   select(c(# `Date`,
  #     `Author`,
  #     `Location`,
  #     # `Location photo`,
  #     `Title`,
  #     `Type`,
  #     `Compound(s)`,
  #     `Substance class`,
  #     `ICD-11 Indication or field of application`,
  #     `ICD-11 Indication as Groups or field of application`,
  #     # `Psychiatric indication?`,
  #     `Adjunct psychotherapy`,
  #     # `Adjacent psychotherapy?`,
  #     `Subjects`,
  #     # `Only minors?`,
  #     `Main psychiatric outcomes`,
  #     # `Reported side effects/adverse events`,
  #     `Side effects (MedDRA)`,
  #     `Consent`,
  #     `in/out patient`,
  #     # `Route of administration`,
  #     `Regimen (route of administration, dose, frequency)`,
  #     `Concomitant Medications`,
  #     `Comment`#,
  #     # `comment 1`,
  #     # `comment 2`,
  #     # `Compound_new_name,
  #     # Country
  #   )) %>% 
  #   rename(`Location (*1)` = Location,
  #          `Compound (*2, *3)` = `Compound(s)`,
  #          `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
  #          `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  # PS.data.print_Compound},
  # 
  # extensions = 'Buttons',
  # 
  # options = list(pageLength = 1000,
  #                searching = FALSE,
  #                lengthChange = FALSE,
  #                dom = 'tB',
  #                autoWidth = TRUE,
  #                buttons = c('copy', 'csv', 'excel')
  # ))
  
  # Published: Completed
  output$table_print_Compound_Completed <-  renderDataTable({df <-  reactiveVal(PS.data.print_Compound)
  PS.data.print_Compound  <-  PS.data.compounds %>%
    arrange(Date) %>%
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2]) %>% 
    filter(Status == "Published")
  
  if("all" %in% Compounds() == FALSE){
    # i=2
    tmp <-  NULL
    for(i in 1:length(Compounds())){
      tmp <-  rbind(tmp, PS.data.print_Compound %>%
                      filter(Compound_new_name %in% Compounds()[i]))
    }
    PS.data.print_Compound  <-  tmp
    rm(tmp)
  } else {
    PS.data.print_Compound  <-  PS.data.print_Compound
  }
  
  PS.data.print_Compound  <-  PS.data.print_Compound %>% 
    ungroup() %>% 
    arrange(Date, `Compound(s)`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  PS.data.print_Compound},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Archival: Archival
  output$table_print_Compound_Archival <-  renderDataTable({df <-  reactiveVal(PS.data.print_Compound)
  PS.data.print_Compound  <-  PS.data.compounds %>%
    arrange(Date) %>%
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2]) %>% 
    filter(Status == "Archival")
  
  if("all" %in% Compounds() == FALSE){
    # i=2
    tmp <-  NULL
    for(i in 1:length(Compounds())){
      tmp <-  rbind(tmp, PS.data.print_Compound %>%
                      filter(Compound_new_name %in% Compounds()[i]))
    }
    PS.data.print_Compound  <-  tmp
    rm(tmp)
  } else {
    PS.data.print_Compound  <-  PS.data.print_Compound
  }
  
  PS.data.print_Compound  <-  PS.data.print_Compound %>% 
    ungroup() %>% 
    arrange(Date, `Compound(s)`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  PS.data.print_Compound},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Pending: Pending
  output$table_print_Compound_Pending <-  renderDataTable({df <-  reactiveVal(PS.data.print_Compound)
  PS.data.print_Compound  <-  PS.data.compounds %>%
    arrange(Date) %>%
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2]) %>% 
    filter(Status == "Pending")
  
  if("all" %in% Compounds() == FALSE){
    # i=2
    tmp <-  NULL
    for(i in 1:length(Compounds())){
      tmp <-  rbind(tmp, PS.data.print_Compound %>%
                      filter(Compound_new_name %in% Compounds()[i]))
    }
    PS.data.print_Compound  <-  tmp
    rm(tmp)
  } else {
    PS.data.print_Compound  <-  PS.data.print_Compound
  }
  
  PS.data.print_Compound  <-  PS.data.print_Compound %>% 
    ungroup() %>% 
    arrange(Date, `Compound(s)`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  PS.data.print_Compound},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Discontinued: Discontinued/Uncertain
  output$table_print_Compound_Discontinued <-  renderDataTable({df <-  reactiveVal(PS.data.print_Compound)
  PS.data.print_Compound  <-  PS.data.compounds %>%
    arrange(Date) %>%
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2]) %>% 
    filter(Status == "Discontinued/Uncertain")
  
  if("all" %in% Compounds() == FALSE){
    # i=2
    tmp <-  NULL
    for(i in 1:length(Compounds())){
      tmp <-  rbind(tmp, PS.data.print_Compound %>%
                      filter(Compound_new_name %in% Compounds()[i]))
    }
    PS.data.print_Compound  <-  tmp
    rm(tmp)
  } else {
    PS.data.print_Compound  <-  PS.data.print_Compound
  }
  
  PS.data.print_Compound  <-  PS.data.print_Compound %>% 
    ungroup() %>% 
    arrange(Date, `Compound(s)`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  PS.data.print_Compound},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # Ongoing: Ongoing
  output$table_print_Compound_Ongoing <-  renderDataTable({df <-  reactiveVal(PS.data.print_Compound)
  PS.data.print_Compound  <-  PS.data.compounds %>%
    arrange(Date) %>%
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2]) %>% 
    filter(Status == "Ongoing")
  
  if("all" %in% Compounds() == FALSE){
    # i=2
    tmp <-  NULL
    for(i in 1:length(Compounds())){
      tmp <-  rbind(tmp, PS.data.print_Compound %>%
                      filter(Compound_new_name %in% Compounds()[i]))
    }
    PS.data.print_Compound  <-  tmp
    rm(tmp)
  } else {
    PS.data.print_Compound  <-  PS.data.print_Compound
  }
  
  PS.data.print_Compound  <-  PS.data.print_Compound %>% 
    ungroup() %>% 
    arrange(Date, `Compound(s)`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
    drop_na(Title) %>% 
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
      # `Only minors?`,
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    )) %>% 
    rename(`Location (*1)` = Location,
           `Compound (*2, *3)` = `Compound(s)`,
           `ICD-11 Indication or field of application (*4)` = `ICD-11 Indication or field of application`,
           `ICD-11 Indication as Groups or field of application (*4)` = `ICD-11 Indication as Groups or field of application`)
  PS.data.print_Compound},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')
  ))
  
  # MAP ---------------------------------------------------------------------
  output$map_plot <-  renderPlotly({ # renderPlot
    fig_map <-  plot_ly(PS.data.map.reduced, type='choropleth', 
                        locations=PS.data.map.reduced$ISO3, 
                        z=PS.data.map.reduced$log_n, 
                        colorscale="Viridis",
                        hovertemplate = paste0(PS.data.map.reduced$Country, ": ", PS.data.map.reduced$n, " publication(s)."))
    
    
    fig_map <- fig_map %>% 
      hide_colorbar() %>%
      layout(title = 'Map of Publications per country')
    
    fig_map_points <- add_trace(fig_map,
                                type='scattergeo',
                                x = PS.data.map$Lon,
                                y = PS.data.map$Lat,
                                showlegend = FALSE,
                                text = PS.data.map$Location,
                                hoverinfo = 'text',
                                mode = 'markers', 
                                inherit = FALSE,
                                marker = list(color='orange',
                                              size = 5) #size = ~numbEmployed, sizeref = 4000, sizemode = 'area')
    ) %>% 
      layout(hoverdistance = 1)
  })
  
  
  
  # FURTHER READING ---------------------------------------------------------
  # reduced_links <-  further_reading %>% 
  #   pull(Link) %>%
  #   gsub("https://", "", .)%>%
  #   gsub("http://", "", .)%>%
  #   gsub("www.", "", .)
  # 
  # output$table_print_further_reading <-  renderDataTable({df <-  reactiveVal(further_reading)
  # 
  # further_reading %>% 
  #   mutate(Link = paste0("<a href='", Link,"' target='_blank'>", reduced_links,"</a>"))},
  # 
  # extensions = 'Buttons',
  # 
  # options = list(pageLength = 1000,
  #                searching = FALSE,
  #                lengthChange = FALSE,
  #                dom = 'tB',
  #                autoWidth = TRUE,
  #                buttons = c('copy', 'csv', 'excel')
  # ))
  output$table_print_further_reading <-  renderDT({df <-  reactiveVal(further_reading)
  reduced_links <-  further_reading %>% 
    pull(Link) %>% 
    gsub("https://", "", .)%>% 
    gsub("http://", "", .)%>% 
    gsub("www.", "", .)
  datatable(further_reading %>% 
              mutate(Link = paste0("<a href='", Link,"' target='_blank'>", reduced_links,"</a>")),
            escape = FALSE,
            rownames = FALSE, extensions = 'Buttons', options = list(pageLength = 1000,
                                                                     searching = FALSE,
                                                                     lengthChange = FALSE,
                                                                     dom = 'tB',
                                                                     autoWidth = TRUE,
                                                                     buttons = c('copy', 'csv', 'excel'))) %>%
    formatStyle(1:5, 'vertical-align'='top') %>% 
    formatStyle(1:5, 'text-align' = 'left')
  })}

# Run the application 
shinyApp(ui = ui, server = server)