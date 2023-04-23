library(dplyr)
library(tidyr)
library(ggplot2)
# library(gsheet)
library(shiny)
library(viridisLite)
library(ggrepel)
library(googlesheets4)
gs4_deauth()
library(plotly)
library(rvest)
library(DT)

sheet_id <- "https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/"
PS.data <- read_sheet(sheet_id, sheet = "PsychChild")
# remove unnamed columns
PS.data <- PS.data %>% 
  select(-contains('...'))
# get PsyChild data
# PS.data <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/edit?usp=sharing')

# get iso codes
iso_codes <- read_sheet(sheet_id, sheet = "iso_codes")

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
# PS.data <- rename(PS.data, Class = 'Substance class')
# PS.data <- rename(PS.data, Compound = 'Compound(s)') # was before: 'Psychedelic Compound(s) in children/adolescents')
# PS.data <- rename(PS.data, Indication = 'Indication (for children/adolescents)/Field of Application')
# PS.data <- rename(PS.data, Indication_ICD11 = 'Indication (Current terminology according to ICD-11)')
# PS.data <- rename(PS.data, Psychotherapy = 'Adjacent psychotherapy?')
# PS.data <- rename(PS.data, Psychiatric_indication = 'Psychiatric indication?')

# remove Date == NA columns
PS.data <- PS.data %>% 
  filter(!is.na(Date))

# create old new names for Compounds list
compound_translation <- tibble(old = c("2-AG \\(2-Arachidonoylglycerol\\)",
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
                               # new = c("AG",
                               #         "AA",
                               #         "AEA",
                               #         "LAE",
                               #         "LSD",
                               #         "OEA",
                               #         "PCP",
                               #         "mCPP",
                               #         "PEA",
                               #         "THC",
                               #         "THC",
                               #         "THC",
                               #         "αET"))
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
PS.data <- PS.data %>% 
  arrange(Date, Author, `Substance class`)

# get unique classes
classes <- sort(unique(unlist(strsplit(PS.data$`Substance class`, split = "; "))))

# Class and compound: arrange by date and add cumulative columns
PS.data <- PS.data %>%
  arrange(Date) %>%
  mutate(one = 1) %>% # ,
  # cumul_years_all = cumsum(one)) %>%
  group_by(`Substance class`) %>%
  mutate(cumul_years_class = cumsum(one)) %>%
  ungroup()

# save for later processing
PS.data.compounds <- PS.data

# change input compounds and data compounds to workable strings
# unique(PS.data.compounds$`Compound(s)`)
PS.data.compounds$Compound_new_name <- PS.data.compounds$`Compound(s)`
for(i in 1:nrow(compound_translation)){
  PS.data.compounds$Compound_new_name <- gsub(compound_translation$old[i], compound_translation$new[i], PS.data.compounds$Compound_new_name)
}
# unique(PS.data.compounds$Compound_new_name)

# get unique compounds
compounds <- sort(unique(unlist(strsplit(PS.data.compounds$Compound_new_name, split = "; "))))

# get countries
# PS.data$Location
# i=1
PS.data$Country <- NA
for(i in 1:nrow(PS.data)){
  curr_Location <- unlist(strsplit(PS.data$Location[i], split = "; "))
  PS.data$Country[i] <- curr_Location[length(curr_Location)]
}
PS.data$Country <- gsub("USA", "United States of America", PS.data$Country)
PS.data$Country <- gsub("^United States$", "United States of America", PS.data$Country)
PS.data$Country <- gsub("Czechoslovakia", "Czech Republic", PS.data$Country)
PS.data$Country <- gsub("England", "United Kingdom", PS.data$Country)
PS.data$Country <- gsub("Scotland", "United Kingdom", PS.data$Country)
PS.data$Country <- gsub("Iran", "Iran, Islamic Republic of", PS.data$Country)
PS.data$Country <- gsub("Russia", "Russian Federation", PS.data$Country)

# save for printing
PS.data.print_Class <- PS.data
PS.data.print_Compound <- PS.data %>% 
  ungroup()

# separate multiple compounds from each other
# i=1
# i=25
# i = 39 # Mescaline
# tmp <- PS.data.compounds
# PS.data.compounds <- tmp
rows_to_remove <- NULL
for(i in 1:nrow(PS.data.compounds)){
  # if compound cell contains ";"
  if(grepl(";", PS.data.compounds$Compound_new_name[i])){
    curr_compounds <- unlist(strsplit(PS.data.compounds$Compound_new_name[i], "; "))
    for(k in curr_compounds){
      PS.data.compounds <- PS.data.compounds %>% 
        add_row(PS.data.compounds[i, ])
      # replace original compounds with new compounds
      PS.data.compounds$Compound_new_name[nrow(PS.data.compounds)] <- k
    }
    rows_to_remove <- c(rows_to_remove, i)
  }
}
# remove original rows
PS.data.compounds <- PS.data.compounds[-rows_to_remove, ]

# cumulative years of compounds
PS.data.compounds <- PS.data.compounds %>%
  arrange(Date) %>%
  group_by(Compound_new_name) %>%
  mutate(cumul_years_compound = cumsum(one)) %>%
  ungroup() %>%
  select((-one))


# class -------------------------------------------------------------------
# add all missing years
for(i in classes){
  # i <- classes.selected[1]
  missing_years <- setdiff(min(PS.data$Date):max(PS.data$Date), PS.data$Date[PS.data$`Substance class` == i])
  PS.data <- PS.data %>%
    add_row(`Substance class` = i,
            Date = missing_years) %>% 
    arrange(`Substance class`, Date)
}

# add line at first year of current selection table
first_year <- min(PS.data$Date) # input$range[1]
for(i in classes){
  # i <- classes.selected[1]
  curr_author <- PS.data %>%
    filter(Date == first_year, `Substance class` == i) %>%
    pull(Author)
  if(is.na(curr_author)){
    PS.data$cumul_years_class[PS.data$`Substance class` == i & PS.data$Date == first_year] <- 0
  }
}

# fill empty years with previous cumul_years_class value
PS.data <- PS.data %>% 
  group_by(`Substance class`) %>% 
  fill(cumul_years_class)

# define constant viridis colours and add to PS.data
cols_class <- tibble(`Substance class` = unique(PS.data$`Substance class`), col_class = viridis(n=length(unique(PS.data$`Substance class`))))
PS.data <- PS.data %>% 
  left_join(cols_class, by = "Substance class")
# plot(1:nrow(cols), col = unique(PS.data$col), pch = 16, cex = 5)
# plot(1:nrow(cols), col = unique(PS.data.plot.Class$col), pch = 16, cex = 5)

# compound -------------------------------------------------------------------
# add all missing years
for(i in compounds){
  # i <- classes.selected[1]
  missing_years <- setdiff(min(PS.data.compounds$Date):max(PS.data.compounds$Date), PS.data.compounds$Date[PS.data.compounds$Compound_new_name == i])
  PS.data.compounds <- PS.data.compounds %>%
    add_row(Compound_new_name = i,
            Date = missing_years)
  # PS.data <- PS.data %>%
  #   arrange(`Substance class`, Date)
}

# add line at first year of current selection table
first_year <- min(PS.data.compounds$Date) # input$range[1]
# i = "2-AG"
for(i in compounds){
  # i <- classes.selected[1]
  curr_author <- PS.data.compounds %>%
    filter(Date == first_year, Compound_new_name == i) %>%
    pull(Author)
  if(is.na(curr_author)){
    PS.data.compounds$cumul_years_compound[PS.data.compounds$Compound_new_name == i & PS.data.compounds$Date == first_year] <- 0
  }
}

# arrange before filling
PS.data.compounds <- PS.data.compounds %>% 
  arrange(Compound_new_name, Date) 


# fill empty years with previous cumul_years_compound value
PS.data.compounds <- PS.data.compounds %>% 
  group_by(Compound_new_name) %>% 
  fill(cumul_years_compound)

# define constant viridis colours and add to PS.data
cols_compound <- tibble(Compound_new_name = unique(PS.data.compounds$Compound_new_name), 
                        col_compound = viridis(n=length(unique(PS.data.compounds$Compound_new_name))))
PS.data.compounds <- PS.data.compounds %>% 
  left_join(cols_compound, by = "Compound_new_name")
# plot(1:nrow(cols_compound), col = unique(PS.data.compounds$col_compound), pch = 16, cex = 5)


# Map ---------------------------------------------------------------------
# url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
# iso_codes <- url %>%
#   read_html() %>%
#   html_table() %>% 
#   bind_rows() %>% 
#   subset(nchar(as.character(X2)) > 1) %>% 
#   select(-X1)
# names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
# head(iso_codes)


PS.data.map <- PS.data
PS.data.map['ISO3'] <- iso_codes$ISO3[match(PS.data.map$Country, iso_codes$Country)]
PS.data.map <- PS.data.map %>% 
  filter(Country != "Unknown")

PS.data.map.reduced <- PS.data.map %>%
  select(Author, Location, Country, ISO3) %>% 
  group_by(Country) %>% 
  summarise(n = n(),
            Publications = paste(Author, collapse = "\n")) %>% 
  left_join(iso_codes %>% 
              select(Country, ISO3)) %>% 
  mutate(log_n = log(n))

PS.data.map.reduced$Publications[PS.data.map.reduced$Country == "United States of America"] <- 
  paste(PS.data.map$Author[PS.data.map$Country == "United States of America"], collapse = ";")

# Add directory of static resources to Shiny's web server
addResourcePath(prefix = "images", directoryPath = "./images/")

# get Further Reading data
further_reading <- read_sheet(sheet_id, sheet = "Further reading")


# User interface ----
ui <- navbarPage(windowTitle = "PsyChild - Tracking clinical psychedelics in children and adolescents.",
                 # title="dsa",#tags$img(src = "./images/logo.svg", width = "200px"),
                 # Introduction -----------------------------------------------------------------
                 # titlePanel(title = span(img(src = "images/logo_header.svg", height = "50px", padding = "1px,1px,1px, 1px"), "")),
                 # title=div(img(src="./images/logo.svg"), "My Title in the Navbar"),
                 # https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
                 tags$script(HTML("var header = $('.navbar > .container-fluid');
  header.append('<div style=\"float:left\"><img src=\"images/logo_header.svg\" alt=\"alt\" style=\"float:left;height:50px;padding-top:1px;\"></div>');
      console.log(header)")),
                 # tags$head(tags$link(rel = "shortcut icon", href = "images/logo_header.svg")),
                 
                 tags$head(tags$link(rel = "icon", type = "image/svg", sizes = "16x16", href = "images/logo_header.svg")),
                 
                 tabPanel(span("Home", style="color:#1e9273ff"),
                          helpText(
                            h3("PsyChild - Tracking clinical psychedelics in children and adolescents."),
                            # HTML('<center><img src="images/logo_header.svg" width="50%"></center>'),
                            # tags$img(src = "./images/logo_header.svg", width = "400px"),
                            p(),
                            # h4("Tracking clinical psychedelics in children and adolescents."),
                            p("PsyChild is a database for psychedelic research in minors. It’s main aim is to provide a growing 
                              bibliography on this multidisciplinary field for researchers, research subjects, patients, guardians, 
                              clinicians, and external experts. Some of the provided records contain accounts of violence, homophobia, 
                              and unethical conduct, underscoring the urgent need to grapple with the difficult history of this 
                              field. While PsyChild takes a neutral stance on the question whether psychedelic-assisted psychotherapy 
                              (PAP) should be provided to minors, we do call for evidence-based, harm-reduction-oriented PAP 
                              protocols designed for minors in case research or treatement should be carried out. PsyChild is 
                              committed to an open science approach and welcomes suggestions and submissions."),
                            HTML("Please use the tabs above to access PsyChild's functionalities.<br><br>"),
                            HTML('<img src="https://live.staticflickr.com/65535/52838364217_569cc496f3_o.jpg" width="50%">'),
                            p(),
                            HTML("<a href='https://twitter.com/ChewingGinger'  target='_blank'>Philipp Rühr</a> 
               is responsible for curating new data for PsyChild, while this webpage is written and maintened by 
               <a href='https://twitter.com/Peter_Th_R'  target='_blank'>Peter T. Rühr</a>. Issues can be reported at <a href='https://github.com/Peter-T-Ruehr/PsyChild/issues'  target='_blank'>PsyChild's GitHub page</a>.<br><br>"),
                            # HTML("If you use this website, please cite it as:<br>
                            #          Rühr, P. & Rühr, P. (<b>2023</b>): <em>PsyChild</em>, accessed yyyy&#92;mm&#92;dd, &lt;http://ruehr.org/shiny/PsyChild/&gt;.")
                            # ),
                            HTML("If you use this website, please cite it as:<br>
                                     <em>PsyChild. Tracking Clinical Psychedelics in Minors</em> (<strong>2023)</strong>. Retrieved &lt;yyyy&#92;mm&#92;dd&gt; from http://ruehr.org/shiny/PsyChild/.")
                          )# <center></center>
                 ),
                 # Classes -----------------------------------------------------------------
                 tabPanel(span("Substance Classes", style="color:#1e9273ff"),
                          # sidebarLayout(
                          #   sidebarPanel(
                          
                          # mainPanel(
                          # helpText(h3("Tracking clinical psychedelics in children and adolescents.")), # "Visualize psychedelic drug use in children through time and space"
                          verbatimTextOutput("class_selected"),
                          plotlyOutput("studies_over_year_plot_Class"), # plotOutput
                          # plotOutput("test_plot"),
                          sliderInput("range",
                                      label = "Years of interest:",
                                      min = min(PS.data$Date),
                                      max = max(PS.data$Date),
                                      value = c(min(PS.data$Date), # min(PS.data$Date), 1950,
                                                max(PS.data$Date)),
                                      step = 1,
                                      sep = ''),
                          checkboxGroupInput("Class",
                                             # h3("Class"),
                                             label = "Choose one or more substance class(es) to display",
                                             choices = list("all",
                                                            "Deliriants",
                                                            "Dissociatives",
                                                            "Entactogens",
                                                            "MAOIs",
                                                            "Phytocannabinoids",
                                                            "Psychedelics",
                                                            "Synthetic cannabinoids"),
                                             selected = "all"),
                          div(dataTableOutput("table_print_Class"), style = "font-size:100%")
                 ),
                 
                 # Compounds --------------------------------------------------------------
                 tabPanel(span("Compounds", style="color:#1e9273ff"),
                          # sidebarLayout(
                          #   sidebarPanel(
                          #     helpText(h3("Tracking clinical psychedelics in children and adolescents.")),
                          
                          # mainPanel(
                          verbatimTextOutput("compound_selected"),
                          plotlyOutput("studies_over_year_plot_Compound"), # plotOutput
                          
                          sliderInput("range_compounds",
                                      label = "Years of interest:",
                                      min = min(PS.data$Date),
                                      max = max(PS.data$Date),
                                      value = c(min(PS.data$Date), # c(min(PS.data$Date), 1950,
                                                max(PS.data$Date)),
                                      step = 1,
                                      sep = ''),
                          checkboxGroupInput("All",
                                             # h3("Compound"),
                                             label = "All Compounds",
                                             choices = c("all"),
                                             selected = "all"),
                          checkboxGroupInput("Endocannabinoids",
                                             # h3("Compound"),
                                             label = "Endocannabinoids",
                                             choices = c("2-AG (2-Arachidonoylglycerol)",
                                                         "AA (Arachidonic acid)",
                                                         "AEA (Anandamide)",
                                                         "OEA (Oleoylethanolamide)",
                                                         "PEA (Palmitoylethanolamide)"),
                                             # "2-AG (2-Arachidonylglycerol)",
                                             # "AA",
                                             # "AEA (Anandamid)",
                                             # "Delta-8-tetrahydrocannabinol (delta-8-THC)",
                                             # "Dexanabinol",
                                             # "Dronabinol",
                                             # "Esketamine",
                                             # "Harmaline",
                                             # "Harmine",
                                             # "Iofetamine",
                                             # "Ketamine",
                                             # "Ketodex",
                                             # "Ketofol",
                                             # "LAE-32 (D-Lysergic acid ethylamide)",
                                             # "Lenabasum",
                                             # "Levonantradol",
                                             # "LSD (Lysergic acid diethylamide)",
                                             # "Marinol",
                                             # "mCPP (meta-Chlorphenylpiperazin)",
                                             # "Mescaline",
                                             # "Methysergide",
                                             # "Nabilone",
                                             # "Nabiximols",
                                             # "OEA (Oleoylethanolamide)",
                                             # "PCP (Phencyclidin)",
                                             # "PEA (Palmitoylethanolamid)",
                                             # "Physostigmine",
                                             # "Phytocannabinoids",
                                             # "Psilocybin",
                                             # "Scopolamine",
                                             # "THC",
                                             # "αET (alpha-Ethyltryptamine)"
                                             selected = NULL),
                          checkboxGroupInput("Synthetic_cannabinoids",
                                             # h3("Compound"),
                                             label = "Synthetic cannabinoids",
                                             choices = c("Dexanabinol",
                                                         "Dronabinol",
                                                         "Levonantradol",
                                                         "Nabilone",
                                                         "Nabiximols"),
                                             selected = NULL),
                          checkboxGroupInput("Cannabinoid_receptor_agonist",
                                             # h3("Compound"),
                                             label = "Cannabinoid receptor agonist",
                                             choices = c("Lenabasum"),
                                             selected = NULL),
                          checkboxGroupInput("Psychedelics",
                                             # h3("Compound"),
                                             label = "Psychedelics",
                                             choices = c("LAE-32 (D-Lysergic acid ethylamide)",
                                                         "LSD (Lysergic acid diethylamide)",
                                                         "Mescaline",
                                                         "Methysergide",
                                                         "Psilocybin"),
                                             selected = NULL),
                          checkboxGroupInput("Dissociatives",
                                             # h3("Compound"),
                                             label = "Dissociatives",
                                             choices = c("Esketamine",
                                                         "Ketamine",
                                                         "Ketodex",
                                                         "Ketofol",
                                                         "PCP (Phencyclidine)"),
                                             selected = NULL),
                          checkboxGroupInput("Harmala_alkaloids",
                                             # h3("Compound"),
                                             label = "Harmala alkaloids",
                                             choices = c("Harmaline",
                                                         "Harmine"),
                                             selected = NULL),
                          checkboxGroupInput("Deliriants",
                                             # h3("Compound"),
                                             label = "Deliriants",
                                             choices = c("Physostigmine",
                                                         "Scopolamine"),
                                             selected = NULL),
                          checkboxGroupInput("Entactogens",
                                             # h3("Compound"),
                                             label = "Entactogens",
                                             choices = c("Iofetamine",
                                                         "mCPP (meta-Chlorophenylpiperazine)",
                                                         "αET (alpha-Ethyltryptamine)"),
                                             selected = NULL),
                          
                          div(dataTableOutput("table_print_Compound"), style = "font-size:80%")
                 ),
                 # )
                 # ))
                 tabPanel(span("Publication Map", style="color:#1e9273ff"),
                          plotlyOutput("map_plot")),
                 
                 tabPanel(span("Further Reading", style="color:#1e9273ff"),
                          div(dataTableOutput("table_print_further_reading"), style = "font-size:80%")),
                 
                 tabPanel(span("Imprint/Contact", style="color:#1e9273ff"),
                          helpText(
                            # HTML('<center><img src="images/logo_header.svg" width="50%"></center>'),
                            # tags$img(src = "./images/logo_header.svg", width = "400px"),
                            # p(),
                            p(),
                            h4("Site Notice"),
                            HTML("<small>Information provided according to Sec. 5 German Telemedia Act (TMG)<br>
Philipp Rühr<br>
Schlesische Straße 5<br>
10997 Berlin</small>"),
                            
                            h4("Contact"),
                            HTML("<small>Telephone: +49 163 843 4522<br>
Email: philippruehr@gmail.com</small>"),
                            
                            h4("Preferred mention for imprints "),
                            HTML("<small><em>PsyChild. Tracking Clinical Psychedelics in Minors</em> (<strong>2023)</strong>. Retrieved &lt;yyyy&#92;mm&#92;dd&gt; from http://ruehr.org/shiny/PsyChild/.</small>"),
                            
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
                            #                         Rühr, P. & Rühr, P. (<b>2023</b>): <em>PsyChild</em>, accessed yyyy&#92;mm&#92;dd, &lt;http://ruehr.org/shiny/PsyChild/&gt;.<br><br>"),
                            # HTML('<center><img src="https://live.staticflickr.com/65535/52838364217_569cc496f3_o.jpg" width="35%"></center>')
                          )
                 )
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
  
  output$studies_over_year_plot_Class <- renderPlotly({ # renderPlot
    
    # # testing
    # input=list(range = c(1839, 2023), # 1839 1950 2023 1980
    #            range_compounds = c(1839, 2023), # 1839 1950 2023 1980
    #            Class = c("Dissociatives")) # "Dissociatives" "all", "Entactogens"
    # output=NULL
    # output$compound_selected <- "LSD (Lysergic acid diethylamide)"
    
    # filter by input range
    PS.data.plot.Class <- PS.data %>%
      filter(Date >= input$range[1],
             Date <= input$range[2])
    
    # select input classes
    if("all" %in% input$Class == FALSE){
      if(length(input$Class) > 1){
        PS.data.plot.Class <- PS.data.plot.Class %>%
          filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
      } else if(length(input$Class) == 1){
        PS.data.plot.Class <- PS.data.plot.Class %>%
          filter(`Substance class` %in% unlist(input$Class))
      } else if(length(input$Class) == 0){
        PS.data.plot.Class <- PS.data.plot.Class %>%
          filter(`Substance class` %in% "Nothing selected!")
      }
    } else {
      PS.data.plot.Class <- PS.data.plot.Class
    }
    
    # if(nrow(PS.data.plot.Class) > 0){
    # globalcolors <- PS.data.plot.Class$col_class
    # } else {
    #   PS.data.plot.Class <- tibble()
    # }
    globalcolors <- PS.data.plot.Class$col_class
    opacity <- 0.75
    fig_classes <- plot_ly(data = PS.data.plot.Class, x=~Date, y=~cumul_years_class,
                           type="scatter",
                           color=~`Substance class`, 
                           mode="lines", 
                           colors = globalcolors, 
                           opacity=opacity, 
                           line = list(width=4)) # ,
    # height=800)
    fig_classes %>% layout(legend = list(orientation = 'h', y=-0.25),
                           xaxis = list(
                             dtick = 10,
                             # tick0 = 10, 
                             tickmode = "linear"),
                           yaxis = list(
                             range = list(0, max(PS.data$cumul_years_class))))
  })
  
  output$table_print_Class <- renderDataTable({df <- reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <- PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2])
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    if(length(input$Class) > 1){
      PS.data.print_Class <- PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
    } else if(length(input$Class) == 1){
      PS.data.print_Class <- PS.data.print_Class %>%
        filter(`Substance class` %in% unlist(input$Class))
    } else if(length(input$Class) == 0){
      PS.data.print_Class <- PS.data.print_Class %>%
        filter(`Substance class` %in% "Nothing selected!")
    }
  } else {
    PS.data.print_Class <- PS.data.print_Class
  }
  
  PS.data.print_Class <- PS.data.print_Class %>% 
    arrange(Date, `Substance class`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    ))
  
  PS.data.print_Class},
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE))
  
  # Compound -------------------------------------------------------------------  
  
  Compounds <- reactive({c(input$All,
                           input$Endocannabinoids,
                           input$Synthetic_cannabinoids,
                           input$Cannabinoid_receptor_agonist,
                           input$Psychedelics,
                           input$Dissociatives,
                           input$Harmala_alkaloids,
                           input$Deliriants,
                           input$Entactogens)})
  
  output$compound_selected <- renderText({
    paste0("Compounds selected: ", paste(Compounds(), collapse = ", "), ".")
  })
  
  
  output$studies_over_year_plot_Compound <- renderPlotly({ # renderPlot
    
    # # testing
    # input=list(range = c(1839, 2023), # 1839 1950 2023 1980
    #            range_compounds = c(1839, 2023), # 1839 1950 2023 1980
    #            Class = c("Dissociatives, Entactogens")) # "Dissociatives" "all"
    # output=NULL
    # output$compound_selected <- paste(c("LSD (Lysergic acid diethylamide)", "Mescaline"), collapse = ", ")
    
    # filter by input range
    PS.data.compounds.plot <- PS.data.compounds  %>% 
      ungroup() %>%
      filter(Date >= input$range_compounds[1],
             Date <= input$range_compounds[2])
    
    # select input compounds
    if("all" %in% Compounds() == FALSE){
      # i=2
      tmp <- NULL
      i=1
      for(i in 1:length(Compounds())){
        tmp <- rbind(tmp, PS.data.compounds.plot %>%
                       ungroup() %>%
                       # filter(`Compound(s)` %in% unlist(strsplit( Compounds(), split = ", ")))
                       filter(Compound_new_name %in% Compounds()[i])) # "Oleoylethanolamide"
      }
      PS.data.compounds.plot <- tmp
      rm(tmp)
    } else {
      PS.data.compounds.plot <- PS.data.compounds.plot
    }
    
    # tmp <- PS.data.compounds.plot %>% 
    #   drop_na(Title)
    
    globalcolors <- PS.data.compounds.plot$col_compound
    opacity <- 0.75
    fig_compounds <- plot_ly(data = PS.data.compounds.plot, x=~Date, y=~cumul_years_compound,
                             type="scatter",
                             color=~Compound_new_name, 
                             mode="lines", 
                             colors = globalcolors, 
                             opacity=opacity, 
                             line = list(width=4)) # ,
    # height=800)
    fig_compounds %>% layout(legend = list(orientation = 'h', y=-0.25),
                             xaxis = list(
                               dtick = 10,
                               # tick0 = 10, 
                               tickmode = "linear"),
                             yaxis = list(
                               range = list(0, max(PS.data.compounds$cumul_years_compound))))
  })
  
  output$table_print_Compound <- renderDataTable({df <- reactiveVal(PS.data.print_Compound)
  PS.data.print_Compound <- PS.data.compounds %>%
    arrange(Date) %>%
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2])
  
  if("all" %in% Compounds() == FALSE){
    # i=2
    tmp <- NULL
    for(i in 1:length(Compounds())){
      tmp <- rbind(tmp, PS.data.print_Compound %>%
                     # filter(Compound %in% unlist(strsplit(output$compound_selected, split = ", ")))
                     filter(Compound_new_name %in% Compounds()[i]))
    }
    PS.data.print_Compound <- tmp
    rm(tmp)
  } else {
    PS.data.print_Compound <- PS.data.print_Compound
  }
  
  PS.data.print_Compound <- PS.data.print_Compound %>% 
    ungroup() %>% 
    arrange(Date, `Compound(s)`) %>% 
    distinct(Title, .keep_all = TRUE) %>% 
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
      # `comment 2`,
      # `Compound_new_name,
      # Country
    ))
  PS.data.print_Compound},
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE))
  
  # MAP ---------------------------------------------------------------------
  output$map_plot <- renderPlotly({ # renderPlot
    fig_map <- plot_ly(PS.data.map.reduced, type='choropleth', 
                       locations=PS.data.map.reduced$ISO3, 
                       z=PS.data.map.reduced$log_n, 
                       # text=paste0(PS.data.map.reduced$Country, ":\n", PS.data.map.reduced$Publications), 
                       colorscale="Viridis",
                       hovertemplate = paste0(PS.data.map.reduced$Country, ":", PS.data.map.reduced$n, " Publications.")) # paste0(PS.data.map.reduced$Country, ":", PS.data.map.reduced$n, " Publications.\n", PS.data.map.reduced$Publications)
    
    fig_map %>% 
      hide_colorbar() %>%
      layout(title = 'Map of Publications per country')
  })
  
  
  
  # FURTHER READING ---------------------------------------------------------
  output$table_print_further_reading <- renderDT({df <- reactiveVal(further_reading)
  reduced_links <- further_reading %>% 
    pull(Link) %>% 
    gsub("https://", "", .)%>% 
    gsub("http://", "", .)%>% 
    gsub("www.", "", .)
  datatable(further_reading %>% 
              mutate(Link = paste0("<a href='", Link,"' target='_blank'>", reduced_links,"</a>")),
            escape = FALSE,
            rownames = FALSE) %>%
    formatStyle(1:5, 'vertical-align'='top') %>% 
    formatStyle(1:5, 'text-align' = 'left')
  })
}

# Run app ----
shinyApp(ui, server)



