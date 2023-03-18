library(shiny)
library(dplyr)
library(plotly)
library(googlesheets4)

sheet_id <- "https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/"
PS.data <- read_sheet(sheet_id, sheet = 1)
# remove unnamed columns
PS.data <- PS.data %>% 
  select(-contains('...'))

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

# # remove Date == NA columns - ot necessary anymore
# PS.data <- PS.data %>% 
#   filter(!is.na(Date))

# arrange PS_data
PS.data <- PS.data %>% 
  arrange(Date, Author, `Substance class`)

# get unique classes
classes <- sort(unique(unlist(strsplit(PS.data$`Substance class`, split = "; "))))

# get unique compounds
compounds <- sort(unique(unlist(strsplit(PS.data$`Compound(s)`, split = "; "))))

# Class and compound: arrange by date and add cumulative columns
PS.data <- PS.data %>%
  arrange(Date) %>%
  mutate(one = 1,
         cumul_years_all = cumsum(one)) %>%
  group_by(`Substance class`) %>%
  mutate(cumul_years_class = cumsum(one)) %>%
  ungroup() %>%
  group_by(`Compound(s)`) %>%
  mutate(cumul_years_compound = cumsum(one)) %>%
  ungroup() %>% 
  select(-one)

# save for printing
PS.data.print_Class <- PS.data
PS.data.print_Compound <- PS.data

# save for later processing
PS.data.compounds <- PS.data

# add all missing years
# Class
for(i in classes){
  # i <- classes.selected[1]
  missing_years <- setdiff(min(PS.data$Date):max(PS.data$Date), PS.data$Date[PS.data$`Substance class` == i])
  PS.data <- PS.data %>%
    add_row(`Substance class` = i,
            Date = missing_years) %>% 
    arrange(`Substance class`, Date)
}

# separate multiple compounds from each other
# i=1
# i=34
for(i in 1:nrow(PS.data.compounds)){
  # if compound cell contains ";"
  if(grepl(";", PS.data.compounds$`Compound(s)`[i])){
    curr_compounds <- unlist(strsplit(PS.data.compounds$Compound[i], "; "))
    for(k in curr_compounds){
      PS.data.compounds <- PS.data.compounds %>% 
        # add_row(Author = PS.data.compounds$Author[i],
        #         Date = PS.data.compounds$Date[i],
        #         `Substance class` = PS.data.compounds$`Substance class`[i],
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
  mutate(cumul_years_compound = cumsum(one)) %>%
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
    PS.data$cumul_years_class[PS.data$Class == i & PS.data$Date == first_year] <- 0
  }
}

# fill empty years with previous cumul_years_class value
PS.data <- PS.data %>% 
  group_by(Class) %>% 
  fill(cumul_years_class)

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
    PS.data.compounds$cumul_years_compound[PS.data.compounds$Compound == i & PS.data.compounds$Date == first_year] <- 0
  }
}

# arrange before filling
PS.data.compounds <- PS.data.compounds %>% 
  arrange(Compound, Date) 

# fill empty years with previous cumul_years_compound value
PS.data.compounds <- PS.data.compounds %>% 
  group_by(Compound) %>% 
  fill(cumul_years_compound)

# define constant viridis colours and add to PS.data
cols_compound <- tibble(Compound = unique(PS.data.compounds$Compound), col_compound = viridis(n=length(unique(PS.data.compounds$Compound))))
PS.data.compounds <- PS.data.compounds %>% 
  left_join(cols_compound, by = "Compound")
# plot(1:nrow(cols_compound), col = unique(PS.data.compounds$col_compound), pch = 16, cex = 5)
