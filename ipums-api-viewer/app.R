

# TO DO #####

## tabset
## single var: 
#### siberbar: paramaeters
#### main: boxplot/barplot over time
####      weighted freq tables
## two vars:
#### if both cat, chi sq
#### if both cont, cor

##

## required to install from github
if(!require(remotes)){
  install.packages("remotes")
}

#### Install/update ipumsr dev version with API support

if(!require(ipumsr) || packageVersion("ipumsr") != "0.4.5.9000" ){
  remotes::install_github("ipums/ipumsr", force = TRUE)
}


if(!require(dplyr)){
  install.packages("dplyr")
}

if(!require(stringr)){
  install.packages("stringr")
}


##########

## if trying to work within this script interactively, you must change the directory to the app-folder.

# setwd("ipums-api-viewer")

library(shiny)
library(ipumsr)
library(dplyr)
library(stringr)
library(ggplot2)

## Set up wait for extract

## Read in Data
data_path <- file.path("..","Data")
data_check <- list.files(data_path) %>% str_detect(".xml")
extract_path <- list.files(data_path, pattern = ".json")

api_check <- list.files(data_path) %>% str_detect(".json")


if(sum(api_check)>1 | sum(data_check) > 1){
  warning("Error: only copy each allowed in shiny folder: extract definitions (.json), ddi (.xml), or data file (.dat.gz)")
}



if (!file.exists(file.path(data_path,"prcs_migration_ex.xml"))) {
  # Load extract definition from JSON
  prcs_migration_extract <- define_extract_from_json(file.path(data_path,"prcs_migration_ex.json"),
    "usa"
  )
  # Submit, wait for, and download extract
  ddi_filename <- submit_extract(prcs_migration_extract) %>% 
    wait_for_extract() %>% 
    download_extract(download_dir = data_path) %>% 
    basename()
  # Infer data file name from DDI file name
  data_filename <- str_replace(ddi_filename, "\\.xml$", ".dat.gz")
  # Standardize DDI and data file names 
  file.rename(file.path(data_path,ddi_filename),
              file.path(data_path, "prcs_migration_ex.xml"))
  file.rename(file.path(data_path, data_filename),
              file.path(data_path,"prcs_migration_ex.dat.gz"))
}

ddi <- read_ipums_ddi(file.path(data_path, "prcs_migration_ex.xml"))

data <- read_ipums_micro(
  ddi,
  data_file = file.path(data_path, "prcs_migration_ex.dat.gz")
)


#### Initital Prep ####

select_choices <- data.frame("Var"= ddi$var_info$var_name)
select_choices$plot_type <- character(nrow(select_choices))
n_vals <- apply(data,2,function(x){
  length(unique(x))
})

select_choices %>% mutate(plot_type = case_when(n_vals < 60 ~ "CAT",
                                                n_vals > 61 ~ "CONT"))
# 
# for( i in seq_along(ddi$var_info$var_name)){
#   vv <- ddi$var_info[i,]
#   max_l <- length(seq(from=vv$start,
#                       to = vv$end))
#   
#   vals <- vv$val_labels[[1]]
#   
#   nrow(vals) < 25 & nrow(vals) != 0
#   
#   if(max_l > 25){
#     select_choices$plot_type[i] <- "HIDE"
#   } else if (length(vals$val) < 25){
#     select_choices$plot_type[i] <- "CAT"
#   } 
#   
# }

# select_choices$plot_type <- c("CAT", "HIDE", "HIDE", "HIDE", "COUNT","HIDE", "HIDE", "CAT", "CONT", "CONT", "HIDE","CAT", "HIDE", "HIDE", "HIDE", "HIDE", "CAT", "HIDE", "CAT", "CONT", "DICHOT", "DICHOT", "DICHOT", "DICHOT", "CAT", "HIDE", "CAT", "HIDE", "CAT", "CAT", "HIDE", "CAT", "HIDE", "CONT", "CAT", "HIDE")


select_choices$lbl <-  ddi$var_info$var_label


select_vec <- select_choices$Var[select_choices$plot_type!="HIDE"]

names(select_vec) <- select_choices$lbl[select_choices$plot_type!="HIDE"]

#### Create empty objects for later

out_tab <- NULL
vals <- NULL



#### Migration Example prep ####

mig_data <- data

college_regex <- "^[123] year(s)? of college$"


mig_data <- mig_data %>% 
  mutate(
    EDUCD3 = EDUCD %>%
      lbl_collapse(~.val %/% 10) %>% 
      lbl_relabel(
        lbl(2, "Less than High School") ~.val > 0 & .val < 6,
        lbl(3, "High school") ~.lbl == "Grade 12", #<<
        lbl(4, "Some college") ~str_detect(.lbl, college_regex), #<<
        lbl(5, "College or more") ~.val %in% c(10, 11)
      ) %>%
      as_factor()
  )

# Prep income variable
value_to_quintile <- function(x) {
  cut_points <- quantile(x, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
  cut(
    x, 
    breaks = c(-Inf, cut_points, Inf), 
    labels = c("Lowest", "Lower Middle", "Middle", "Upper Middle", "Highest"),
    ordered_result = TRUE
  )
}

hhincome_quintiles <- mig_data %>% 
  filter(PERNUM == 1 & HHINCOME != 9999999) %>% 
  group_by(YEAR) %>% 
  mutate(hhincome_quintile = value_to_quintile(HHINCOME)) %>% 
  ungroup() %>% 
  select(YEAR, SERIAL, hhincome_quintile)

mig_data <- mig_data %>% 
  left_join(hhincome_quintiles, by = c("YEAR", "SERIAL"))

# Prep migration variable
mig_data <- mig_data %>% 
  mutate(
    moved_in_last_year = case_when(
      MIGRATE1 %in% c(0, 9) ~ NA, 
      MIGRATE1 == 1 ~ FALSE, 
      MIGRATE1 %in% 2:4 ~ TRUE
    )
  )


# Prep age variable
age_to_age_group <- function(x) {
  cut_points <- c(9, 17, 34, 64)
  cut(
    x,
    breaks = c(-Inf, cut_points, Inf),
    labels = c("0-9", "10-17", "18-34", "35-64", "65+"),
    ordered_result = TRUE
  )
}

mig_data <- mig_data %>% 
  mutate(age_group = age_to_age_group(AGE))

# UI #####

ui <- fluidPage(
    # Application title
    h3("Exploring Trends Over Time: Puerto Rico Community Survey"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "Var",
                "Select Variable",
                choices = select_vec,
                selected = "AGE"
            ),
            checkboxInput(
              "drop_na",
              "Drop NA/Missing Values"
            ),
            checkboxInput(
              "grouped",
              "Unstack Barplot"
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
          tabPanel(
          "Interactive Plot",
          h3(textOutput("main_plot_lbl")),
          plotOutput("main_plot"),
          h3(textOutput("main_tab_lbl")),
          tableOutput("main_tab")
        ),
        tabPanel(
          "Metadata",
          
          h3("Variable Definition"),
          textOutput("var_desc"),
          h3("Value Labels (All Possible)"),
          tableOutput("vals_lbls")
        ),
        tabPanel(
          "Migration Ex.",
          tabsetPanel(
            tabPanel("Overall",
                     plotOutput("mig_plot_all"),
                     textOutput("mig_text")),
            tabPanel("By Education",
                     plotOutput("mig_plot_edu")),
            tabPanel("By HH Income",
                     plotOutput("mig_plot_inc")),
            tabPanel("By Age",
                     plotOutput("mig_plot_age"))
          )
        )
        
        
        ))
        
    )
)

#### SERVER ####
server <- function(input, output) {
  
  
  ## For interactive testing
  # input <- list("Var" = "EDUCD3","drop_na" = TRUE)
  
  
  #### Render Text Main panel
  output$main_plot_lbl <- renderText({paste("Trends in", input$Var, "over time")})
  
  output$main_tab_lbl <- renderText({paste("Weighted frequencies for", input$Var)})
  
  #### Render Metadata

  output$var_desc <- renderText({
    
    if(input$Var == "EDUCD3"){
      text <- ipums_var_desc(ddi, "EDUCD")
    } else {
      text <- ipums_var_desc(ddi, input$Var)
    }
    text
      })
  
  output$vals_lbls <- renderTable({
    if(input$Var == "EDUCD3"){
      ipums_val_labels(ddi, "EDUCD")
    }else {
      ipums_val_labels(ddi, input$Var)
    }
      }, digits = 0, align = "c")

  
  #### capture data ####
  
  selectedData <- reactive({
    vars <- c(input$Var, "YEAR", "HHWT", "PERWT", "CPI99", "PERNUM")
    
    if(input$Var == "EDUCD3"){
      drop_vals <- data.frame("val" = 1:length(levels(data$EDUCD3)),
                              "lbl" = levels(data$EDUCD3))
    } else {
      drop_vals <- ipums_val_labels(ddi, input$Var)
      
    }
    drop_vals_chk <- stringr::str_detect(drop_vals$lbl, "N/A") |
      stringr::str_detect(drop_vals$lbl, "[Mm]issing")
    drop_vals <- drop_vals[drop_vals_chk,]
      
    if(input$drop_na & nrow(drop_vals) > 0){
      
      data <- data[!data[[input$Var]] %in% drop_vals$val,] 
    } 
    
 
      
      data <- data %>% select(all_of(vars))
      return(data)
     
    
  })
  
  
  
  # Capture main table #####
  
  #### Generate weighted frequencies
  
    to_plot <- reactive({
      
      if (input$Var %in% c("GQ", "COSTELEC", "HHINCOME", "VACANCY", "CINETHH", "NUMPREC")) {
        out_tab <-
          xtabs(selectedData()$HHWT[selectedData()$PERNUM == 1] ~ 
                  selectedData()[[input$Var]][selectedData()$PERNUM == 1] +
                  selectedData()$YEAR[selectedData()$PERNUM == 1])
      } else {
        out_tab <-
          xtabs(selectedData()$PERWT ~ 
                  selectedData()[[input$Var]] + 
                  selectedData()$YEAR)
      }
      
      
      
      #### reformat continuous variables ####
      
      if(input$Var %in% c("COSTELEC", "HHINCOME", "AGE", "INCTOT")){
        
        year_summary <- data.frame("Min." = numeric(),"1st Qu." = numeric(), "Median" = numeric(), "Mean" = numeric(), "3d Qu." = numeric(), "Max." = numeric())
        
        for(i in unique(selectedData()$YEAR)){
          year_summary <- rbind(year_summary, summary(selectedData()[[input$Var]][selectedData()$YEAR==i]))
        }
        
        rownames(year_summary) <- unique(selectedData()$YEAR)
        out_tab <- year_summary
        colnames(out_tab) <- c("Min.", "1st Qu.", "Median", "Mean", "3d Qu.", "Max.")
        
      } else {
        
  
        
        t_labs <- ipums_val_labels(ddi, input$Var)
        if(length(t_labs) > 0 ){
          
          tab_names <- data.frame("val" = as.numeric(rownames(out_tab)))
          tab_names <- left_join(tab_names, t_labs, by = "val")
          rownames(out_tab) <- tab_names$lbl
          
        }
      }
      
     
      return(out_tab)

    })

  #### Main table ####


    output$main_tab <- renderTable({


        as.data.frame.matrix(to_plot())

    },
    striped = TRUE, borderd = TRUE, rownames =TRUE, digits = 0, width = "85%", align = "c"
    )

    
    ##### main plot ####

    output$main_plot <- renderPlot({
      
      cols <- rainbow(nrow(to_plot()), .6, .9)
      par("mar" = c(2,2,0,0))
     layout(matrix(c(1,1,2,1,1,2), ncol =2))
      
      if(input$Var %in% c("COSTELEC", "HHINCOME", "AGE", "INCTOT")){
        
        layout(matrix(c(1,1,1,1), ncol =2))
        

        
        boxplot(selectedData()[[input$Var]] ~ selectedData()$YEAR, col = rainbow(length(unique(selectedData()$YEAR)), .6,.9), xlab = "", ylab = "", main = "")
        
     
      } else {
        ## stacked bar chart
        #### maybe make stacked vs grouped an option
        barplot(to_plot(),
                ylab = "",

                beside = input$grouped,
                col = cols,
                xlab = ""
        )
        par("mar" = c(0,0,0,0))
        plot(1, type = "n", bty = "n", xaxt = "n", yaxt="n", xlab = "", ylab = "")
        if(length(rownames(to_plot())) >10){
          nc <- 4
        } else {
          nc <- 2
        }
        legend("center", legend = rownames(to_plot()), pch = 22, pt.bg = cols, cex = 1.5, ncol = nc) 
      }  
    })
    
    #### Render text for migration example
    # Migration 2015-2019 {.tabset}
    
  output$mig_text <- renderText(
    "The percentage of people who had moved in the last year increased between 2017 
    and 2018 from about 6% to over 8% among all persons in Puerto Rico, but the 
    magnitude of this trend varies by education, household income, and age.
    
    Note: These graphs show trends in point estimates from sample data, without 
    displaying estimates of sampling error. Differences over time or across groups 
    may not be statistically significant. To calculate confidence intervals for 
    point estimates, follow the 
    [IPUMS USA instructions for using replicate weights](https://usa.ipums.org/usa/repwt.shtml)."
  )
    
    #### Render migration plots ####
    
    
    
    ## Overall
    
  output$mig_plot_all <- renderPlot({
    


    mig_data %>% 
      filter(!is.na(moved_in_last_year)) %>% 
      group_by(YEAR) %>% 
      summarize(
        pct_moved = 100 * sum(PERWT[moved_in_last_year]) / sum(PERWT)
      ) %>% 
      ggplot(aes(x = YEAR, y = pct_moved)) +
      geom_line() +
      labs(
        title = "Percentage of people who moved in the past year, 2015-2019",
        x = NULL,
        y = "%"
      )
    
  })
    ## By education
    
  output$mig_plot_edu <- renderPlot({
    mig_data %>% 
      filter(!is.na(moved_in_last_year)) %>% 
      filter(AGE >= 25) %>%
      group_by(YEAR, EDUCD3) %>% 
      summarize(
        pct_moved = 100 * sum(PERWT[moved_in_last_year]) / sum(PERWT),
        .groups = "drop"
      ) %>% 
      ggplot(aes(x = YEAR, y = pct_moved)) +
      geom_line() +
      facet_wrap(~EDUCD3) +
      labs(
        title = "Percentage of people who moved in the past year, 2015-2019",
        subtitle = "Among persons age 25 and older",
        x = NULL,
        y = "%"
      )
  })
    
    ## By household income
    
  output$mig_plot_inc <- renderPlot({
    mig_data %>% 
      filter(!is.na(moved_in_last_year)) %>% 
      filter(!is.na(hhincome_quintile)) %>% 
      group_by(YEAR, hhincome_quintile) %>% 
      summarize(
        pct_moved = 100 * sum(PERWT[moved_in_last_year]) / sum(PERWT),
        .groups = "drop"
      ) %>% 
      ggplot(aes(x = YEAR, y = pct_moved)) +
      geom_line() +
      facet_wrap(~hhincome_quintile) +
      labs(
        title = "Percentage of people who moved in the past year, 2015-2019",
        x = NULL,
        y = "%"
      )
    
  })
    
    
    
    ## By age
   
  output$mig_plot_age <- renderPlot({
    mig_data %>% 
      filter(!is.na(moved_in_last_year)) %>% 
      group_by(YEAR, age_group) %>% 
      summarize(
        pct_moved = 100 * sum(PERWT[moved_in_last_year]) / sum(PERWT),
        .groups = "drop"
      ) %>% 
      ggplot(aes(x = YEAR, y = pct_moved)) +
      geom_line() +
      facet_wrap(~age_group) +
      labs(
        title = "Percentage of people who moved in the past year, 2015-2019",
        x = NULL,
        y = "%"
      )
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)

