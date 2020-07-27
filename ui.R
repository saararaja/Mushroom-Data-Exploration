###UI for Mushroom Exploration App

#Import Libraries
library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(caret)
library(tree)
library(class)
library(cluster)
library(ggplot2)
library(reshape2)
library(dendextend)

# Read in Data
Datafile <- read_excel("./mushrooms.xlsx") %>%
  rename('cap_shape' = `cap-shape`,
         'cap_surface' = `cap-surface`,
         'cap_color' = `cap-color`,
         'gill_attachment' = `gill-attachment`,
         'gill_spacing' = `gill-spacing`,
         'gill_size' = `gill-size`,
         'gill_color' = `gill-color`,
         'stalk_shape' = `stalk-shape`,
         'stalk_root' = `stalk-root`,
         'stalk_surface_above_ring' = `stalk-surface-above-ring`,
         'stalk_surface_below_ring' = `stalk-surface-below-ring`,
         'stalk_color_above_ring' = `stalk-color-above-ring`,
         'stalk_color_below_ring' = `stalk-color-below-ring`,
         'veil_type' = `veil-type`,
         'veil_color' = `veil-color`,
         'ring_number' = `ring-number`,
         'ring_type' = `ring-type`,
         'spore_print_color' = `spore-print-color`)


#Make all the columns factors
col_names <- names(Datafile)
Datafile[,col_names] <- lapply(Datafile[,col_names], factor)

#The Variable Names
col_names <- names(Datafile)[-1]

attach(Datafile)




# Define UI for application 
shinyUI(fluidPage(navbarPage("Mushroom Classification App",
                             
                             
                             #Tab 1: Information
                             tabPanel("About",
                                      includeMarkdown("./intro.md"),
                                      hr()),
                             
                             #Tab 2: Full Raw Data
                             tabPanel("Raw Data",
                                      sidebarPanel(width = 3,
                                                   h3("Select Data of Interest"),
                                                   selectizeInput("subset", "Data Subset", selected = "All Mushrooms Data", choices = c("All Mushrooms Data", "Edible Mushrooms Data", "Poisonous Mushrooms Data")),
                                                   hr(),
                                                   h3("Download the Data"),
                                                   downloadButton("downloadData", "Download"),
                                                   hr(),
                                                   includeMarkdown("./data dictionary.md")
                                      ),
                                      mainPanel(
                                        uiOutput("datatitle"),
                                        dataTableOutput("rawdata")
                                      )),
                             
                             withMathJax(),
                             
                             #Tab 3: Data Exploration
                             tabPanel("Data Exploration",
                                      sidebarPanel(width = 3,
                                                   h3("Exploration Specifications"),
                                                   hr(),
                                                   selectizeInput("number", "Number of Variables", selected = "One Variable Summary", choices = c("One Variable Summary", "Two Variable Summary")),
                                                   conditionalPanel(condition = "input.number =='One Variable Summary'",
                                                                    selectizeInput("one", "Select One Predictor Variable to Analyze", choices = col_names),
                                                                    checkboxInput("split", "Split by Response Variable?"),
                                                                    conditionalPanel(condition = "input.split",
                                                                                     checkboxInput("chi", "Show Chi Squared Results?"))
                                                   ),
                                                   conditionalPanel(condition = "input.number=='Two Variable Summary'",
                                                                    selectizeInput("two", "Select Two Predictor Variables to Analyze", choices = col_names, multiple = TRUE, options = list(maxItems = 2L)),
                                                                    checkboxInput("chi2", "Show Chi Squared Results?")
                                                   ),
                                                   hr(),
                                                   includeMarkdown("./data dictionary.md")
                                      ),
                                      mainPanel(
                                        h2("Data Summaries"),
                                        fluidRow(column(width = 6,
                                                        downloadButton("saveplot", "Save Plot"),
                                                        hr(),
                                                        h4("Plot:"),
                                                        plotOutput("bars")
                                        ),
                                        column(width = 3,
                                               downloadButton("downloadData2", "Download Plot Data"),
                                               hr(),
                                               h4("Frequency Table:"),
                                               tableOutput("freq")
                                        )
                                        ),
                                        conditionalPanel(condition = "input.chi",
                                                         fluidRow(
                                                           h3("Pearson's Chi Squared Test for Independence"),
                                                           withMathJax("$$\\chi^2=\\sum_{i=1}^{k}\\frac{(O_i-E_i)^2}{E_i}$$"),
                                                           textOutput("chi_results")
                                                         )
                                        ),
                                        conditionalPanel(condition = "input.chi2",
                                                         fluidRow(
                                                           h3("Pearson's Chi Squared Test for Independence"),
                                                           withMathJax("$$\\chi^2=\\sum_{i=1}^{k}\\frac{(O_i-E_i)^2}{E_i}$$"),
                                                           textOutput("chi_results2")
                                                         )
                                        )
                                      )
                             ),
                             
                             
                             #Tab 4: Data Clustering
                             tabPanel("Clustering",
                                      sidebarPanel(
                                        h3("Clustering Options"),
                                        sliderInput("clust", "Choose Number of Clusters",
                                                    min = 2, max = 10, value = 5, step = 1),
                                        selectizeInput("color", "Choose Color Palette", choices = c("Pretty", "Neutral"))
                                      ),
                                      mainPanel(
                                        h2("Hierarchical Clustering"),
                                        plotOutput("Dendro"),
                                        h4("Note: Please allow a few minutes for this tab to refresh after each option selection")
                                      )
                             ),
                             
                             #Tab 5: Modeling
                             tabPanel("Modeling",
                                      sidebarPanel(width = 3,
                                                   h3("Model Selection"),
                                                   selectizeInput("model", "Select Model Type", selected = "KNN", choices = c("KNN", "Classification Tree")),
                                                   conditionalPanel(condition = "input.model == 'KNN'",
                                                                    sliderInput("K", "Select Number of Neighbors",
                                                                                min = 5, max = 50, value = 5, step = 5)
                                                   ),
                                                   conditionalPanel(condition = "input.model =='Classification Tree'",
                                                                    sliderInput("nodes", "Select Complexity Parameter",
                                                                                min = .05, max = .85, value = .05, step = .1)
                                                   )
                                      ),
                                      mainPanel(fluidRow(column(width = 6,
                                                                h3("Training Set Cross Validation Results"),
                                                                plotOutput("cv"),
                                                                hr()
                                      ),
                                      column(width = 3,
                                             h3("Test Set Confusion Matrix"),
                                             verbatimTextOutput("confusion"),
                                             h4(textOutput("acc"))
                                      )
                                      ),
                                      fluidRow(
                                        h3("Make Custom Prediction"),
                                        column(width = 2,
                                               h4("Choose Values"),
                                               selectizeInput("cap_shape", "cap_shape", choices = levels(cap_shape)),
                                               selectizeInput("cap_surface", "cap_surface", choices = levels(cap_surface)),
                                               selectizeInput("cap_color", "cap_color", choices = levels(cap_color)),
                                               selectizeInput("bruises", "bruises", choices = levels(bruises)),
                                               selectizeInput("odor", "odor", choices = levels(odor)),
                                               selectizeInput("gill_attachment", "gill_attachment", choices = levels(gill_attachment)),
                                               selectizeInput("gill_spacing", "gill_spacing", choices = levels(gill_spacing)),
                                               selectizeInput("gill_size", "gill_size", choices = levels(gill_size)),
                                               selectizeInput("gill_color", "gill_color", choices = levels(gill_color)),
                                               selectizeInput("stalk_shape", "stalk_shape", choices = levels(stalk_shape)),
                                               selectizeInput("stalk_root", "stalk_root", choices = levels(stalk_root))
                                        ),
                                        column(width = 2, 
                                               h4("Choose Values"),
                                               selectizeInput("stalk_surface_above_ring", "stalk_surface_above_ring", choices = levels(stalk_surface_above_ring)),
                                               selectizeInput("stalk_surface_below_ring", "stalk_surface_below_ring", choices = levels(stalk_surface_below_ring)),
                                               selectizeInput("stalk_color_above_ring", "stalk_color_above_ring", choices = levels(stalk_color_above_ring)),
                                               selectizeInput("stalk_color_below_ring", "stalk_color_below_ring", choices = levels(stalk_color_below_ring)),
                                               selectizeInput("veil_color", "veil_color", choices = levels(veil_color)),
                                               selectizeInput("ring_number", "ring_number", choices = levels(ring_number)),
                                               selectizeInput("ring_type", "ring_type", choices = levels(ring_type)),
                                               selectizeInput("spore_print_color", "spore_print_color", choices = levels(spore_print_color)),
                                               selectizeInput("population", "population", choices = levels(population)),
                                               selectizeInput("habitat", "habitat", choices = levels(habitat))
                                        ),
                                        column(width = 5,
                                               actionButton("new", "Make New Prediction"),
                                               br(),
                                               br(),
                                               h3(textOutput("outcome"))
                                        )
                                        
                                      )
                                      )
                             )
                             
                             
                             
                             
                             
)))


