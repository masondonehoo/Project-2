library(rsconnect)
library(shiny)
library(bslib)
library(tidyverse)
library(DT)

nba_clean <- readRDS("nba_cleaned.rds")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("2020-2021 NBA Season Play-by-Play Analytics"),
  
  sidebarLayout(
    #Creating the controls to filter and subset data
    sidebarPanel(
      h4("Filter Dataset"),
      p("Click 'Apply Filters' to apply selections"),
      
      #Filter for Shot Type
      checkboxGroupInput(
        inputId = "cat_shot_type",
        label = "Shot Types:",
        choices = levels(nba_clean$ShotType),
        selected = levels(nba_clean$ShotType)
      ),
      actionButton("toggle_shot_types", "Select/Deselect All Shot Types",
                   class = "btn-sm btn-secondary"),
      hr(),
      
      #Filter for Shot Outcome
      checkboxGroupInput(
        inputId = "cat_outcome",
        label = "Shot Outcome:",
        choices = levels(nba_clean$ShotOutcome),
        selected = levels(nba_clean$ShotOutcome)
      ),
      hr(),
      
      #Filter for quarter interested in
      checkboxGroupInput(
        inputId = "cat_quarter",
        label = "Quarter",
        choices = levels(nba_clean$QuarterLabel),
        selected = levels(nba_clean$QuarterLabel)[1:4]
      ),
      hr(),
      
      #Filter for whether or not there was an assist
      checkboxGroupInput(
        inputId = "cat_assisted",
        label = "Assisted?:",
        choices = levels(nba_clean$IsAssisted),
        selected = levels(nba_clean$IsAssisted)
      ),
      hr(),
      
      #Different sliders for numeric variables
      h5("Numeric Bounds"),
      uiOutput("num_dist_ui"), #Slider for shot distance
      uiOutput("num_sec_ui"), #Slider for seconds remaining
      uiOutput("num_scorediff_ui"), #Slider for difference in score 
      uiOutput("num_totalscore_ui"), #Slider for total score
      hr(),
      
      #Makes sure new summaries are printed until this button is clicked
      actionButton("update_data", "Apply Filters", 
                   class = "btn-primary btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        #Creating "About" panel
        tabPanel(
          "About",
          br(),
          h3("About this Application"),
          p("This shiny app is an interactive webpage to explore play-by-play
            data from the 2020-2021 NBA Season, such as game situations,
            shot types, and many more data points"),
          
          h4("Data Source"),
          p("The data comes from a publically available kaggle data set which
            compiles play-by-play logs from every NBA Game from the 2015-2016
            NBA Season to the 2020-2021 NBA Season"),
          p(tags$a(
            href = "https://www.kaggle.com/datasets/schmadam97/nba-playbyplay-data-20182019/data",
            target = "_blank",
            "Take a look at the Kaggle dataset")),
          
          h4("App Navigation Guide"),
          tags$ul(
            tags$li(strong("Sidebar Panel:"), 
                    " Filter by categorical/numerical variables"),
            tags$li(strong("About Tab:"), " About the app and the data"),
            tags$li(strong("Data Download Tab:"), " Download a csv copy of the data"),
            tags$li(strong("Data Exploration Tab:"), " Create custom numerical/graphical summaries"),
          ),
          br(),
          
          img(src = "nba-logo.png", height = "120px", alt = "NBA Logo")
        ),
        
        #Creating panel to download the dataset being used
        tabPanel(
          "Data Downlaod",
          br(),
          
          downloadButton("download_csv", "Download Filtered Dataset",
                         class = "btn-success"),
          br(), br(),
        ),
        
        #Creating panel for data exploration
        tabPanel(
          "Data Exploration",
          br(),
          tabsetPanel(
            #Subtab for numerical and graphical summaries
            tabPanel(
            "Numerical Summaries & Tables",
            br(),
            selectInput("table_mode", "Summary Display Mode:",
                        choices = c("Categorical Contingency Table",
                                    "Grouper Numeric Statistics")),
            uiOutput("table_controls.ui"),
            hr(),
            
            verbatimTextOutput("contingency_output"),
            
            dataTableOutput("summary_stats_table")
          ),
          
          #Subtab for plots
          tabPanel(
            "Graphical Exploration",
            br(),
            
            fluidRow(
              #Controls for plot graphics
              column(3, selectInput("plot_type", "Plot Type:",
                                    choices = c("Bar Chart", "Histogram", "Boxplot",
                                                "Density Plot", "Scatterplot",
                                                "2D Density Heatmap"))),
              column(3, selectInput("x_var", "X Variable:",
                                    choices = c("ShotDist", "SecLeft", "ScoreDiff",
                                                "TotalScore", "ShotType", "ShotOutcome",
                                                "QuarterLabel", "IsAssisted"))),
              column(3, selectInput("y_var", "Y Variable", 
                                    choices = c("ShotDist", "SecLeft", "ScoreDiff",
                                                "TotalScore"))),
              column(3, selectInput("fill_color_var", "Fill/Color-by:",
                                    choices = c("None", "ShotOutcome", 
                                                "IsAssisted", "ShotType",
                                                "QuarterLabel")))
            ),
            fluidRow(
              #Controls for faceting and layout
              column(6, selectInput("facet_var", "Facet By:", 
                                    choices = c("None", "QuarterLabel",
                                                "ShotOutcome", "IsAssisted",
                                                "ShotType"))),
              column(6, checkboxInput("coord_flip", "Flip Coordinates", value = F))
            ),
            hr(),
            )
          )
        )
      )
    )
  )
)
