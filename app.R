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
        choices = sort(unique(nba_clean$ShotType)),
        selected = sort(unique(nba_clean$ShotType))
      ),
      actionButton("toggle_shot_types", "Select/Deselect All Shot Types",
                   class = "btn-sm btn-secondary"),
      hr(),
      
      #Filter for Shot Outcome
      checkboxGroupInput(
        inputId = "cat_outcome",
        label = "Shot Outcome:",
        choices = sort(unique(nba_clean$ShotOutcome)),
        selected = sort(unique(nba_clean$ShotOutcome))
      ),
      hr(),
      
      #Filter for quarter interested in
      checkboxGroupInput(
        inputId = "cat_quarter",
        label = "Quarter",
        choices = sort(unique(nba_clean$QuarterLabel)),
        selected = sort(unique(nba_clean$QuarterLabel))
      ),
      hr(),
      
      #Filter for whether or not there was an assist
      checkboxGroupInput(
        inputId = "cat_assisted",
        label = "Assisted?:",
        choices = sort(unique(nba_clean$IsAssisted)),
        selected = sort(unique(nba_clean$IsAssisted))
      ),
      hr(),
      
      #Different sliders for numeric variables
      h5("Numeric Bounds"),
      uiOutput("num_dist_ui"), #Slider for shot distance
      uiOutput("num_sec_ui"), #Slider for seconds remaining
      uiOutput("num_scorediff_ui"), #Slider for difference in score 
      uiOutput("num_totalscore_ui"), #Slider for total score
      hr(),
      
      #Makes sure no new summaries are printed until this button is clicked
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
          "Data Download",
          br(),
          
          downloadButton("download_csv", "Download Filtered Dataset",
                         class = "btn-success"),
          br(), br(),
          dataTableOutput("subset_table")
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
                                    "Grouped Numeric Statistics")),
            uiOutput("table_controls_ui"),
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
            plotOutput("exploration_plot", height = "500px")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session){
  
  #Creating UI slider for range of values for ShotDist
  output$num_dist_ui <- renderUI({
    min_v <- min(nba_clean$ShotDist, na.rm = T)
    max_v <- max(nba_clean$ShotDist, na.rm = T)
    sliderInput("num_dist", "Shot Distance (ft):", min = min_v, max = max_v,
                value = c(min_v, max_v))
  })
  
  #Creating UI slider for range of values for SecLeft
  output$num_sec_ui <- renderUI({
    min_v <- min(nba_clean$SecLeft, na.rm = T)
    max_v <- max(nba_clean$SecLeft, na.rm = T)
    sliderInput("num_sec", "Seconds Left In Quarter:", min = min_v, max = max_v,
                value = c(min_v, max_v))
  })
  
  #Creating UI slider for range of values for ScoreDiff
  output$num_scorediff_ui <- renderUI({
    min_v <- min(nba_clean$ScoreDiff, na.rm = T)
    max_v <- max(nba_clean$ScoreDiff, na.rm = T)
    sliderInput("num_scorediff", "Score Difference (Home - Away)", min = min_v,
                max = max_v, value = c(min_v, max_v))
  })
  
  #Creating UI slider for range of values for TotalScore
  output$num_totalscore_ui <- renderUI({
    min_v <- min(nba_clean$TotalScore, na.rm = T)
    max_v <- max(nba_clean$TotalScore, na.rm = T)
    sliderInput("num_totalscore", "Total Score in Game", min = min_v, max = max_v,
                value = c(min_v, max_v))
  })
  
  #Select/Deselect all ShotType values 
  observeEvent(input$toggle_shot_types, {
    current <- input$cat_shot_type
    all_types <- sort(unique(nba_clean$ShotType))
    if(length(current) < length(all_types)){
      updateCheckboxGroupInput(session, "cat_shot_type", selected = all_types)
    } else{
      updateCheckboxGroupInput(session, "cat_shot_type", selected = character(0))
    }
  })
  
  
  filtered_data <- eventReactive(input$update_data, ignoreNULL = F, {
    
    #Checks that filters for each variable were selected
    req(input$cat_shot_type, input$cat_outcome, input$cat_quarter, input$cat_assisted)
    
    #Filters data based on what variable is selected
    df <- nba_clean %>%
      filter(
        ShotType %in% input$cat_shot_type,
        ShotOutcome %in% input$cat_outcome,
        QuarterLabel %in% input$cat_quarter,
        IsAssisted %in% input$cat_assisted
      )
    
    #Filters ShotDist based on the values specified by UI slider
    if(!is.null(input$num_dist)){
      df <- df %>% filter(ShotDist >= input$num_dist[1] & ShotDist <= input$num_dist[2])
    }
    
    #Filters SecLeft based on the values specified by UI slider
    if(!is.null(input$num_sec)){
      df <- df %>% filter(SecLeft >= input$num_sec[1] & SecLeft <= input$num_sec[2])
    }
    
    #Filters ScoreDiff based on the values specified by UI slider
    if(!is.null(input$num_scorediff)){
      df <- df %>% filter(ScoreDiff >= input$num_scorediff[1] & ScoreDiff <= input$num_scorediff[2])
    }
    
    #Filters TotalScore based on the values specified by UI slider
    if(!is.null(input$num_totalscore)){
      df <- df %>% filter(TotalScore >= input$num_totalscore[1] & TotalScore <= input$num_totalscore[2])
    }
    
    df
  })
  
  #Shows preview of data table with filtered options
  output$subset_table <- renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = T))
  })
  
  #Allows user to download the csv of the filtered data set
  output$download_csv <- downloadHandler(
    filename = function() {paste0("nba_filtered", Sys.Date(), ".csv")},
    content = function(file) {write.csv(filtered_data(), file, row.names = F)}
  )
  
  #Setting up Contingency Tables/Summary Statistics
  output$table_controls_ui <- renderUI({
    if(input$table_mode == "Categorical Contingency Table"){
      tagList(
        selectInput("cat_v1", "Row Categorical Variable:", 
                    choices = c("ShotType", "ShotOutcome", "QuarterLabel", "IsAssisted")),
        selectInput("cat_v2", "Column Categorical Variable:", 
                    choices = c("None", "ShotType", "ShotOutcome", "QuarterLabel", "IsAssisted"))
      )
      } else{
        tagList(
          selectInput("num_v", "Numeric Variable to Summarize:", 
                      choices = c("ShotDist", "SecLeft", "ScoreDiff", "TotalScore")),
          selectInput("group_v", "Group to Summarize Across:",
                      choices = c("ShotType", "ShotOutcome", "QuarterLabel", "IsAssisted"))
        )
      }
  })
  
  #Renders Contingency Table
  output$contingency_output <- renderPrint({
    data <- filtered_data()
    req(data, input$table_mode == "Categorical Contingency Table", input$cat_v1)
    
    if(is.null(input$cat_v2) || input$cat_v2 == "None"){
      table(data[[input$cat_v1]])
    } else{
      table(Row = data[[input$cat_v1]], Column = data[[input$cat_v2]])
    }
  })
  
  #Renders Numeric Summary Statistics
  output$summary_stats_table <- renderDataTable({
    data <- filtered_data()
    req(data, input$table_mode == "Grouped Numeric Statistics",
        input$num_v, input$group_v)
    
    sum_stats <- data %>%
      group_by(!!sym(input$group_v)) %>%
      summarize(
        Mean = round(mean(!!sym(input$num_v), na.rm = T), 2),
        SD = round(sd(!!sym(input$num_v), na.rm = T), 2),
        Median = round(median(!!sym(input$num_v), na.rm = T), 2),
        IQR = round(IQR(!!sym(input$num_v), na.rm = T), 2),
        Min = min(!!sym(input$num_v), na.rm = T),
        Max = max(!!sym(input$num_v), na.rm = T),
        Count = n(),
        .groups = "drop"
      )
    datatable(sum_stats, options = list(pageLength = 10, dom = 't'))
  })
  
  #Renders Plot of Filtered Data
  output$exploration_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "Filter produced no result, please adjust.")
    )
    
    p <- ggplot(data)
    
    #For bar charts
    if(input$plot_type == "Bar Chart"){
      if(input$fill_color_var != "None"){
        p <- p + geom_bar(aes(x = !!sym(input$x_var), fill = !!sym(input$fill_color_var)),
                          position = "dodge")
      } else{
        p <- p + geom_bar(aes(x = !!sym(input$x_var)), fill = "navyblue")
      }
      
      #For histograms
    } else if(input$plot_type == "Histogram"){
      if(input$fill_color_var != "None"){
        p <- p + geom_histogram(aes(x = !!sym(input$x_var), fill = !!sym(input$fill_color_var)),
                                alpha = 0.6, position = "identity", bins = 30)
      } else{
        p <- p + geom_histogram(aes(x = !!sym(input$x_var)), fill = "navyblue",
                                bins = 30)
      }
      
      #For boxplots
    } else if(input$plot_type == "Boxplot"){
      if(input$fill_color_var != "None"){
        p <- p + geom_boxplot(aes(x = !!sym(input$x_var), y = !!sym(input$y_var),
                                  fill = !!sym(input$fill_color_var)))
      } else{
        p <- p + geom_boxplot(aes(x = !!sym(input$x_var), y = !!sym(input$y_var)),
                              fill = "navyblue")
      }
      
      #For density plots
    } else if(input$plot_type == "Density Plot"){
      if(input$fill_color_var != "None"){
        p <- p + geom_density(aes(x = !!sym(input$x_var), fill = !!sym(input$fill_color_var)),
                              alpha = 0.5)
      } else{
        p <- p + geom_density(aes(x = !!sym(input$x_var)), fill = "navyblue",
                              alpha = 0.5)
      }
      
      #For scatterplots
    } else if(input$plot_type == "Scatterplot"){
      if(input$fill_color_var != "None"){
        p <- p + geom_point(aes(x = !!sym(input$x_var), y = !!sym(input$y_var),
                                color = !!sym(input$fill_color_var)), alpha = 0.5)
      } else{
        p <- p + geom_point(aes(x = !!sym(input$x_var), y = !!sym(input$y_var)),
                            color = "navyblue", alpha = 0.5)
      }
      
      #For 2D density heatmaps
    } else if(input$plot_type == "2D Density Heatmap"){
      p <- p + geom_bin2d(aes(x = !!sym(input$x_var), y = !!sym(input$y_var)),
                          bins = 30) + scale_fill_viridis_c()
    }
    
    #Applies faceting if specified by user
    if(input$facet_var != "None"){
      p <- p + facet_wrap(vars(!!sym(input$facet_var)))
    }
    
    #Flips the coordinates if specified by user and makes sure names on the
    #x-axis don't overlap
    if(input$coord_flip){
      p <- p + coord_flip()
    } else{
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    #Sets the y-axis label depending on what type of graph is requested
    y_label <- if(input$plot_type %in% c("Bar Chart", "Histogram")){
      "Count"
    } else if(input$plot_type %in% "Density Plot"){
      "Density"
    } else{
      input$y_var
    }
    
    #Outputs the plot
    p + theme_minimal() + labs(title = paste(input$plot_type, 
                                                  "of 2020-2021 NBA Dataset"),
                                    x = input$x_var, y = y_label)
  })
}

shinyApp(ui = ui, server = server)
