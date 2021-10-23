#loading packages
library(shiny)
#loading packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(purrr)
library(janitor)
library(DT)
library(ggmosaic)
library(palmerpenguins)
library(car)
library(shinyBS)


#make some factors
#easier to let ggplot2 control plotting (color, fill) based on type
data(mtcars)
uvals<-sapply(mtcars,function(x){length(unique(x))})
mtcars<-map_if(mtcars,uvals<4,as.factor) %>%
  as.data.frame()


#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank())

#function for confidence interval for stripchart
confint.fun.ttest <- function(x, conf = 0.95){
  return(data.frame(Mean = mean(x, na.rm = T),
                    ymin  = t.test(x, conf.level = conf)$conf.int[1],
                    ymax = t.test(x, conf.level = conf)$conf.int[2]))}

# ui ---------------------------------------------------------------------------

# title ----
header <- dashboardHeader(
  title = "BIOL 116 App"
  
)

# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarid",
              
              br(),
              menuItem("Welcome", tabName = "welcome", icon = icon("dashboard")),
              
              # Horizontal line ----
              tags$hr(),
              
              menuItem("Choose Data", tabName = "input_data", icon = icon("table")),
              conditionalPanel(
                condition = 'input.sidebarid == "input_data"',
                
                # Input: CSV file ----
                fileInput("file1", "Upload Data File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                
                # Input: Select separator ----
                radioButtons("sep", "Separator",
                             choices = c(Semicolon = ";",
                                         Comma = ",",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select which data to display
                selectInput("dataset","Choose Data to Use:",
                            choices =list(penguins = "penguins", mtcars = "mtcars", 
                                          uploaded_file = "inFile"), selected=NULL),
              ),
              
              
              # Horizontal line ----
              tags$hr(),
              
              menuItem("Plot", tabName = "plot", icon = icon("bar-chart-o")),
              conditionalPanel(
                condition = 'input.sidebarid == "plot"',
 
                                # Input: Select which y variable to display
                selectInput("y_var", "Y (Response) Variable:", choices=NULL),
                
                # Input: Select class of y variable
                radioButtons("class_y", label = "Choose the Y Variable's Data Type:", choices = list(Quantitative = "Continuous", Categorical = "Categorical"), selected = ""),
 
                
                # Input: Select which x variable to display
                selectInput("x_var", "X (Explanatory) Variable:", choices=NULL),
                
                # Input: Select class of x variable
                radioButtons("class_x", label = "Choose the X Variable's Data Type:", choices = list(Quantitative = "Continuous", Categorical = "Categorical"), selected = ""),
                
                
                # Input: Select type of plot based on x&y variable class chosen (except for continuous/continuous or categorical/categorical -> no choice; will be scatter or mosaic)
                
                # Input: Select plot type if one variable is discrete and one is continuous
                conditionalPanel(
                  condition = "input.class_x == 'Categorical' && input.class_y == 'Continuous'",
                  selectInput("both_plots", 
                              "Plot Options", 
                              choices = list(Stripchart = "Stripchart", Boxplot = "Boxplot"))
                ),
                # Input: Select which grouping variable to display; if applicable
                conditionalPanel(
                  condition = "input.class_x == 'Categorical' && input.class_y == 'Continuous' && input.group_var == 1", 
                  selectInput("group", "Grouping Variable:", choices=NULL),
                ),
              ),
              
              
              # Horizontal line ----
              tags$hr(),
              
              menuItem("Descriptive Stats", tabName = "stats", icon = icon("calculator")),
              conditionalPanel(
                condition = 'input.sidebarid == "stats"',
                
                # Input: Select which variable to display
                selectInput("var1", "Variable 1 (response variable):", choices=NULL),
                
                # Input: Select class of variable 1
                radioButtons("class1", label = "Choose Class of Variable 1:", choices = list(Quantitative = "Quantitative", Categorical = "Categorical"), selected = ""),
                
                # Input: Select which variable to display
                selectInput("var2", "Variable 2 (explanatory variable):", choices=NULL),
                
                # Input: Select class of variable 2
                radioButtons("class2", label = "Choose Class of Variable 2:", choices = list(Quantitative = "Quantitative", Categorical = "Categorical")),
              ),
              
              # Horizontal line ----
              tags$hr(),
              
              menuItem("Analysis", tabName = "analysis", icon = icon("cog")),
              conditionalPanel(
                condition = 'input.sidebarid == "analysis"',
                
                # Variable selection
                selectInput("response.var", "Choose the response (Y) variable", choices=NULL),
                
                # Input: Select class of response variable
                radioButtons("classr", label = "Choose class of response (Y) variable:", choices = list(Quantitative = "Quantitative", Categorical = "Categorical"), selected = ""),
                
                selectInput("independent.var", "Choose the explantory (X) variable", choices=NULL),
                
                # Input: Select class of Independent variable
                radioButtons("classi", label = "Choose class of explantory (X) variable:", choices = list(Quantitative = "Quantitative", Categorical = "Categorical"), selected = ""),
                
                conditionalPanel(
                  condition = 'input.sidebarid == "analysis" && input.classr == "Quantitative" && input.classi == "Categorical"',
                  
                  # Choosing number of groups in categorical variable
                  radioButtons("categories", label = "How many groups/categories does your categorical variable have?", choices=list(two = "two", more.than.two = "more.than.two"), selected = ""),
                ),
                
                conditionalPanel(
                  condition = 'input.sidebarid == "analysis" && input.classr == "Categorical" && input.classi == "Categorical"',
                  
                  # Choosing number of groups in categorical variable
                  radioButtons("categories2", label = "Do either of your categorical variables have more than two groups/categories?", choices=list(yes = "yes", no = "no"), selected = ""),
                ),
                
                conditionalPanel(
                  condition = 'input.sidebarid == "analysis" && input.classr == "Quantitative" && input.classi == "Categorical" && input.categories == "two"',
                  
                  tags$strong("Two Sample t-test Options"),
                  br(),
                  
                  # Choosing an alpha level
                  numericInput("alpha",
                               label = "Please select a significance level:",
                               value = 0.05,
                               min = 0.01,
                               max = 0.10),
                  
                  # Choosing equal variance or not
                  radioButtons("varequal", label = "Do the two samples have equal variance?", choices=list(yes = "yes", no = "no"), selected = "yes")
                  
                )
                
              )
              
              
              
  )
  
)

# body ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome",
            box(title = "Welcome!", solidHeader = TRUE, width = NULL, status = "info",
                tags$div("This shiny app was created as part of an Open Education Resource for students at the University of British Columbia. The goal was to create an app that simplifies data analysis in Biology labs for students with minimal statistics/coding experience, while maintaining Open Science principles such as reproducibility. To enhance reproducibility, all of the R script used to generate plots, descriptive statistics, and any analyses are displayed alongside the outputs."),
                br(),
                tags$strong("Step 1: Choose or upload a dataset"),
                br(),
                tags$div("You can upload your own dataset following the instructions under the Choose Data tab. Alternatively, both the mtcars and penguins datasets from the base and palmerpenguins package in R are available for use in this app."),
                br(),
                tags$strong("Step 2: Create a plot"),
                br(),
                tags$div("Follow the instructions under the Plot tab to visualize your data and save a copy of your new plot."),
                br(),
                tags$strong("Step 3: Calculate descriptive statistics"),
                br(),
                tags$div("Follow the instructions under the Descriptive Stats tab to calculate statstics for selected variables from your dataset based on their type (ie. quantitative - discrete or continuous, categorical - nominal or ordinal)."),
                br(),
                tags$strong("Step 4: Perform statistical analyses"),
                br(),
                tags$div("Follow the instructions under the Analysis tab to perform statistical tests using your selected data. The type of test performed depends on the types of variables in your dataset.")
            ),
    ),
    tabItem(tabName = "input_data",
            fluidRow(
              box(title = "Instructions", width = 4, solidHeader = TRUE, status = "success", 
                  tags$strong("To upload your data:"),
                  tags$ol(
                    tags$li("Browse for and select a csv or txt file containing your data"), 
                    tags$li("If your file contains headers, ensure you click header option"), 
                    tags$li("Choose the type of seperator used in your data file. If your file is a csv, then choose comma."),
                  ),
                  br(),
                  tags$div("Then a table with your data will display on the screen."),
              ),
              box(title = "Data Table", solidHeader = TRUE, width = 8, status = "info",
                  dataTableOutput('data_table'),
              ),
            ),
    ),
    tabItem(tabName = "plot",
            fluidRow(
              box(title = "Instructions", width = 4, solidHeader = TRUE, status = "success", 
                  tags$strong("To create and save a plot"),
                  tags$ol(
                    tags$li("Choose the y (response) variable"), 
                    tags$li("Choose the data type of the y variable (either 'Quantitative' or 'Categorical')"),
                    tags$li("Choose the x (explanatory) variable"), 
                    tags$li("Choose the data type of the x variable (either 'Quantitative' or 'Categorical')"),
                    tags$li("Depending on your choices for the above, you may be prompted to choose the type of plot you'd like to create"),
                    tags$li("To save a copy of your plot, click the Download Plot button"),
                  ),
                  tags$strong("NOTE:"),
                  tags$div("Only the following 3 combinations will successfully produce a graph:"),
                  tags$ul(
                    tags$li("Y (response) variable is quantitative and X (explanatory) variable is quantitative"),
                    tags$li("Y (response) variable is quantitative and X (explanatory) is categorical "),
                    tags$li("Y (response) variable is categorical and X (explanatory) is categorical"),
                  ),
              ),
              box(title = "Plot", width = 8, solidHeader = TRUE, status = "info",
                  plotOutput("p"),
                  br(),
                  downloadButton('downloadPlot', 'Download Plot'),
              ),
            ),
            fluidRow(
              box(title = "Source Code", solidHeader = TRUE, width = 12, status = "warning", 
                  tags$div("The source code shows you the R script that is used to generate your plot."),
                  verbatimTextOutput("plot_source_code")
              ),
            ),
    ),
    tabItem(tabName = "stats",
            fluidRow(
              box(title = "Instructions", width = 4, solidHeader = TRUE, status = "success", 
                  tags$div("The descriptive statistics displayed are based on the data type of variables you selected (quantitative or categorical)."),
                  tags$strong("NOTE:"),
                  tags$div("Only the following 3 combinations will produce meaningful descriptive statistics"),
                  tags$ul(
                    tags$li("Variable 1 (response variable) is quantitative and Variable 2 (explanatory variable) is quantitative"),
                    tags$li("Variable 1 (response variable) is quantitative and Variable 2 (explanatory variable) is categorical"),
                    tags$li("Variable 1 (response variable) is categorical and Variable 2 (explanatory variable) is categorical"),
                  ),
              ),
              box(title = "Descriptive Statistics", solidHeader = TRUE, width = 8, status = "info",
                  tags$strong("Interpreting the Output"),
                  br(),
                  tags$em("For two quantitative variables"),
                  tags$ul(
                    tags$li("Each variable is shown in the first column. Row '1' represents 'Variable 1' whereas row '2' indicates 'Variable 2'."),
                    tags$li("Common descriptive statistics for quantitative variables are shown. These include: sample size (n), mean, standard deviation (sd), median, and inter-quartile range (iqr)."),
                  ),
                  tags$em("For a quantitative response variable (Variable 1) and a categorical explanatory variable (Variable 2)"),
                  tags$ul(
                    tags$li("Common descriptive statistics for the quantitative response variable are shown grouped by the categories within the categorical explanatory variable."),
                    tags$li("Specifically, rows represent levels (categories) of the categorical variable and columns show the sample size (n), mean, standard deviation (sd), median, and inter-quartile range (iqr) for that category"),
                  ),
                  tags$em("For two categorical variables (nominal or ordinal)"),
                  tags$ul(
                    tags$li("A contingency table showing the frequencies (counts) of observations within each combination of categories (across the two categorical variables)."),
                  ),
                  br(),
                  tags$strong("Descriptive Statistics Output for Your Variables"),
                  verbatimTextOutput("stats"),
              ),
            ),
            fluidRow(
              box(title = "Source Code", solidHeader = TRUE, width = 12, status = "warning", 
                  tags$div("The source code shows you the R script that is used to generate the descriptive statistics for the chosen variables."),
                  verbatimTextOutput("stats_source_code"),
              ),
            ),
            
    ),
    tabItem(tabName = "analysis",
            fluidRow(
              box(title = "Instructions", width = 4, solidHeader = TRUE, status = "success", 
                  tags$div("Statistical tests will be automatically performed based on your selection of variables and their data types. The type of analysis performed depends on the type of data you have."),
                  br(),
                  tags$strong("t-test"),
                  tags$ul(
                    tags$li("This analysis is used when examining a single quantitative (numeric) response variable in relation to a single categorical variable that has only 2 groups."),
                    tags$li("When performing a t-test in this app, you will be asked for a few additional parameters."),
                    tags$ul(
                      tags$li("Type in the significance level you would like to use for the t-test. For example, if you'd like a 5% significance interval, type in 0.05."),
                      tags$li("One assumption of the t-test is that the variance for each sample is approximately equal. However, the t-test used by this app (Welch's t-test) is somewhat robust to deviations in this assumption. For now, we will assume that both of your samples have equal variance. As such, please select 'Yes' when prompted for this option."),
                    ),
                  ),
                  br(),
                  tags$strong("ANOVA"),
                  tags$ul(
                    tags$li("This analysis is used when examining a single quantitative (numeric) response variable in relation to a single categorical explanatory variable that has more than 2 groups."),
                  ),
                  br(),
                  tags$strong("Fisher's exact test"),
                  tags$ul(
                    tags$li("This analysis is used when testing for an association between two categorical variables. It is only used when both categorical variables have exactly 2 levels/groups. For example, if one variable is sex (male/female) and the other is survival (yes/no)."),
                  ),
                  br(),
                  tags$strong("Chi-square contingency analysis"),
                  tags$ul(
                    tags$li("This analysis is used when testing for an association between two categorical variables. It is only used when at least one of the categorical variables has more than 2 levels/groups. For example, if one variable is flower colour (pink/red) and the other is season (spring/summer/fall)."),
                  ),
                  
              ),
              box(title = "Analysis Results", solidHeader = TRUE, width = 8, status = "info",
                  bsAlert("alert"),
                  tags$strong("Interpreting the Output"),
                  br(),
                  htmlOutput("interpret_analysis"),
                  br(),
                  tags$strong("Your Analysis Results"),
                  verbatimTextOutput("analysis"),
              ),
            ),
            fluidRow(
              box(title = "Source Code", solidHeader = TRUE, width = 12, status = "warning", 
                  tags$div("The source code shows you the R script that is used to perform the statistical analysis on the selected variables."),
                  verbatimTextOutput("analysis_source_code")
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)


# server -----------------------------------------------------------------------

server <-  function(input, output, session) {
  
  # Update variables based on the data
  observe({
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "x_var", choices = var.opts)
    updateSelectInput(session, "y_var", choices = var.opts)
    updateSelectInput(session, "group", choices = var.opts)
    updateSelectInput(session, "var1", choices = var.opts)
    updateSelectInput(session, "var2", choices = var.opts)
    updateSelectInput(session, "response.var", choices = var.opts)
    updateSelectInput(session, "independent.var", choices = var.opts)
  })
  
  #get data object
  get_data<-reactive({
    
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    obj<-list(data=get(input$dataset),
              x_var=input$x_var,
              y_var=input$y_var,
              group=input$group
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$x_var, obj$y_var, obj$group) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()
    
    
    obj
    
  })
  
  
  # set uploaded file
  upload_data<-reactive({
    
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  observeEvent(input$file1,{
    inFile<<-upload_data()
  })
  
  # Display any inputted data 
  
  observeEvent(input$file1, {
    output$data_table <- renderDataTable({
      inFile
    })
  })
  
  
  
  #plotting function using ggplot2
  p <- reactive({
    
    #get the data
    plot.obj<-get_data()
    
    #conditions for plotting
    if(is.null(plot.obj)) return()
    
    #make sure variables loaded
    if(plot.obj$x_var == "" | plot.obj$y_var =="") return()
    
    #suppress error messages
    req(input$x_var, input$class_x, input$y_var, input$class_y)
    
    #plot types
    
    if(input$class_x == "Continuous" && input$class_y == "Continuous") {
      ggplot(plot.obj$data,
             aes_string(
               x 		= plot.obj$x_var,
               y 		= plot.obj$y_var)) + 
        geom_point(color='black',
                   alpha=0.5, 
                   position = 'jitter') +
        .theme +
        labs(x 		= input$x_var,
             y 		= input$y_var)
    }
    
    else if(input$class_x == "Categorical" && input$class_y == "Categorical") {
      ggplot(plot.obj$data,
             aes_string(
               x 		= plot.obj$x_var,
               fill 	= plot.obj$y_var)) + 
        geom_mosaic(aes(x = product(!!sym(plot.obj$x_var)), fill = !!sym(plot.obj$y_var))) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
        .theme +
        labs(
          fill 	= input$y_var,
          x 		= input$x_var,
          y 		= "Relative frequency")
    } 
    
    else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Boxplot") {
      ggplot(plot.obj$data,
             aes_string(
               x 		= plot.obj$x_var,
               y 		= plot.obj$y_var)) + 
        geom_boxplot(fill = "lightgrey") +
        .theme +
        labs(x 		= input$x_var,
             y 		= input$y_var)
    }
    
    else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Stripchart") {
      ggplot(plot.obj$data,
             aes_string(
               x 		= plot.obj$x_var,
               y 		= plot.obj$y_var)) + 
        geom_jitter(shape = 1, 
                    position = position_jitter(0.1)) + 
        stat_summary(fun.data = confint.fun.ttest, geom = "errorbar", 
                     colour = "black", width = 0.07, 
                     position = position_nudge(x = 0.15)) +
        stat_summary(fun = mean, 
                     geom = "point", 
                     colour = "firebrick", 
                     size = 2, 
                     position = position_nudge(x = 0.15)) +
        .theme +
        labs(x 		= input$x_var,
             y 		= input$y_var)
    }
    
  })
  
  output$p <- renderPlot({p()})
  
  # Saving the plot
  
  output$downloadPlot <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      png(file)
      print(p())
      dev.off()
    })   
  
  # showing plot source code
  
  plot_source_code <- reactive({
    
    #suppress error messages
    req(input$x_var, input$class_x, input$y_var, input$class_y)
    
    if(input$class_x == "Continuous" && input$class_y == "Continuous") {
      "library(ggplot2)
            
ggplot(data, 
    aes(x = x_var, y = y_var)) + 
    geom_point(color = 'black', alpha = 0.5, position = 'jitter') +
    labs(x = x_var, y = y_var) +
    axis.line = element_line(colour = 'gray', size = .75) +
    panel.background = element_blank() +
    plot.background = element_blank())"
    }
    
    else if(input$class_x == "Categorical" && input$class_y == "Categorical") {
      "library(ggplot2)
library(ggmosaic)

ggplot(data) + 
    geom_mosaic(aes(x = product(plot.obj$x_var), fill = plot.obj$y_var)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    labs(x = x_var, y = 'Relative frequency', fill = y_var) +
    panel.background = element_blank() +
    plot.background = element_blank())"
    } 
    
    else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Boxplot") {
      "library(ggplot2)

ggplot(data, 
    aes(x = x_var, y = y_var)) + 
    geom_boxplot(fill = 'lightgrey') +
    labs(x = x_var, y = y_var) +
    axis.line = element_line(colour = 'gray', size = .75) +
    panel.background = element_blank() +
    plot.background = element_blank())"
    }
    
    else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Stripchart") {
      "library(ggplot2)

confint.fun.ttest <- function(x, conf = 0.95){
  return(data.frame(Mean = mean(x, na.rm = T),
                    ymin  = t.test(x, conf.level = conf)$conf.int[1],
                    ymax = t.test(x, conf.level = conf)$conf.int[2]))}
                    
ggplot(data) + 
        geom_jitter(colour = 'black', size = 3, shape = 1, width = 0.1) + 
        stat_summary(fun.data = confint.fun.ttest, geom = 'errorbar', 
                     colour = 'black', width = 0.07, 
                     position = position_nudge(x = 0.15)) +
        stat_summary(fun = mean, 
                     geom = 'point', 
                     colour = 'firebrick', 
                     size = 2, 
                     position = position_nudge(x = 0.15)) +
    labs(x = x_var, y = y_var) +
    axis.line = element_line(colour = 'gray', size = .75) +
    panel.background = element_blank() +
    plot.background = element_blank())"
    }
    
  })
  
  output$plot_source_code <- renderText({plot_source_code()})
  
  ## Output table of descriptive statistics and sample size 
  
  
  stats <- reactive({ 
    
    #suppress error messages
    req(input$dataset, input$var1, input$class1, input$var2, input$class2)
    
    if(input$class1 == "Quantitative" && input$class2 == "Quantitative"){
      
      var1 <- get(input$dataset)%>%
        select(input$var1, input$var2)%>%
        summarise(n = n(),
                  Mean = mean(get(input$var1), na.rm = T), 
                  SD = sd(get(input$var1), na.rm = T),
                  Median = median(get(input$var1), na.rm = T),
                  iqr = IQR(get(input$var1), na.rm = T))
      
      var2 <- get(input$dataset)%>%
        select(input$var1, input$var2)%>%
        summarise(n = n(),
                  Mean = mean(get(input$var2), na.rm = T), 
                  SD = sd(get(input$var2), na.rm = T),
                  Median = median(get(input$var2), na.rm = T),
                  iqr = IQR(get(input$var2), na.rm = T))
      
      q <- full_join(var1, var2)
      
      q
      
      
    }
    
    else if(input$class1 == "Quantitative" && input$class2 == "Categorical"){
      
      q <- get(input$dataset) %>%
        select(input$var1, input$var2) %>%
        group_by(!!sym(input$var2)) %>% 
        summarise(
              n = n(),
              Mean = mean(get(input$var1), na.rm = T), 
              SD = sd(get(input$var1), na.rm = T),
              Median = median(get(input$var1), na.rm = T),
              iqr = IQR(get(input$var1), na.rm = T)
                  )
      
      q
      
    }
    
    else if(input$class1 == "Categorical" && input$class2 == "Categorical"){
      
      q <- select(get(input$dataset), input$var1, input$var2)
      table(q)
    }
    
  })
  
  output$stats <- renderPrint({stats()})
  
  
  # showing source code for descriptive statistics table
  
  output$stats_source_code <- renderText({
    
    #suppress error messages
    req(input$dataset, input$var1, input$class1, input$var2, input$class2)
    
    if(input$class1 == "Quantitative" && input$class2 == "Quantitative"){
      
      "library(dplyr)

var1 <- data %>%
         summarise(n = n(),
                   Mean = mean(var1), na.rm = T), 
                   SD = sd(var1), na.rm = T),
                   Median = median(var1), na.rm = T),
                   iqr = IQR(var1), na.rm = T))
var2 <- data %>%
        summarise(n = n(),
                  Mean = mean(var2), na.rm = T), 
                  SD = sd(var2), na.rm = T),
                  Median = median(var2), na.rm = T),
                  iqr = IQR(var2), na.rm = T))
            
stats <- full_join(var1, var2)
stats"
      
      
    }
    
    else if(input$class1 == "Quantitative" && input$class2 == "Categorical"){
      
      "library(dplyr)

stats <- data %>%
          group_by(var2) %>%
        summarise(
          n = n(),
          Mean = mean(var1, na.rm = T), 
          SD = sd(var1, na.rm = T),
          Median = median(var1, na.rm = T),
          iqr = IQR(var1, na.rm = T)
        )

stats"
      
    }
    
    else if(input$class1 == "Categorical" && input$class2 == "Categorical"){
      
      "library(dplyr)

stats <- data %>%
         select(var1, var2)

table(stats)"
      
    }
    
  })
  
  ## Analysis Output
  
  analysis <- reactive({ 
    
    #suppress error messages
    req(input$dataset, input$response.var, input$independent.var, input$classr, input$classi)
    
    if(input$classr == "Quantitative" && input$classi == "Categorical" && input$categories == "two"){
      
      createAlert(session, "alert", "tAlert", title = "WARNING", content = "The Two Sample t-test is run assuming that 1) each of the two samples is a random sample from its population 2) the numerical variable is normally distributed in each population 3) the variance (and thus standard deviation) of the numerical variable is the same in both populations", append = FALSE)
      
      req(input$alpha, input$varequal, input$categories)
      
      conf <- (1-input$alpha)
      ve <- ifelse(input$varequal == 'yes', TRUE, FALSE)
      
      t.test(get(input$response.var) ~ get(input$independent.var), data = get(input$dataset), var.equal = ve, conf.level = conf)
      
    }
    
    else if(input$classr == "Quantitative" && input$classi == "Categorical" && input$categories == "more.than.two"){
      
      createAlert(session, "alert", "aAlert", title = "WARNING", content = "The ANOVA is run assuming that 1) the measurements in every group represent a random sample from the corresponding population 2) the Y-variable has a normal distribution in each population 3) the variance is the same in all populations (homogeneity of variance assumption)", append = FALSE)
      
      req(input$categories)
      
      lm <- lm(get(input$response.var) ~ get(input$independent.var), data = get(input$dataset)) 
      lm.anova <- anova(lm)
      lm.anova
      
    }
    
    else if(input$classr == "Categorical" && input$classi == "Categorical" && input$categories2 == "no"){
      
      req(input$categories2)
      
      fisher.table <- xtabs(~ get(input$response.var) + get(input$independent.var), data = get(input$dataset))
      fisher.results <- fisher.test(fisher.table)
      fisher.results
      
    }
    
    else if(input$classr == "Categorical" && input$classi == "Categorical" && input$categories2 == "yes"){
      
      req(input$categories2)
      
      createAlert(session, "alert", "cAlert", title = "WARNING", content = "The Chi-Square Contingency Analysis is run assuming that 1) none of the categories should have an expected frequency of less than one 2) no more than 20% of the categories should have expected frequencies less than five", append = FALSE)
      
      chi.table <- xtabs(~ get(input$response.var) + get(input$independent.var), data = get(input$dataset))
      chisq.results <- chisq.test(chi.table)
      chisq.results
      
      
    }
    
    
  })
  
  output$analysis <- renderPrint({analysis()})
  
  # Analysis source code
  output$analysis_source_code <- renderText({
    
    req(input$dataset, input$response.var, input$independent.var, input$classr, input$classi)
    
    if(input$classr == "Quantitative" && input$classi == "Categorical" && input$categories == "two"){
      
      "t.test(response.var ~ independent.var, data = dataset, var.equal = 'TRUE', conf.level = 0.95)"
      
    }
    
    else if(input$classr == "Quantitative" && input$classi == "Categorical" && input$categories == "more.than.two"){
      
      "library(car)

lm <- lm(response.var ~ independent.var, data = dataset) 
lm.anova <- anova(lm)
lm.anova"
      
    }
    
    else if(input$classr == "Categorical" && input$classi == "Categorical" && input$categories2 == "no"){
      
      "library(stats)

fisher.table <- xtabs(~ response.var + input$independent.var, data = dataset)
fisher.results <- fisher.test(fisher.table)
fisher.results"
      
    }
    
    else if(input$classr == "Categorical" && input$classi == "Categorical" && input$categories2 == "yes"){
      
      "library(stats)

chi.table <- xtabs(~ response.var + independent.var, data = dataset)
chisq.results <- chisq.test(chi.table)
chisq.results"
      
    }
    
  })
  
  output$interpret_analysis <- renderPrint({
    
    req(input$dataset, input$response.var, input$independent.var, input$classr, input$classi)
    
    if(input$classr == "Quantitative" && input$classi == "Categorical" && input$categories == "two"){
      
      str1 <- paste("For this course the main thing we are interested in is the P-value. First, recall the significance value you set before you began your research project. Commonly the significance level (alpha) is set at 0.05. If your significance level is 0.05, this means that if the P-value you obtain is less than 0.05 there is evidence for an association between the two categorical variables. However, if the P-value is equal to or greater than 0.05, then there is no evidence to suggest an association between those two variables.")
      
      str2 <- paste("In the t-test output the P-value is located on the third line of output beside P-value.")
      
      str3 <- paste("Next, you can look at the figure you made to give more information about the direction of the relationship between those variables. For example, in an experiment investigating whether male or female chinstrap penguin differ in body mass; you might provide a statement like this: 'Based on Figure 1, male chinstrap penguins exhibit significantly greater body mass than female penguins'.")   
      
    }
    
    else if(input$classr == "Quantitative" && input$classi == "Categorical" && input$categories == "more.than.two"){
      
      str1 <- paste("For this course the main thing we are interested in is the P-value. First, recall the significance value you set before you began your research project. Commonly the significance level is set at 0.05. If your significance level is 0.05, this means that if the P-value you obtain is less than 0.05 there is evidence for an association between the two categorical variables. However, if the P-value is equal to or greater than 0.05, then there is no evidence to suggest an association between those two variables.")
      
      str2 <- paste("In the ANOVA output the P-value is located in the Pr(>F) column.")
      
      str3 <- paste("Next, you can look at the figure you made to give more information about the direction of the relationship between those variables. For example, in an experiment investigating whether male or female chinstrap penguin differ in body mass; you might provide a statement like this: 'Based on Figure 1, male chinstrap penguins exhibit significantly greater body mass than female penguins'.")    
      
      
    }
    
    else if(input$classr == "Categorical" && input$classi == "Categorical" && input$categories2 == "no"){
      
      str1 <- paste("For this course the main thing we are interested in is the P-value. First, recall the significance value you set before you began your research project. Commonly the significance level is set at 0.05. If your significance level is 0.05, this means that if the P-value you obtain is less than 0.05 there is evidence for an association between the two categorical variables. However, if the P-value is equal to or greater than 0.05, then there is no evidence to suggest an association between those two variables.")
      
      str2 <- paste("In the Fisher's test output the P-value is located on the third line of output beside P-value.")
      
      str3 <- paste("Next, you can look at the figure you made to give more information about the direction of the relationship between those variables. For example, in an experiment investigating whether there is an association between the level of trematode parasitism and the frequency (or probability) of being eaten; you might provide a statement like this: 'Based on Figure 1, the probability of being eaten increases substantially with increasing intensity of parasitism'.")
      
    }
    
    else if(input$classr == "Categorical" && input$classi == "Categorical" && input$categories2 == "yes"){
      
      str1 <- paste("For this course the main thing we are interested in is the P-value. First, recall the significance value you set before you began your research project. Commonly the significance level is set at 0.05. If your significance level is 0.05, this means that if the P-value you obtain is less than 0.05 there is evidence for an association between the two categorical variables. However, if the P-value is equal to or greater than 0.05, then there is no evidence to suggest an association between those two variables.")   
      
      str2 <- paste("In the Chi-Square Contingency analysis output the P-value is located on the third line of output beside P-value.")
      
      str3 <- paste("Next, you can look at the figure you made to give more information about the direction of the relationship between those variables. For example, in an experiment investigating whether there is an association between the level of trematode parasitism and the frequency (or probability) of being eaten; you might provide a statement like this: 'Based on Figure 1, the probability of being eaten increases substantially with increasing intensity of parasitism'.")  
    } 
    
    HTML(paste(str1, str2, str3, sep ='<br/><br/>'))
    
  })
  
  
  
}

# shiny app --------------------------------------------------------------------

shinyApp(ui, server)




