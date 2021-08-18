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
                
                menuItem("Upload Data", tabName = "input_data", icon = icon("table")),
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
                                 selected = ",")
                ),
                
                
                # Horizontal line ----
                tags$hr(),
                
                menuItem("Plot", tabName = "plot", icon = icon("bar-chart-o")),
                conditionalPanel(
                    condition = 'input.sidebarid == "plot"',
                    
                    # Input: Select which data to display
                    selectInput("dataset","Data:",
                                choices =list(penguins = "penguins", mtcars = "mtcars",
                                              uploaded_file = "inFile"), selected=NULL),
                    
                    # Input: Select which x variable to display
                    selectInput("x_var", "X Variable:", choices=NULL),
                    
                    # Input: Select class of x variable
                    radioButtons("class_x", label = "Choose Class of X Variable:", choices = list(Quantitative = "Continuous", Categorical = "Categorical"), selected = ""),
                    
                    # Input: Select which y variable to display
                    selectInput("y_var", "Y Variable:", choices=NULL),
                    
                    # Input: Select class of y variable
                    radioButtons("class_y", label = "Choose Class of Y Variable:", choices = list(Quantitative = "Continuous", Categorical = "Categorical"), selected = ""),
                    
                    # Input: Select type of plot based on x&y variable class chosen (except for continuous/continuous or categorical/categorical -> no choice; will be scatter or mosaic)
                    
                    # Input: Select plot type if one variable is discrete and one is continuous
                    conditionalPanel(
                        condition = "input.class_x == 'Categorical' && input.class_y == 'Continuous' || 
                                input.class_x == 'Continuous' && input.class_y == 'Categorical'",
                        selectInput("both_plots", 
                                    "Plot Options", 
                                    choices = list(Stripchart = "Stripchart", Boxplot = "Boxplot", Barchart = "Barchart"))
                    ),
                    conditionalPanel(
                        condition = "input.class_x == 'Categorical' && input.class_y == 'Continuous' && input.both_plots == 'Barchart' ||
                        input.class_x == 'Continuous' && input.class_y == 'Categorical' && input.both_plots == 'Barchart'",
                        checkboxInput("group_var", "Do you have a grouping variable?", value = FALSE)
                    ),
                    
                    # Input: Select which grouping variable to display; if applicable
                    conditionalPanel(
                        condition = "input.class_x == 'Categorical' && input.class_y == 'Continuous' && input.group_var == 1 ||
                        input.class_x == 'Continuous' && input.class_y == 'Categorical' && input.group_var == 1", 
                        selectInput("group", "Grouping Variable:", choices=NULL)),
                ),
                
                
                # Horizontal line ----
                tags$hr(),
                
                menuItem("Descriptive Stats", tabName = "stats", icon = icon("calculator")),
                conditionalPanel(
                    condition = 'input.sidebarid == "stats"',
                    
                    # Input: Select which data to display
                    selectInput("stats.dataset","Data:",
                                choices =list(penguins = "penguins", mtcars = "mtcars",
                                              uploaded_file = "inFile"), selected=NULL),
                    
                    # Input: Select which variable to display
                    selectInput("var1", "Variable 1:", choices=NULL),
                    
                    # Input: Select class of variable 1
                    radioButtons("class1", label = "Choose Class of Variable 1:", choices = list(Quantitative = "Quantitative", Categorical = "Categorical")
                                 , selected = ""),
                    
                    # Input: Select which variable to display
                    selectInput("var2", "Variable 2:", choices=NULL),
                    
                    # Input: Select class of variable 2
                    radioButtons("class2", label = "Choose Class of Variable 2:", choices = list(Quantitative = "Quantitative", Categorical = "Categorical"))
                    )
            
    )
    
)

# body ----
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "welcome",
                box(title = "Welcome!", solidHeader = TRUE, width = NULL, status = "info",
                    tags$div("This shiny app was created as part of an Open Education Resource for students at the University of British Columbia. The goal was to create an app that simplifies data analysis in Biology labs for students with minimal statistics/coding experience, while maintaining Open Science principles such as reproducibility. To enhance reproducibility, all of the R script used to generate the plots and descriptive statistics is displayed alongside the outputs."),
                    br(),
                    tags$strong("Choose or upload a dataset:"),
                    br(),
                    tags$div("You can upload your own dataset following the instructions under the Upload Data tab. Alternatively, both the mtcars and penguins datasets from the base and palmerpenguins package in R are available for use in this app."),
                    br(),
                    tags$strong("Create a plot:"),
                    br(),
                    tags$div("Follow the instructions under the Plot tab to visualize your data and save a copy of your new plot."),
                    br(),
                    tags$strong("Calculate descriptive statistics:"),
                    br(),
                    tags$div("Follow the instructions under the Descriptive Stats tab to calculate statstics for selected variables from your dataset based on their class (ie. quantitative - discrete or continuous, categorical - nominal or ordinal)."),
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
                        tags$strong("Select a dataset"),
                        tags$div("Both the mtcars and penguins datasets are loaded into this app. Alternatively, you can upload your own dataset under the Upload Data tab. To see a description of the mtcars dataset", tags$a("click here.", href = "https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars"), "To see a description of the penguins dataset", tags$a("click here.", href = "https://www.rdocumentation.org/packages/palmerpenguins/versions/0.1.0")),
                        br(),
                        tags$strong("To create and save a plot"),
                        tags$ol(
                            tags$li("Choose the x variable"), 
                            tags$li("Choose the type/class of the x variable"),
                            tags$li("Choose the y variable"), 
                            tags$li("Choose the type/class of the y variable"),
                            tags$li("Depending on your choices for the above, you may be prompted to choose the type of plot you'd like to create"),
                            tags$li("To save a copy of your plot, click the Download Plot button"),
                        ),
                        tags$strong("NOTE:"),
                        tags$div("If you would like to create a grouped bar chart:"),
                        tags$ol(
                            tags$li("Both the x variable and the grouping variable should be categorical (nominal or ordinal)"),
                            tags$li("The y variable should be continuous (ie. frequency or count data)"),
                        ),
                        br(),
                        tags$strong("Source Code:"),
                        tags$div("The source code shows you the R script that is used to generate your plot."),
                        ),
                    box(title = "Plot", width = 8, solidHeader = TRUE, status = "info",
                        plotOutput("p"),
                        br(),
                        downloadButton('downloadPlot', 'Download Plot'),
                        ),
                ),
                fluidRow(
                    box(title = "Source Code", solidHeader = TRUE, width = 12, status = "warning", 
                        verbatimTextOutput("plot_source_code")
                    ),
                ),
        ),
        tabItem(tabName = "stats",
                fluidRow(
                    box(title = "Instructions", width = 4, solidHeader = TRUE, status = "success", 
                        tags$strong("To calculate descriptive statistics"),
                        tags$ol(
                            tags$li("Choose a dataset from the Data dropdown menu"), 
                            tags$li("Select your first variable from the 'Variable 1' dropdown menu"), 
                            tags$li("Choose the type/class of variable 1"),
                            tags$li("Select a second variable from the 'Variable 2' dropdown menu"), 
                            tags$li("Choose the type/class of variable 2"),
                        ),
                        tags$div("Descriptive statistics will automatically be generated for the two variables chosen. The descriptive statistics displayed are based on the class/type of variables you selected."),
                        br(),
                        tags$strong("Interpreting the Output"),
                        br(),
                        tags$em("For two quantitative variables (discrete or continuous)"),
                        tags$ul(
                            tags$li("Each variable is shown in the first column. Row '1' represents 'Variable 1' whereas row '2' indicates 'Variable 2'."),
                            tags$li("Common descriptive statistics for quantitative variables are shown. These include: sample size (n), mean, standard deviation (sd), median, and inter-quartile range (iqr)."),
                        ),
                        tags$em("For a quantitative variable (discrete or continuous) and a categorical variable (nominal or ordinal)"),
                        tags$ul(
                            tags$li("Common descriptive statistics for quantitative variables are shown grouped by the categorical variable."),
                            tags$li("Specifically, rows represent levels of the categorical variable and columns show the sample size (n), mean, standard deviation (sd), median, and inter-quartile range (iqr) for that group."),
                        ),
                        tags$em("For two categorical variables (nominal or ordinal)"),
                        tags$ul(
                            tags$li("A table showing the frequencies (counts) of each group is displayed."),
                        ),
                        tags$div(""),
                        br(),
                        tags$strong("Source Code:"),
                        tags$div("The source code shows you the R script that is used to generate the descriptive statistics for the chosen variables."),
                        ),
                    box(title = "Descriptive Statistics", solidHeader = TRUE, width = 8, status = "info",
                        verbatimTextOutput("stats"),
                        ),
                ),
                fluidRow(
                    box(title = "Source Code", solidHeader = TRUE, width = 12, status = "warning", 
                        verbatimTextOutput("stats_source_code")
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
        if(!exists(input$stats.dataset)) return() #make sure upload exists
        var.opts2<-colnames(get(input$stats.dataset))
        updateSelectInput(session, "x_var", choices = var.opts)
        updateSelectInput(session, "y_var", choices = var.opts)
        updateSelectInput(session, "group", choices = var.opts)
        updateSelectInput(session, "var1", choices = var.opts2)
        updateSelectInput(session, "var2", choices = var.opts2)
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
    
    observeEvent (input$file1, {
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
        
        else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Barchart" && input$group_var == 1 ||
                input$class_x == "Continuous" && input$class_y == "Categorical" && input$both_plots == "Barchart" && input$group_var == 1) {
            
            ggplot(plot.obj$data,
                   aes_string(
                       x 		= plot.obj$x_var,
                       y         = plot.obj$y_var,
                       fill 	= plot.obj$group)) + 
                geom_bar(stat="identity", 
                         position = "dodge") + 
                .theme +
                labs(fill 	= input$group,
                    x 		= input$x_var,
                    y 		= input$y_var)
        } 
        
        else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Barchart" ||
                input$class_x == "Continuous" && input$class_y == "Categorical" && input$both_plots == "Barchart") {
            
            ggplot(plot.obj$data,
                   aes_string(
                       x 		= plot.obj$x_var,
                       y         = plot.obj$y_var)) + 
                geom_bar(stat="identity", 
                         position = "dodge") + 
                .theme +
                labs(x 		= input$x_var,
                     y 		= input$y_var)
        } 
        
        else if(input$class_x == "Categorical" && input$class_y == "Categorical") {
            ggplot(plot.obj$data,
                   aes_string(
                       x 		= plot.obj$x_var,
                       y         = plot.obj$y_var,
                       fill 	= plot.obj$y_var)) + 
                geom_mosaic(aes(x = product(!!sym(plot.obj$x_var)), fill = !!sym(plot.obj$y_var))) +
                .theme +
            labs(
                fill 	= input$y_var,
                x 		= input$x_var,
                y 		= input$y_var)
        } 
        
        else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Boxplot" ||
                input$class_x == "Continuous" && input$class_y == "Categorical" && input$both_plots == "Boxplot") {
            ggplot(plot.obj$data,
                   aes_string(
                       x 		= plot.obj$x_var,
                       y 		= plot.obj$y_var)) + 
                geom_boxplot(fill = "lightgrey") +
                .theme +
                labs(x 		= input$x_var,
                    y 		= input$y_var)
        }
        
        else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Stripchart" ||
                input$class_x == "Continuous" && input$class_y == "Categorical" && input$both_plots == "Stripchart") {
            ggplot(plot.obj$data,
                   aes_string(
                       x 		= plot.obj$x_var,
                       y 		= plot.obj$y_var)) + 
                geom_jitter(shape = 1, 
                            position = position_jitter(0.1)) + 
                stat_summary(fun.data = mean_cl_normal, 
                             geom = "errorbar", 
                             colour = "black", width = 0.05, 
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
        
        else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Barchart") {
"library(ggplot2)

ggplot(data, 
    aes(x = x_var, y = y_var, fill = group)) + 
    geom_bar(stat='identity', position = 'dodge', fill = 'lightgrey') +
    labs(x = x_var, y = y_var, fill = group) +
    axis.line = element_line(colour = 'gray', size = .75) +
    panel.background = element_blank() +
    plot.background = element_blank())"
            } 
        
        else if(input$class_x == "Categorical" && input$class_y == "Categorical") {
"library(ggplot2)
library(ggmosaic)

ggplot(data, 
    aes(x = x_var, y = y_var, fill = group)) + 
    geom_mosaic(aes(x = product(x_var, y_var), fill = group)) +
    labs(x = x_var, y = y_var, fill = group) +
    axis.line = element_line(colour = 'gray', size = .75) +
    panel.background = element_blank() +
    plot.background = element_blank())"
            } 
        
        else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Boxplot" ||
                input$class_x == "Continuous" && input$class_y == "Categorical" && input$both_plots == "Boxplot") {
"library(ggplot2)

ggplot(data, 
    aes(x = x_var, y = y_var)) + 
    geom_boxplot(fill = 'lightgrey') +
    labs(x = x_var, y = y_var) +
    axis.line = element_line(colour = 'gray', size = .75) +
    panel.background = element_blank() +
    plot.background = element_blank())"
            }
        
        else if(input$class_x == "Categorical" && input$class_y == "Continuous" && input$both_plots == "Stripchart" ||
                input$class_x == "Continuous" && input$class_y == "Categorical" && input$both_plots == "Stripchart") {
"library(ggplot2)

ggplot(data, 
    aes(x = x_var, y = y_var)) + 
    geom_jitter(shape = 1, position = position_jitter(0.1)) + 
    stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', colour = 'black', width = 0.05, position = position_nudge(x = 0.15)) +
    stat_summary(fun = mean, geom = 'point', colour = 'firebrick', size = 2, position = position_nudge(x = 0.15)) +
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
        req(input$stats.dataset, input$var1, input$class1, input$var2, input$class2)
        
        if(input$class1 == "Quantitative" && input$class2 == "Quantitative"){
            
            var1 <- get(input$stats.dataset)%>%
                select(input$var1, input$var2)%>%
                summarise(n = n(),
                          mean = mean(get(input$var1), na.rm = T), 
                          sd = sd(get(input$var1), na.rm = T),
                          median = median(get(input$var1), na.rm = T),
                          iqr = IQR(get(input$var1), na.rm = T))
            
            var2 <- get(input$stats.dataset)%>%
                select(input$var1, input$var2)%>%
                summarise(n = n(),
                          mean = mean(get(input$var2), na.rm = T), 
                          sd = sd(get(input$var2), na.rm = T),
                          median = median(get(input$var2), na.rm = T),
                          iqr = IQR(get(input$var2), na.rm = T))
            
        q <- full_join(var1, var2)
        q
            
        
        }
        
        else if(input$class1 == "Quantitative" && input$class2 == "Categorical"){
            
        q <- get(input$stats.dataset)%>%
            select(input$var1, input$var2)%>%
            group_by(!!sym(input$var2)) %>% 
            summarise(n = n(),
                      mean = mean(get(input$var1), na.rm = T), 
                      sd = sd(get(input$var1), na.rm = T),
                      median = median(get(input$var1), na.rm = T),
                      iqr = IQR(get(input$var1), na.rm = T))
        q
            
        
        }
        
        else if(input$class1 == "Categorical" && input$class2 == "Quantitative"){
            
            q <- get(input$stats.dataset)%>%
                select(input$var1, input$var2)%>%
                group_by(!!sym(input$var1)) %>% 
                summarise(n = n(),
                          mean = mean(get(input$var2), na.rm = T), 
                          sd = sd(get(input$var2), na.rm = T),
                          median = median(get(input$var2), na.rm = T),
                          iqr = IQR(get(input$var2), na.rm = T))
            q
            
        }
        
        else if(input$class1 == "Categorical" && input$class2 == "Categorical"){
          
        q <- select(get(input$stats.dataset), input$var1, input$var2)
        table(q)
        }
        
    })
    
    output$stats <- renderPrint({stats()})
        
    
    # showing source code for descriptive statistics table
    
    output$stats_source_code <- renderText({
        
        #suppress error messages
        req(input$stats.dataset, input$var1, input$class1, input$var2, input$class2)

        if(input$class1 == "Quantitative" && input$class2 == "Quantitative"){
            
"library(dplyr)

var1 <- data %>%
         summarise(n = n(),
                   mean = mean(var1), na.rm = T), 
                   sd = sd(var1), na.rm = T),
                   median = median(var1), na.rm = T),
                   iqr = IQR(var1), na.rm = T))
var2 <- data %>%
        summarise(n = n(),
                  mean = mean(var2), na.rm = T), 
                  sd = sd(var2), na.rm = T),
                  median = median(var2), na.rm = T),
                  iqr = IQR(var2), na.rm = T))
            
stats <- full_join(var1, var2)
stats"
            
            
        }
        
        else if(input$class1 == "Quantitative" && input$class2 == "Categorical"){
            
"library(dplyr)

stats <- data %>%
          group_by(var2) %>%
          summarise(n = n(),
                   mean = mean(var1), na.rm = T), 
                   sd = sd(var1), na.rm = T),
                   median = median(var1), na.rm = T),
                   iqr = IQR(var1), na.rm = T))
stats"
            
            
        }
        
        else if(input$class1 == "Categorical" && input$class2 == "Quantitative"){
            
"library(dplyr)

stats <- data %>%
          group_by(var1) %>%
          summarise(n = n(),
                   mean = mean(var2), na.rm = T), 
                   sd = sd(var2), na.rm = T),
                   median = median(var2), na.rm = T),
                   iqr = IQR(var2), na.rm = T))
stats"
            
        }
        
        else if(input$class1 == "Categorical" && input$class2 == "Categorical"){
            
"library(dplyr)

stats <- data %>%
         select(var1, var2)

table(stats)"
            
        }
        
    })
    
        
    
}

# shiny app --------------------------------------------------------------------

shinyApp(ui, server)





