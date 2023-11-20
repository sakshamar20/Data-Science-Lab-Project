library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# Load Data
data <- read.csv("Basket Data.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "University Admissions Data Explorer", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("UG College", tabName = "ug_college", icon = icon("university"),
               menuSubItem("Analysis", tabName = "graph1"),
               menuSubItem("UG Major Accept v/s Reject", tabName = "graph2")
      ),
      
      menuItem("Universities", tabName = "universities", icon = icon("university"),
               menuSubItem("Admit v/s Reject", tabName = "graph3")
      ),
      
      menuItem("UG Major v/s Target Major", tabName = "ug_major", icon = icon("graduation-cap"),
               menuSubItem("Plot", tabName = "graph4")
      ),
      menuItem("Data Table", tabName = "overall_data", icon = icon("university"),
               menuSubItem("Table", tabName = "table")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Graph 1
      tabItem(tabName = "graph1",
              titlePanel("UG College Data"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("ug_college_1", "Choose UG College", choices = unique(data$UG.COLLEGE), selected = "IIT Kanpur"),
                  selectInput("x_axis_1", "Choose X - Axis", choices = c("CGPA", "PAPERS", "WORK.EXPERIENCE.M.", "TOEFL", "GRE.TOTAL."), selected = "CGPA"),
                  selectInput("y_axis_1", "Choose Y - Axis", choices = c("CGPA", "PAPERS", "WORK.EXPERIENCE.M.", "TOEFL", "GRE.TOTAL."), selected = "TOEFL")
                ),
                
                mainPanel(
                  plotlyOutput("plot_1"),
                  dataTableOutput("data_table_1")
                )
              )
      ),
      
      # Graph 2
      tabItem(tabName = "graph2",
              titlePanel("University Admissions Data"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("ug_college_2", "Select UG College", choices = unique(data$UG.COLLEGE), selected = "IIT Kanpur"),
                  selectInput("parameter_2", "Select Parameter", choices = c("CGPA", "GRE.TOTAL.", "TOEFL")),
                  uiOutput("slider_2"),
                  selectInput("ug_major_2", "Select UG Major", choices = unique(data$UG.MAJOR), selected = c("Computer Science", "Mechanical Engineering", "Chemical Engineering", "Civil Engineering", "Electrical Engineering"), multiple = TRUE)
                ),
                
                mainPanel(
                  plotlyOutput("plot_2"),
                  dataTableOutput("table_data_2")
                )
              )
      ),
      
      # Graph 3
      tabItem(tabName = "graph3",
              titlePanel("University Admissions Analysis"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("university_3", "Select University", unique(data$`UNIVERSITY.NAME`)),
                  selectInput("score_3", "Select Score Type", c("GRE.TOTAL.", "TOEFL", "PAPERS", "WORK.EXPERIENCE.M.", "CGPA"), selected = "CGPA"),
                  uiOutput("slider_3")
                ),
                
                mainPanel(
                  plotlyOutput("plot_3"),
                  dataTableOutput("data_table_3")
                )
              )
      ),
      
      # Graph 4
      tabItem(tabName = "graph4",
              titlePanel("University Admissions Data"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("ug_major_4", "Select UG MAJOR", choices = unique(data$UG.MAJOR)),
                  selectInput("x_axis_4", "Select X - Axis", choices = c("CGPA", "GRE.TOTAL.", "TOEFL", "WORK.EXPERIENCE.M.", "PAPERS"))
                ),
                
                mainPanel(
                  plotlyOutput("plot_4"),
                  dataTableOutput("data_table_4")
                )
              )
      ),
      
      # Table
      tabItem(tabName = "table",
              titlePanel("University Admissions Data Table"),
              dataTableOutput("table_5")
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Graph 1
  
  # Filter Data according to chosen Parameters and make it Reactive
  filtered_data_1 <- reactive({
    data %>%
      filter(UG.COLLEGE == input$ug_college_1)
  })
  
  # Plot Graph of plot_1
  output$plot_1 <- renderPlotly({
    plot_data <- filtered_data_1()
    
    color_map <- ifelse(plot_data$STATUS == "Admit", "Accepted", "Rejected")
    
    fig <- plot_ly(data = plot_data, x = ~get(input$x_axis_1), y = ~get(input$y_axis_1),
                   color = color_map, colors = c("#4ebd48", "#d6352f"),
                   hoverinfo = "text",
                   text = ~paste("University: ", UNIVERSITY.NAME,
                                 "<br>STATUS: ", STATUS,
                                 "<br>", input$x_axis_1, ": ", get(input$x_axis_1),
                                 "<br>", input$y_axis_1, ": ", get(input$y_axis_1)),
                   mode = "markers")
    
    fig <- fig %>% layout(title = paste(input$x_axis_1, " v/s ", input$y_axis_1),
                          xaxis = list(title = input$x_axis_1),
                          yaxis = list(title = input$y_axis_1))
    
    fig
  })
  
  # Print Filtered Data Table
  output$data_table_1 <- renderDataTable({
    datatable(
      filtered_data_1(),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  
  # Graph 2
  
  # Filter Data according to chosen Parameters and make it Reactive
  filtered_data_2 <- reactive({
    data %>%
      filter(UG.COLLEGE == input$ug_college_2, get(input$parameter_2) >= input$slider_2, UG.MAJOR %in% input$ug_major_2)
  })
  
  # Slider values according to chosen option
  output$slider_2 <- renderUI({
    if (input$parameter_2 == "CGPA") {
      sliderInput("slider_2", paste("Select Threshold for", input$parameter_2),
                  min = 5, max = 10, value = 6, step = 1, round = 1)
    } else if (input$parameter_2 == "TOEFL") {
      sliderInput("slider_2", paste("Select Threshold for", input$parameter_2),
                  min = 80, max = 120, value = 100, step = 1, round = 1)
    } else if (input$parameter_2 == "GRE.TOTAL.") {
      sliderInput("slider_2", paste("Select Threshold for", input$parameter_2),
                  min = 300, max = 350, value = 320, step = 1, round = 1)
    }
  })
  
  # Plot Graph of plot_2
  output$plot_2 <- renderPlotly({
    ggplotly(
      ggplot(filtered_data_2(), aes_string(x = "UG.MAJOR", fill = "STATUS")) +
        geom_bar() +
        labs(title = "Admit v/s Reject Based on UG Major", x = "UG.MAJOR", y = "Count") +
        scale_fill_manual(values = c("Admit" = "#4ebd48", "Reject" = "#d6352f")) +  # Specify colors
      
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    )
  })
  
  # Print Filtered Data Table
  output$table_data_2 <- renderDataTable({
    datatable(
      filtered_data_2(),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  
  # Graph 3
  
  # Filter Data according to chosen Parameters and make it Reactive
  filtered_data_3 <- reactive({
    score_col_3 <- switch(input$score_3,
                          "GRE.TOTAL." = "GRE.TOTAL.",
                          "TOEFL" = "TOEFL",
                          "CGPA" = "CGPA",
                          "PAPERS" = "PAPERS",
                          "WORK.EXPERIENCE.M." = "WORK.EXPERIENCE.M.")
    
    filtered_3 <- data %>%
      filter(`UNIVERSITY.NAME` == input$university_3, !!as.name(score_col_3) >= input$score_threshold_3)
    
    return(filtered_3)
  })
  
  # Slider values according to chosen option
  output$slider_3 <- renderUI({
    if (input$score_3 == "TOEFL") {
      sliderInput("score_threshold_3", "Select Threshold", min = 80, max = 120, value = 100, step = 1)
    } else if (input$score_3 == "GRE.TOTAL.") {
      sliderInput("score_threshold_3", "Select Threshold", min = 300, max = 340, value = 320, step = 1)
    } else if (input$score_3 == "PAPERS") {
      sliderInput("score_threshold_3", "Select Threshold", min = 0, max = 4, value = 0, step = 1)
    } else if (input$score_3 == "WORK.EXPERIENCE.M.") {
      sliderInput("score_threshold_3", "Select Threshold", min = 0, max = 80, value = 0, step = 1)
    } else if (input$score_3 == "CGPA") {
      sliderInput("score_threshold_3", "Select Threshold", min = 5, max = 10, value = 6, step = 1)
    }
  })
  
  # Plot Graph of plot_3
  output$plot_3 <- renderPlotly({
    admissions_data <- filtered_data_3() %>%
      group_by(STATUS) %>%
      summarise(count = n())
    
    pie_chart_3 <- plot_ly(labels = ~admissions_data$STATUS, values = ~admissions_data$count, type = "pie", marker = list(colors = c(input$color_admit, input$color_reject)))
    
    pie_chart_3 %>% layout(title = paste("Admissions Pie Chart for", input$university_3))
  })
  
  # Print Filtered Data Table
  output$data_table_3 <- renderDataTable({
    datatable(
      filtered_data_3(),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  
  # Graph 4
  
  # Filter Data according to chosen Parameters and make it Reactive
  filtered_data_4 <- reactive({
    data %>%
      filter(UG.MAJOR == input$ug_major_4)
  })
  
  # Plot Graph of plot_4
  output$plot_4 <- renderPlotly({
    plot_ly(filtered_data_4(), x = ~get(input$x_axis_4), y = ~TARGET.MAJOR, color = ~STATUS, type = "scatter", mode = "markers",
            text = ~paste("University: ", UNIVERSITY.NAME,
                          "<br>UG College: ", UG.COLLEGE),
            colors = c("#4ebd48", "#d6352f")) %>%
      layout(title = paste("TARGET.MAJOR vs", input$x_axis_4),
             xaxis = list(title = input$x_axis_4),
             yaxis = list(title = "TARGET.MAJOR"))
  })
  
  # Print Filtered Data Table
  output$data_table_4 <- renderDataTable({
    datatable(
      filtered_data_4(),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  
  # Table
  
  output$table_5 <- renderDataTable({
    datatable(
      data,
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

