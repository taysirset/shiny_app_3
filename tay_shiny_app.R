library(shiny)
library(plotly)  # Load the Plotly library to generate a cool plot

# Use a cool Plotly template
cool_template <- ggplot2::theme_minimal() + theme(
  plot.background = element_rect(fill = "black"),
  panel.background = element_rect(fill = "black"),
  axis.line = element_line(color = "white"),
  axis.text = element_text(color = "white"),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white"),
  strip.text = element_text(color = "white")
)

# Define UI for application that reads and displays CSV data
ui <- fluidPage(
  # Application title
  titlePanel("Your Barbie CSV Data Viewer"),

  # Sidebar with a file input for uploading CSV file, a slider input for number of bins, and buttons for modeling the data and exporting plots
  sidebarLayout(
    sidebarPanel(
      fileInput("csvFile", "Choose a CSV file"),
      sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
      actionButton("modelButton", "Model Data (Linear Model)"),
      downloadButton("exportButton", "Export Plots")
    ),

    # Show a table of the CSV contents and a scatter plot of the data
    mainPanel(
      tableOutput("contents"),
      plotlyOutput("scatterPlot"),  # Use plotlyOutput instead of plotOutput for Plotly plots - hopefully generates a cool plot
      plotlyOutput("linearModelPlot"),  # Use plotlyOutput instead of plotOutput for Plotly plots - hopefully generates a cool plot
      textOutput("modelSummary")
    )
  )
)

# Define server logic required to read and display the CSV data
server <- function(input, output) {
  data <- reactive({
    req(input$csvFile)  # Require the file input to be present before reading
    read.csv(input$csvFile$datapath)
  })

  # Display the CSV data in a table
  output$contents <- renderTable({
    data()
  })

  # Generate the scatter plot based on the uploaded CSV data
  output$scatterPlot <- renderPlotly({
    req(input$csvFile)  # Require the file input to be present before generating the plot

    # Extract the columns of interest (assuming they are the first two columns)
    x <- data()[, 1]
    y <- data()[, 2]

    # Create the scatter plot using plotly
    p <- plot_ly(x = x, y = y, type = "scatter", mode = "markers", marker = list(color = 'deeppink')) %>%
      layout(title = "Barbie's Scatter Plot", xaxis = list(title = "X-axis"), yaxis = list(title = "Y-axis")) %>%
      layout(template = cool_template)  # Apply the cool template

    return(p)
  })

  # Linear model when the "Model Data" button is clicked
  model <- eventReactive(input$modelButton, {
    req(input$csvFile)  # Require the file input to be present before modeling

    # Extract the columns of interest (assuming they are the first two columns)
    x <- data()[, 1]
    y <- data()[, 2]

    # Perform linear regression
    lm_result <- lm(y ~ x)

    # Return the linear model
    lm_result
  })

  # Display the linear model summary
  output$modelSummary <- renderText({
    req(input$modelButton)  # Require the model to be created before displaying the summary

    lm_result <- model()

    # Extract slope, intercept, and correlation coefficient
    slope <- coef(lm_result)[2]
    intercept <- coef(lm_result)[1]
    cor_coef <- cor(data()[, 1], data()[, 2])

    paste("Slope:", slope, "\n",
          "Intercept:", intercept, "\n",
          "Correlation Coefficient:", cor_coef, "\n")
  })

  # Plot the linear model overlayed on the original data
  output$linearModelPlot <- renderPlotly({
    req(input$modelButton)  # Require the model to be created before plotting

    # Extract the columns of interest (assuming they are the first two columns)
    x <- data()[, 1]
    y <- data()[, 2]

    # Create the scatter plot using plotly
    scatter_plot <- plot_ly(x = x, y = y, type = "scatter", mode = "markers", marker = list(color = 'deeppink')) %>%
      layout(title = "Barbie's Scatter Plot with Linear Model Included", xaxis = list(title = "X-axis"), yaxis = list(title = "Y-axis")) %>%
      layout(template = cool_template)  # Apply the cool template

    # Extract the coefficients of the linear model
    lm_result <- model()
    slope <- coef(lm_result)[2]
    intercept <- coef(lm_result)[1]

    # Add the linear model line to the scatter plot
    model_line <- x * slope + intercept
    scatter_plot <- scatter_plot %>% add_lines(x = x, y = model_line, line = list(color = 'pink', width = 2))

    return(scatter_plot)
  })

  # Export the plots as PNG files when the "Export Plots" button is clicked
  output$exportButton <- downloadHandler(
    filename = function() {
      paste("plots_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create a temporary directory to save the plots
      temp_dir <- tempdir()

      # Save the scatter plot
      scatter_plot <- plotly::plotly_build(output$scatterPlot)
      scatter_path <- file.path(temp_dir, "scatter_plot.png")
      plotly::export(scatter_plot, file = scatter_path)

      # Save the linear model plot
      linear_model_plot <- plotly::plotly_build(output$linearModelPlot)
      linear_model_path <- file.path(temp_dir, "linear_model_plot.png")
      plotly::export(linear_model_plot, file = linear_model_path)

      # Create the ZIP file and add the plots to it
      zip_file <- file.path(temp_dir, "plots.zip")
      zip(zip_file, files = c(scatter_path, linear_model_path))

      # Copy the ZIP file to the specified location
      file.copy(zip_file, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
