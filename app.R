library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(shinycssloaders)
library(shinyWidgets)

## UI
ui <- fluidPage(
  div(style = 'margin-top: 5px;',
  tabsetPanel(
    tabPanel("Data summary",
             titlePanel("BirdNETprocess - a Shiny app for visualising birdNET data"),
  fluidRow(
    column(4,
           "File upload",
           fileInput('file', 'Insert birdnet .txt files', multiple = T, accept = '.txt')
           ),
    column(4,
           "Confidence level",
           numericInput('confidence', 'Minimum confidence level', value = 0, min = 0, max = 1),
           actionButton('update', 'Update')
           ),
    style = 'margin-bottom: 30px;'),
  fluidRow(
    column(8,
            dataTableOutput('df') %>% withSpinner()
  ),
  style = 'margin-bottom: 30px;'),
  fluidRow(
    column(4,
           tableOutput('summary_stats')
  ),
  column(8,
         plotOutput('quickcalls')))
    ),
  tabPanel("Recordings by time",
    fluidRow(
      column(6,
           plotOutput('quicktime')),
      column(6,
           plotOutput('quickconfidence'))
      ),
    fluidRow(
      column(6),
      column(6,
             sliderInput("bw_quickconfidence", "Bandwidth of KDE", min = 0, max = 1, step = 0.05, value = 0.75))
    )
  ),
  tabPanel("Species over time",
           fluidRow(
             div(style = 'margin-left: 5px; margin-top: 5px;',
             uiOutput('bird_choices')
           ),
           fluidRow(
             column(6,
                    plotOutput('quicktime_byspecies')),
             column(6,
                    plotOutput('birdtime'))
           ),
           fluidRow(
             column(6),
             column(6,
                    uiOutput('kde_birdtime_slider')))
           )
           )
)))

## server
server <- function(input, output, session) {

  # reactive expression for combining uploaded .txt files and creating single df
  bird_data <- reactive({
    # require the input file so code waits for file upload
    req(input$file)

    # get file names and file paths - cant just use file paths, because
    # when uploaded via the app, temp file names dont have YYMMMDD format
    file_names <- input$file$name
    file_paths <- input$file$datapath

    # appropriated 'read_birdnet_file' in light of the above
    read_files <- function(file_path, file_name){

      #df <- readr::read_delim(file_path, delim = "\t", show_col_types = FALSE)
      # fread much faster
      df <- data.table::fread(file_path, sep = "\t", showProgress = FALSE)

      begin_col <- "Begin Time (s)"
      if (!begin_col %in% names(df)) {
        stop("Could not find 'Begin Time (s)' column in the BirdNET file.")
      }

      start_time <- parse_birdnet_filename_datetime(file_name)

      # add columns
      df <- df %>%
        dplyr::mutate(
          file_name             = file_name,
          start_time           = start_time,
          recording_window_time = start_time + .data[[begin_col]]
        )

      return(df)
    }

    combined_df <- purrr::map2_dfr(file_paths, file_names, read_files) %>%
      as.data.frame()

    # ta da
    combined_df
  })

  confidence_value <- eventReactive(input$update, {
    input$confidence
  })

  # filter data according to input confidence
  bird_data_confidence_filtered <- eventReactive(input$update, {
    bird_data() %>% filter(Confidence >= confidence_value() & `Common Name` != 'nocall')
    })


  # combined dataframe - only show important columns for brevity
  output$df <- renderDataTable(bird_data_confidence_filtered()[c(2,3,8,9,10,15)],
                               rownames = F,
                               options = list(pageLength = 5,
                                              autoWidth = TRUE),
                               class = 'compact')

  # now run quickstats on uploaded files
  output$tablesummary <- renderTable({
    req(bird_data_confidence_filtered())
    quickstats(bird_data_confidence_filtered())
  })

  # quickstats but title appears with it
  output$summary_stats <- renderUI({
    req(bird_data_confidence_filtered())
    tagList(
      div(h4('Data Summary'), style = "text:align: centre;"),
      tableOutput('tablesummary')
    )
  })

  # quickcalls plot
  output$quickcalls <- renderPlot({
    req(bird_data_confidence_filtered())
    req(confidence_value())
    quickcalls(bird_data_confidence_filtered())
  },
  res = 96)

  # quicktime for recordings by time tab
  output$quicktime <- renderPlot({
    req(bird_data_confidence_filtered())
    req(confidence_value())
    quicktime(bird_data_confidence_filtered())
  })

  # quickconfidence for recordings by time tab
  output$quickconfidence <- renderPlot({
    req(bird_data_confidence_filtered())
    req(confidence_value())
    quickconfidence(bird_data_confidence_filtered(),
                    bw = input$bw_quickconfidence)
  })

  # bird choices made by user on species over time tab
  output$bird_choices <- renderUI({
    req(bird_data_confidence_filtered())
    req(confidence_value())
    selectizeInput("chosen_birds",
                   "Bird species (max 6)",
                   choices = unique(bird_data_confidence_filtered()$`Common Name`),
                   multiple = T,
                   width = '100%',
                   options = list(maxItems = 6))
  })

  # kde slider on species over time tab
  output$kde_birdtime_slider <- renderUI({
    req(input$chosen_birds)
    sliderInput("bw_birdtime", "Bandwidth of KDE", min = 0, max = 1, step = 0.05, value = 0.75)
  })

  # quicktime by species for species over time tab
  output$quicktime_byspecies <- renderPlot({
    req(bird_data_confidence_filtered())
    req(input$chosen_birds)
    quicktime(bird_data_confidence_filtered(),
              bird.names = input$chosen_birds)
  })

  # birdtime for species over time tab
  output$birdtime <- renderPlot({
    req(bird_data_confidence_filtered())
    req(input$chosen_birds)
    birdtime(bird_data_confidence_filtered(),
             bird.names = input$chosen_birds,
             bw = input$bw_birdtime)
  })
}

#options(shiny.maxRequestSize = 1024 * 1024^2) #for increasing upload limit
shinyApp(ui, server)
