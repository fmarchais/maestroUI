# Load required libraries
library(shiny)
library(shinydashboard)

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

library(shinycssloaders)
library(shinyWidgets)
library(htmltools)

library(timevis)
library(DT)
library(plotly)

Sys.setenv(LANG = "en")




###### A metttre dans les fonctions ####################

custom_theme <-  ggplot2::theme(
  # Background colors - cleaner with subtle blue tint
  plot.background = element_rect(fill = "#f8f9fa", color = NA),
  panel.background = element_rect(fill = "#fdfdfe", color = NA),
  
  # Much more discrete grid lines
  panel.grid.major = element_line(color = "#f0f2f5", linewidth = 0.3, linetype = "solid"),
  panel.grid.minor = element_blank(),  # Remove minor grid lines completely
  
  # Facet styling with blue/violet theme
  strip.background = element_rect(fill = "#3c8dbc", color = "#3c8dbc", linewidth = 0.5),
  strip.text = element_text(color = "#f0f2f5", face = "bold", size = 15),
  
  # Panel spacing
  panel.spacing.x = unit(10, "mm"),
  
  # Modern typography
  plot.title = element_text(color = "#323130", face = "bold", size = 18),
  axis.title = element_text(color = "#605e5c", face = "bold", size = 13),
  axis.text = element_text(color = "#605e5c", size = 13),
  
  # Legend styling
  legend.background = element_rect(fill = "#ffffff", color = "#edebe9"),
  legend.text = element_text(color = "#605e5c", size = 12),
  legend.title = element_text(color = "#323130", face = "bold", size = 14),
  
  # Border
  panel.border = element_rect(color = "#edebe9", fill = NA, size = 0.5),
  
  # Rotate x axis labels
  axis.text.x = element_text(angle = 90)
)





## GET HMS BREAKS ##########
get_hms_breaks <- function(y) {
  rng <- range(as.numeric(y))
  span <- diff(rng)
  if (span <= 3600) {
    # 1-minute interval
    as_hms(seq(floor(rng[1]), ceiling(rng[2]), by = 60))
  } else if (span <= 10800) {
    # 5-minute intervals
    as_hms(seq(floor(rng[1]), ceiling(rng[2]), by = 5 * 60))
  } else {
    # 1-hour intervals
    as_hms(seq(floor(rng[1]), ceiling(rng[2]), by = 3600))
  }
}












# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "maestroUI", titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Pipeline Logs", tabName = "runs", icon = icon("book-open")),
      menuItem("DAGs", tabName = "dags", icon = icon("diagram-project")),
      menuItem("Schedule", tabName = "schedule", icon = icon("clock"))
    ),
    br(),
    # Filters
    h4("Filters", style = "margin-left: 15px; color: white;"),
    div(style = "margin: 15px;",
        radioButtons("tz", "Timezone", 
                     choices = c("UTC","Local")
                     ),
        pickerInput("pipeline_filter", "Pipeline:",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE),
        
        ##### Sélectionner les status = conditionnels (pour les not_run)
        uiOutput("sidebar_inputs"),
        ############
        
        dateRangeInput("date_filter", "Date Range:",
                      start = Sys.Date() - 7,
                      end = Sys.Date())
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .status-success { background-color: #00a65a	!important; color: white !important; }
        .status-failed  { background-color: #dc3545 !important; color: white !important; }
        .status-not-run { background-color: #6c757d !important; color: white !important; }
        .status-warning { background-color: #ffc107 !important; color: black !important; }
        
        .metric-box {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 10px;
          text-align: center;
          margin: 10px;
        }
        
        .metric-value {
          font-size: 2.5em;
          font-weight: bold;
          margin-bottom: 5px;
        }
        
        .metric-label {
          font-size: 1.1em;
          opacity: 0.9;
        }
        
        .pipeline-card {
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin: 10px 0;
          padding: 15px;
          border-left: 4px solid #007bff;
        }
        
        .pipeline-name {
          font-size: 1.2em;
          font-weight: bold;
          margin-bottom: 8px;
          
        }
        
        .pipeline-info {
          font-size: 0.9em;
          color: #666;
        }

      "))
    ),
    
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
        fluidRow(
          column(3,
            div(class = "metric-box",
                div(class = "metric-value", textOutput("total_runs")),
                div(class = "metric-label", "Total Runs in the last 24 hours")
            )
          ),
          column(3,
            div(class = "metric-box",
                div(class = "metric-value", textOutput("success_rate")),
                div(class = "metric-label", "Success Rate in the last 24 hours")
            )
          ),
          column(3,
            div(class = "metric-box",
                div(class = "metric-value", textOutput("active_pipelines")),
                div(class = "metric-label", "Active Pipelines")
            )
          ),
          column(3,
            div(class = "metric-box",
                div(class = "metric-value", textOutput("last_run_time")),
                div(class = "metric-label", "Last Run")
            )
          )
        ),
        
        
        
        fluidRow(
          column(12,
                 box(title = "Last 10 Runs",
                     status = "primary", 
                     solidHeader = TRUE, 
                     width = NULL,
                     uiOutput("pipeline_cards")
                 ),
                 
                 box(title = "Pipeline Timeline", 
                     status = "primary", 
                     solidHeader = TRUE, 
                     width = NULL,
                     withSpinner(timevis::timevisOutput("timeviz"))
                 )
          )
        )
      ),
      
      # Pipeline Runs tab
      tabItem(tabName = "runs",
        fluidRow(
          column(12,
            box(title = "Pipeline Execution Logs",
                status = "primary", 
                solidHeader = TRUE,
                width = NULL,
                withSpinner(DT::dataTableOutput("runs_table"))
            )
          )
        )
      )
    )
  )
)






# Define Server
server <- function(input, output, session) {
  
  # Load and process data
  logs_data <- reactive({
    # Load the CSV file
    logs <- read.csv("logs.csv", stringsAsFactors = FALSE) %>%
      # logs nativement en UTC
      mutate_at( 
        c("pipeline_started", "pipeline_ended", "next_run", "date_invoked"),
        ~ as_datetime(., "UTC")
      )  %>%
      mutate_at(
        c("pipeline_started", "pipeline_ended", "next_run", "date_invoked"),
        ~ if(input$tz == "Local") {
          with_tz(., tz = Sys.timezone()) # local tz
        } else {
          . # stay UTC
        }
      )
    
    maestro <- read_delim(
        "maestro.log", 
        delim = "]", 
        col_names = c("pipe_name","type","event_time","output")
    ) %>%
      mutate_all(~ str_squish(str_remove_all(., "\\[|\\:"))) %>%
      # maestro.log encodé nativement en Local
      mutate(event_time = str_remove_all(event_time, "\\..*"),
             event_time = as_datetime(event_time, tz = Sys.timezone()),
             event_time = if(input$tz == "Local") {
               event_time # stay local
             } else { 
               with_tz(event_time, "UTC")
             }
      )
    
    #  les warn/info/erreurs ne se lancent pas tous sur le même timing (debut, fin)
    df <- logs %>% 
      left_join(maestro , by = c("pipe_name", "date_invoked"     = "event_time")) %>%
      left_join(maestro , by = c("pipe_name", "pipeline_started" = "event_time") ) %>%
      left_join(maestro , by = c("pipe_name", "pipeline_ended"   = "event_time")) 
    
    df2 <- df %>% 
      unite(mess_invoked, type.x, output.x, sep = " : ", na.rm = TRUE) %>%
      unite(mess_start, type.y, output.y, sep = " : ", na.rm = TRUE) %>%
      unite(mess_end, type, output, sep = " : ", na.rm = TRUE) 
    
    df3 <- df2 %>%
      mutate(row = row_number()) %>%       # keep identity to rejoin later
      pivot_longer(cols = starts_with("mess_"),
                   names_to = "when",
                   values_to = "msg") %>%
      mutate(
        msg = str_trim(msg),
        msg = na_if(msg, ""),          # turn empty strings into NA
        level = str_extract(msg, "^(INFO|WARNING|ERROR)"),     # extract level
        message = str_remove(msg, "^(INFO|WARNING|ERROR)\\s*:\\s*") # remove "INFO :"
      ) %>%
      select(-c(when, msg)) %>%
      unique() %>%
      complete(level = c("INFO","WARNING","ERROR")) %>%
      pivot_wider(names_from = level,
                  values_from = message,
                  values_fn = ~ paste(., collapse = " | "), # collapse if multiple per level
                  values_fill = NA) %>%
      filter(!is.na(pipe_name)) %>%
      arrange(row) %>%
      select(-row) %>%
      relocate(c("INFO","WARNING", "ERROR"), .before = next_run) 
    

    # Process the data
    df4 <- df3 %>%
      mutate(
        status = case_when(
          !invoked ~ "not_run",
          invoked & success ~ "success",
          invoked & !success ~ "failed",
          TRUE ~ "unknown"
        ),
        duration = ifelse(!is.na(pipeline_started) & !is.na(pipeline_ended),
                         as.numeric(difftime(pipeline_ended, pipeline_started, units = "secs")),
                         NA),
        duration_s = hms::as_hms(duration),
        run_date = as.Date(pipeline_started, format = "%Y-%m-%dT%H:%M:%SZ")
      )
    
    return(df4)
  })
  
  
  
  ############## UI 
  
  # Update pipeline filter choices
  output$sidebar_inputs <- renderUI({
    req(input$tabs) # ensure a tab is selected
    switch(input$tabs,
           dashboard = pickerInput("status_filter", "Status:",
                                   choices = list("Success" = "success", "Failed" = "failed"),
                                   multiple = TRUE,
                                   selected = c("success","failed")),
           runs = pickerInput("status_filter", "Status:",
                              choices = list("Success" = "success", "Failed" = "failed", "Not Run" = "not_run"),
                              multiple = TRUE,
                              selected = c("success","failed")),
           # Si autre tabItem non défini ici
           NULL
    )
  })
  
  
  
  observe({
    df <- logs_data()
    choices <- sort(unique(df$pipe_name))
    updatePickerInput(session, "pipeline_filter", 
                     choices = setNames(choices, choices),
                     selected = choices)
  })
  
  
  
  ######################## FIN UI
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Filtered data based on inputs
  filtered_data <- reactive({
    req(input$pipeline_filter)
    
    df <- logs_data()
    
    # Filter by pipeline
    if (!is.null(input$pipeline_filter) && length(input$pipeline_filter) > 0) {
      df <- df %>% filter(pipe_name %in% input$pipeline_filter)
    }
    
    # Filter by status
    df <- df %>% filter(status %in% input$status_filter)
    
    # Filter by date
    df <- df %>% 
      filter(
        between(as_date(date_invoked), input$date_filter[1] ,input$date_filter[2])
      )
    
    return(df)
  })
  
  
  
  
  
  
  # Dashboard metrics
  output$total_runs <- renderText({
    df <- filtered_data() %>%
      # keep last 24 hours
      filter(date_invoked >= (now() - days(1)) )
    
    sum(df$invoked, na.rm = TRUE)
  })
  
  output$success_rate <- renderText({
    df <- filtered_data() %>%
      # keep last 24 hours
      filter(date_invoked >= (now() - days(1)) )
    
    invoked_runs <- df[df$invoked == TRUE, ]
    if (nrow(invoked_runs) > 0) {
      rate <- round(sum(invoked_runs$success, na.rm = TRUE) / nrow(invoked_runs) * 100, 1)
      paste0(rate, "%")
    } else {
      "N/A"
    }
  })
  
  output$active_pipelines <- renderText({
    df <- filtered_data()
    length(unique(df$pipe_name))
  })
  
  output$last_run_time <- renderText({
    df <- filtered_data()
    last_run <- max(df$pipeline_started, na.rm = TRUE)
    if (!is.infinite(last_run)) {
      format(last_run, "%Y-%m-%d  %H:%M")
    } else {
      "N/A"
    }
  })
  
  

 
  
  
  # Pipeline cards
  output$pipeline_cards <- renderUI({
    df <- logs_data()
    
    # Get latest status for each pipeline
    latest_status <- df %>%
      group_by(pipe_name) %>%
      filter(
        if(
          # have started at least once
          any(!is.na(pipeline_started)) ){
          pipeline_started == max(pipeline_started, na.rm = TRUE)
            } else if( any(!is.na(date_invoked)) ){
              # have been invoked at least once
          date_invoked == max(date_invoked, na.rm = TRUE)
            } else {
              # haven't been invoked yet
              next_run == max(next_run, na.rm = TRUE)
            }
      ) %>% 
      distinct() %>%
      mutate(label_status = case_when(
        status == "success" ~ "Success",
        status == "failed" ~ "Failed",
        status == "not_run" ~ "Not Run Yet"
      ),
      status_color = case_when(
        status == "success" ~ "#00a65a",
        status == "failed"  ~ "#dc3545",
        status == "not-run" ~ "#6c757d",
        status == "warning" ~ "#ffc107"
      ),
      last_time = dplyr::coalesce(pipeline_started, date_invoked, next_run)
      ) %>%
      ungroup() %>%
      arrange(desc(last_time))
    
    latest_status <- latest_status %>% slice_head(n = 10)
    
    # split into rows of up to 5 items
    n <- nrow(latest_status)
    if (n == 0) {
      return(NULL)
    }
    groups <- split(seq_len(n), ceiling(seq_len(n) / 5))
    row_divs <- lapply(groups, function(idx) {
      items <- lapply(idx, function(i) {
        row <- latest_status[i, ]
        border_color <- row$status_color
        
        div(
          class = "pipeline-card",
          style = paste0(
            # made the left border a bit wider
            "border-left: 6px solid ", border_color, ";",
            "padding: 10px;",
            "background: #fff;",
            "border-radius: 4px;",
            "box-shadow: 0 1px 2px rgba(0,0,0,0.04);",
            # fixed basis and no grow so cards don't stretch when fewer items are present
            "flex: 0 0 18%;",
            "min-width: 140px;"
          ),
          div(class = "pipeline-name", style = "font-weight: 600; margin-bottom: 6px;", row$pipe_name),
          div(
            class = "pipeline-info",
            if (!is.na(row$pipeline_started)) {
              paste("Last run:", format(row$pipeline_started, "%Y-%m-%d %H:%M"))
            } else {
              paste("Next run:", format(row$next_run, "%Y-%m-%d %H:%M"))
            },
            # errors and warnings
            if (!is.na(row$errors) && row$errors > 0) {
              tags$span(style = "color: #dc3545 ; margin-left: 10px;", paste("Errors:", row$errors))
              },
            if (!is.na(row$warnings) && row$warnings > 0){ 
              tags$span(style = "color: #ffc107; margin-left: 10px;", paste("Warnings:", row$warnings))
              }
          )
        )
      })
      
      # if this row has fewer than 5 items, center them; otherwise left-align
      row_justify <- if (length(idx) < 5) "left" else "flex-start"
      row_style <- paste0("display: flex; gap: 12px; margin-bottom: 12px; align-items: stretch; justify-content: ", row_justify, ";")
      
      div(
        class = "pipeline-row",
        style = row_style,
        do.call(tagList, items)
      )
    })
    
    do.call(tagList, row_divs)
    
  })
  
  # Runs table
  output$runs_table <- DT::renderDataTable({
    df <- filtered_data() %>%
      select(pipe_name, status, date_invoked, pipeline_started, pipeline_ended, 
             duration = duration_s,
             INFO, WARNING, ERROR, next_run
             ) %>%
      mutate(
        pipeline_started = format(pipeline_started, "%Y-%m-%d %H:%M:%S"),
        pipeline_ended = format(pipeline_ended, "%Y-%m-%d %H:%M:%S"),
        next_run = format(next_run, "%Y-%m-%d %H:%M:%S"),
        date_invoked = format(date_invoked, "%Y-%m-%d %H:%M:%S")
      ) %>%
      arrange(desc(date_invoked), pipe_name)
    
    DT::datatable(df, 
                  options = list(pageLength = 25, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle("status",
                      backgroundColor = DT::styleEqual(
                        c("success", "failed", "not_run"),
                        c("#00a65a", "#dc3545", "#6c757d")
                      ),
                      color = "white")
  })
  
  
  
  
  
  output$timeviz <- renderTimevis({

    df_time <- filtered_data() %>%
      filter(!status == "not_run") %>%
      mutate(
        start = pipeline_started,
        end = pipeline_ended,
        type = ifelse(
          !is.na(pipeline_ended),
          "range",
          "point"
        ),
        content = duration_s,
        id = 1:n(),
        pipe_name = as.factor(pipe_name),
        group = as.numeric(pipe_name),
        style = ifelse(
          status == "success",
          "background-color: #4CAF50; border-color: #4CAF50",   # green
          "background-color: #F44336; border-color: #F44336;"   # red
        ), 
        title = 
          paste0(
            "started on : ", pipeline_started, " -", input$tz,
            "- for ", duration_s)
      ) %>%
      select(
        id,
        content,
        start,
        end,
        group,
        pipe_name,
        style,
        title
      )
    
    
    groups <- data.frame(
      id = as.numeric(unique(df_time$pipe_name)),
      content = unique(df_time$pipe_name)
    ) %>%
      arrange(content)
    
    diff_timezone <- difftime( force_tz(now(),"UTC"), now() , units = "hours")  %>% as.numeric()
    
    timevis(df_time, 
            groups = groups,
            timezone = if(input$tz == "Local"){
              diff_timezone
            } else if(input$tz == "UTC"){
              0
            } 
    ) %>%
      setWindow(Sys.Date() -7, Sys.Date() +1)
    
    
  })
  
  
  
  
  
 
}

# Run the application
shinyApp(ui = ui, server = server)