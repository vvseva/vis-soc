library(shiny)
library(bslib)
library(logolink)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(geomtextpath)
library(xml2)
library(magrittr) # Added for extract2()
library(shinycssloaders) # Added for spinners

ui <- fluidPage(
  theme = bs_theme(version = 5),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("Logolink Experiment Interface"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("model_file", "Upload .nlogox Model", accept = c(".nlogox", ".xml")),
      
      div(class = "control-section",
          h4("Model Parameters"),
          uiOutput("dynamic_controls")
      ),
      
      div(class = "control-section",
          h4("Experiment Settings"),
          sliderInput("num_runs", "Number of Runs:", min = 1, max = 50, value = 1, step = 1),
          uiOutput("metric_controls"), # New UI element for parsed metrics
          actionButton("run_btn", "Run Experiment", class = "sys1-btn", width = "100%")
      )
    ),
    
    mainPanel(
      div(class = "plot-container",
          h4("NetLogo World"),
          withSpinner(plotOutput("world_plot", height = "400px"), type = 8, color = "#000000")
      ),
      div(class = "plot-container",
          h4("Segregation Indices Over Time"),
          withSpinner(plotOutput("spaghetti_plot", height = "500px"), type = 8, color = "#000000")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to hold and manage the uploaded model file path
  model_path <- reactive({
    req(input$model_file)
    tmp_file <- tempfile(fileext = ".nlogox")
    file.copy(input$model_file$datapath, tmp_file)
    tmp_file
  })
  
  # Parse the .nlogox XML file to dynamically generate UI sliders, choosers, and metrics
  observeEvent(model_path(), {
    tryCatch({
      doc <- read_xml(model_path())
      
      sliders <- xml_find_all(doc, ".//slider")
      choosers <- xml_find_all(doc, ".//chooser")
      
      ui_elems <- list()
      
      # Process sliders
      for (s in sliders) {
        name <- xml_attr(s, "variable")
        if (is.na(name)) name <- xml_attr(s, "display")
        
        min_val <- as.numeric(xml_attr(s, "min"))
        max_val <- as.numeric(xml_attr(s, "max"))
        val <- as.numeric(xml_attr(s, "default"))
        step_val <- as.numeric(xml_attr(s, "step"))
        
        if (is.na(min_val)) min_val <- 0
        if (is.na(max_val)) max_val <- 100
        if (is.na(val)) val <- min_val
        if (is.na(step_val)) step_val <- NULL
        
        if (!is.na(name)) {
          ui_elems[[name]] <- sliderInput(
            inputId = paste0("var_", name), 
            label = name, 
            min = min_val, 
            max = max_val, 
            value = val,
            step = step_val
          )
        }
      }
      
      # Process choosers
      for (c in choosers) {
        name <- xml_attr(c, "variable")
        if (is.na(name)) name <- xml_attr(c, "display")
        
        choice_nodes <- xml_find_all(c, "./choice")
        choices <- xml_attr(choice_nodes, "value")
        
        if (!is.na(name) && length(choices) > 0) {
          # Block "real-norrkoping" road scenario
          if (name == "road-scenario") {
            choices <- setdiff(choices, "real-norrkoping")
            if (length(choices) == 0) choices <- c("empty")
          }
          
          ui_elems[[name]] <- selectInput(
            inputId = paste0("var_", name), 
            label = name, 
            choices = choices
          )
        }
      }
      
      if (length(ui_elems) == 0) {
        ui_elems <- list(helpText("No valid sliders or choosers found in the uploaded file."))
      }
      
      output$dynamic_controls <- renderUI({ tagList(ui_elems) })
      
      # Process metrics from plot updates (e.g., "<update>plot index-exposure</update>")
      plot_updates <- xml_find_all(doc, ".//plot//pen//update")
      plot_texts <- xml_text(plot_updates)
      
      # Extract strings starting with "plot "
      parsed_metrics <- plot_texts[str_starts(plot_texts, "plot\\s")]
      parsed_metrics <- str_remove(parsed_metrics, "plot\\s+") %>% str_trim()
      parsed_metrics <- unique(parsed_metrics)
      
      # Render multiple select input for parsed metrics
      if (length(parsed_metrics) > 0) {
        output$metric_controls <- renderUI({
          selectInput("selected_metrics", "Select Metrics:", 
                      choices = parsed_metrics, selected = parsed_metrics, multiple = TRUE)
        })
      } else {
        output$metric_controls <- renderUI({ helpText("No plotting metrics detected in XML.") })
      }
      
    }, error = function(e) {
      output$dynamic_controls <- renderUI({ 
        helpText(paste("Could not automatically parse GUI elements:", e$message)) 
      })
    })
  })
  
  # Run the experiment using logolink
  experiment_results <- eventReactive(input$run_btn, {
    req(model_path())
    
    # Check if metrics are available and selected
    metrics_to_collect <- input$selected_metrics
    if (is.null(metrics_to_collect)) {
      metrics_to_collect <- c("index-exposure", "index-dissimilarity") # Safe fallback
    }
    
    # Append required patch data metrics to ensure the "lists" element is populated
    metrics_to_collect <- c(
      metrics_to_collect,
      '[pxcor] of patches',
      '[pycor] of patches',
      '[pcolor] of patches'
    )
    
    # Isolate dynamic inputs created from the model and pass them as constants
    input_names <- names(input)
    var_inputs <- input_names[str_starts(input_names, "var_")]
    
    exp_vars <- list()
    for (v in var_inputs) {
      original_name <- str_remove(v, "var_")
      val <- input[[v]]
      
      if (!is.null(val) && length(val) > 0 && any(val != "")) {
        
        # Backend block: double-check we don't pass real-norrkoping
        if (original_name == "road-scenario" && any(val == "real-norrkoping")) {
          val <- "empty"
        }
        
        # 1. Parse booleans (true/false strings from NetLogo choosers into R logicals like T or F)
        if (is.character(val) && length(val) == 1 && tolower(val) %in% c("true", "false", "t", "f")) {
          exp_vars[[original_name]] <- as.logical(toupper(val))
        } 
        # 2. Parse numerics cleanly (into double/numeric vectors)
        else if (suppressWarnings(all(!is.na(as.numeric(val))))) {
          exp_vars[[original_name]] <- as.numeric(val)
        } 
        # 3. Parse categorical strings strictly as characters (e.g., 'empty')
        else {
          exp_vars[[original_name]] <- val
        }
      }
    }
    
    # Add explicit console logging to see exactly what is being sent to logolink
    message("--- STARTING LOGOLINK EXPERIMENT ---")
    message("Model Path: ", model_path())
    message("Metrics to collect: ", paste(metrics_to_collect, collapse = ", "))
    message("Constants list:")
    str(exp_vars)
    
    # 1. Create the experiment configuration safely
    setup_file <- tryCatch({
      logolink::create_experiment(
        name = "Venue Segregation Model Analysis",
        repetitions = input$num_runs,
        run_metrics_every_step = TRUE,
        setup = "setup",
        go = "go",
        time_limit = 200,
        metrics = metrics_to_collect,
        constants = exp_vars
      )
    }, error = function(e) {
      err_msg <- paste("create_experiment() failed:", e$message)
      message(err_msg)
      showNotification(err_msg, type = "error", duration = NULL)
      return(NULL)
    })
    
    if (is.null(setup_file)) return(NULL)
    
    # 2. Run the experiment requesting both table and lists
    results <- tryCatch({
      message("Running experiment...")
      model_path() |>
        logolink::run_experiment(
          setup_file = setup_file,
          output = c("table", "lists")
        )
    }, error = function(e) {
      err_msg <- paste("run_experiment() failed:", e$message)
      message(err_msg)
      showNotification(err_msg, type = "error", duration = NULL)
      return(NULL)
    })
    
    if (is.null(results)) return(NULL)
    message("Experiment finished. Extracting results...")
    
    # 3. Extract robustly following the user's explicit pipeline logic
    extracted_data <- tryCatch({
      message("Results class: ", paste(class(results), collapse = ", "))
      message("Results names: ", paste(names(results), collapse = ", "))
      
      # Table extraction
      res_table <- tryCatch({
        results |> magrittr::extract2("table")
      }, error = function(e) {
        message("Table extraction error: ", e$message)
        # Fallback if the table itself was just returned as a flat data frame
        if (is.data.frame(results)) return(results)
        NULL
      })
      
      # Lists extraction and color mutation
      res_lists <- tryCatch({
        l_data <- results |> magrittr::extract2("lists")
        
        if (is.null(l_data)) {
          message("lists extraction returned NULL.")
          NULL
        } else {
          l_data |> 
            mutate(
              across(
                .cols = matches("^pcolor_of_patches|^color_of_"),
                .fns = parse_netlogo_color
              )
            )
        }
      }, error = function(e) {
        message("Lists extraction/mutation error: ", e$message)
        NULL
      })
      
      list(table = res_table, lists = res_lists)
      
    }, error = function(e) {
      err_msg <- paste("Extraction block critically failed:", e$message)
      message(err_msg)
      showNotification(err_msg, type = "error", duration = NULL)
      return(NULL)
    })
    
    message("--- LOGOLINK EXPERIMENT COMPLETE ---")
    return(extracted_data)
  })
  
  # Render the NetLogo world raster plot based on actual model outputs
  output$world_plot <- renderPlot({
    req(input$run_btn)
    res <- experiment_results()
    plot_data <- res$lists
    
    validate(
      need(!is.null(plot_data), "The experiment did not return lists data to visualize the world. Check R console for specific extraction errors."),
      need(nrow(plot_data) > 0, "Agent lists data was returned empty from the model.")
    )
    
    # Visualize the actual returned NetLogo world using hexadecimal colors
    ggplot(plot_data, aes(x = pxcor_of_patches, y = pycor_of_patches, fill = pcolor_of_patches)) +
      geom_raster() +
      coord_fixed(expand = FALSE) +
      scale_fill_identity() + # Scale identity because parse_netlogo_color returns valid hex codes
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        text = element_text(family = "Courier", face = "bold")
      )
  })
  
  # Render the Spaghetti Plot for model parameters
  output$spaghetti_plot <- renderPlot({
    req(input$run_btn)
    res <- experiment_results()
    data <- res$table
    
    # Validation messages if table result extraction fails
    validate(
      need(!is.null(data), "The logolink experiment failed to return a table object. Check popup notifications or R console for details."),
      need(is.data.frame(data), "The result format is not a compatible data frame. Check popup notifications or R console for details."),
      need(nrow(data) > 0, "! The experiment produced no table results. Check popup notifications or R console for details.")
    )
    
    metrics_selected <- input$selected_metrics
    if (is.null(metrics_selected) || length(metrics_selected) == 0) return(NULL)
    
    # Standardize column names (hyphens in NetLogo variables -> underscores in R data frames)
    names(data) <- str_replace_all(names(data), "-", "_")
    metrics_safe_names <- str_replace_all(metrics_selected, "-", "_")
    
    # Implement the requested data flow and visualization
    data |> 
      filter(step <= 200) |> 
      select(
        any_of(c("run_number", "step")),
        any_of(metrics_safe_names),
        any_of("venue_catchment_distance")         
      ) |> 
      # Safety fallback in case 'venue_catchment_distance' wasn't one of the constants
      mutate(venue_catchment_distance = if ("venue_catchment_distance" %in% names(cur_data())) venue_catchment_distance else "Default") |>
      # Ensure run_number exists in case logolink omitted it for single runs
      mutate(run_number = if ("run_number" %in% names(cur_data())) run_number else 1) |>
      pivot_longer(
        cols = any_of(metrics_safe_names),
        names_to = "index",
        values_to = "value"
      ) |> 
      mutate(
        grouping_combo_run = str_c(run_number, index, venue_catchment_distance),
        grouping_combo_condition = str_c(index, venue_catchment_distance)
      ) |> 
      ggplot(aes(x = step, y = value, color = index, group = grouping_combo_run)) +
      geom_line(alpha = 0.25) +
      facet_wrap(~ venue_catchment_distance) +
      geomtextpath::geom_textpath(
        aes(group = grouping_combo_condition, color = index, label = index), 
        offset = unit(5, "pt"), gap = TRUE, text_smoothing = 100
      ) +
      labs(
        title = "Segregation Indices Over Time", 
        x = "Step", 
        y = "Index Value", 
        color = "Index"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", family = "Courier", face = "bold"),
        text = element_text(family = "Courier", color = "black"),
        plot.title = element_text(face = "bold", margin = margin(b = 15))
      )
  })
}

shinyApp(ui, server)