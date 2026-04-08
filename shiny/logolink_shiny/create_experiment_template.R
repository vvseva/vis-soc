#' Generate a logolink create_experiment Template
#'
#' Scans a NetLogo XML file (.nlogox) for UI elements (sliders, choosers, 
#' switches) and plotting pens, and generates a ready-to-run R script 
#' template for `logolink::create_experiment()`.
#'
#' @param model_path A string specifying the path to the .nlogox file.
#' @param print_to_console Logical; if TRUE, uses cat() to print the result 
#'   directly to the console.
#' @param copy_to_clipboard Logical; if TRUE, copies the generated code to 
#'   the system clipboard using the `clipr` package.
#' @param insert_at_cursor Logical; if TRUE, inserts the generated code 
#'   directly at the cursor position in RStudio using `rstudioapi`.
#'
#' @return A character string containing the generated R code.
#' @import xml2
#' @import stringr
#' @export
create_experiment_template <- function(
    model_path, 
    print_to_console = TRUE,
    copy_to_clipboard = TRUE,
    insert_at_cursor = FALSE
) {
  has_xml2 <- requireNamespace("xml2", quietly = TRUE)
  has_stringr <- requireNamespace("stringr", quietly = TRUE)
  
  if (!has_xml2 || !has_stringr) {
    stop("Packages 'xml2' and 'stringr' are required for this function.")
  }
  
  doc <- xml2::read_xml(model_path)
  
  # --- 1. Parse constants (sliders, choosers, switches) ---
  constants_list <- list()
  
  # Process sliders
  sliders <- xml2::xml_find_all(doc, ".//slider")
  for (s in sliders) {
    name <- xml2::xml_attr(s, "variable")
    if (is.na(name)) {
      name <- xml2::xml_attr(s, "display")
    }
    
    val <- xml2::xml_attr(s, "default")
    
    if (!is.na(name) && !is.na(val)) {
      constants_list[[name]] <- val # Sliders are numeric, leave as-is
    }
  }
  
  # Process switches
  switches <- xml2::xml_find_all(doc, ".//switch")
  for (sw in switches) {
    name <- xml2::xml_attr(sw, "variable")
    if (is.na(name)) {
      name <- xml2::xml_attr(sw, "display")
    }
    
    val <- xml2::xml_attr(sw, "on")
    
    if (!is.na(name) && !is.na(val)) {
      # Convert NetLogo "true"/"false" to R TRUE/FALSE
      is_true <- tolower(val) == "true"
      constants_list[[name]] <- ifelse(is_true, "TRUE", "FALSE")
    }
  }
  
  # Process choosers
  choosers <- xml2::xml_find_all(doc, ".//chooser")
  for (c in choosers) {
    name <- xml2::xml_attr(c, "variable")
    if (is.na(name)) {
      name <- xml2::xml_attr(c, "display")
    }
    
    # NetLogo 'current' attribute is 0-indexed
    current_idx <- as.numeric(xml2::xml_attr(c, "current")) + 1 
    choice_nodes <- xml2::xml_find_all(c, "./choice")
    choices <- xml2::xml_attr(choice_nodes, "value")
    
    if (!is.na(name) && length(choices) > 0 && !is.na(current_idx)) {
      val <- choices[current_idx]
      
      # If the value is a string that isn't a number, wrap it in quotes for R
      if (suppressWarnings(is.na(as.numeric(val)))) {
        val <- paste0("\"", val, "\"")
      }
      constants_list[[name]] <- val
    }
  }
  
  # Format constants into a string
  if (length(constants_list) > 0) {
    const_lines <- sapply(names(constants_list), function(n) {
      paste0("    \"", n, "\" = c(", constants_list[[n]], ")")
    })
    
    constants_str <- paste0(
      "  constants = list(\n", 
      paste(const_lines, collapse = ",\n"), 
      "\n  )"
    )
  } else {
    constants_str <- "  constants = list()"
  }
  
  # --- 2. Parse metrics ---
  plot_updates <- xml2::xml_find_all(doc, ".//plot//pen//update")
  plot_texts <- xml2::xml_text(plot_updates)
  
  # Extract lines that explicitly "plot [variable]"
  metrics <- plot_texts[stringr::str_starts(plot_texts, "plot\\s")]
  
  metrics <- metrics |> 
    stringr::str_remove("plot\\s+") |> 
    stringr::str_trim() |> 
    unique()
  
  # Add default patch properties to ensure lists output is populated
  metrics <- c(
    metrics, 
    "[pxcor] of patches", 
    "[pycor] of patches", 
    "[pcolor] of patches"
  )
  
  metrics_lines <- paste0("    \"", metrics, "\"")
  metrics_str <- paste0(
    "  metrics = c(\n", 
    paste(metrics_lines, collapse = ",\n"), 
    "\n  )"
  )
  
  # --- 3. Build final template string ---
  template <- paste0(
    "setup_file <- create_experiment(\n",
    "  name = \"Auto-Generated Analysis\",\n",
    "  repetitions = 1,\n",
    "  run_metrics_every_step = TRUE,\n",
    "  setup = \"setup\",\n",
    "  go = \"go\",\n",
    "  time_limit = 200,\n",
    metrics_str, ",\n",
    constants_str, "\n",
    ")\n"
  )
  
  if (print_to_console) {
    cat(template)
  }
  
  if (copy_to_clipboard) {
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(template)
      message("Template successfully copied to clipboard.")
    } else {
      warning(
        "Package 'clipr' is required to copy to clipboard. ",
        "Please install it using install.packages('clipr')."
      )
    }
  }
  
  if (insert_at_cursor) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && 
        rstudioapi::isAvailable()) {
      rstudioapi::insertText(text = template)
      message("Template inserted at cursor position.")
    } else {
      warning(
        "Package 'rstudioapi' is required and must be run within RStudio ",
        "to insert at cursor. Please install it or use RStudio."
      )
    }
  }
  
  invisible(template)
}
