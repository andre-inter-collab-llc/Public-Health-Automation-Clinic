qu# ==============================================================================
# Combine & Compare Spreadsheets
# Public Health Automation Clinic
# https://andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic/
#
# A Shiny app for combining multiple Excel/CSV spreadsheets and comparing
# their structure. Designed for public health professionals who collect data
# from multiple sources and need to merge them reliably.
#
# Author: André van Zyl, Intersect Collaborations LLC
# ==============================================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(openxlsx)
library(DT)
library(tidyr)
library(purrr)
library(tibble)
library(rpivotTable)

# --- Brand Configuration (Intersect Collaborations) --------------------------
# Colors aligned with _brand.yml from Public Health Automation Clinic

brand <- list(
  blue  = "#2494f7",
  teal  = "#00a4bb",
  navy  = "#01272f",
  dark  = "#020506",
  slate = "#204d70",
  ivory = "#fffff0",
  white = "#ffffff"
)

app_theme <- bs_theme(
  version = 5,
  bg = brand$white,
  fg = brand$dark,
  primary = brand$blue,
  secondary = brand$teal,
  info = brand$slate,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font = font_google("Fira Code"),
  "navbar-bg" = brand$navy,
  "card-border-color" = "rgba(36, 148, 247, 0.15)",
  "btn-border-radius" = "0.375rem"
)

# Increase max upload size to 100 MB
options(shiny.maxRequestSize = 100 * 1024^2)

# --- Helper Functions ---------------------------------------------------------

#' Read a spreadsheet file (Excel or CSV)
#'
#' @param path File path (temporary upload path)
#' @param original_name Original filename (used for extension detection)
#' @param sheet Sheet name (ignored for CSV files)
#' @param header_row Row number containing column headers
#' @return A list with `data` (tibble or NULL) and `error` (string or NULL)
read_spreadsheet <- function(path, original_name, sheet = NULL, header_row = 1) {
  ext <- tolower(tools::file_ext(original_name))

  tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      available_sheets <- excel_sheets(path)
      if (!is.null(sheet) && !(sheet %in% available_sheets)) {
        return(list(
          data = NULL,
          error = paste0(
            "Sheet ", sQuote(sheet), " not found. ",
            "Available: ", paste(available_sheets, collapse = ", ")
          )
        ))
      }
      df <- read_excel(
        path,
        sheet = sheet,
        skip = header_row - 1,
        col_types = "text"
      )
    } else if (ext == "csv") {
      df <- read.csv(
        path,
        skip = header_row - 1,
        colClasses = "character",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      df <- as_tibble(df)
    } else {
      return(list(data = NULL, error = paste("Unsupported file type:", ext)))
    }

    # Clean auto-generated column names (e.g., ...1, ...2 from readxl)
    df <- df %>%
      rename_with(
        ~ ifelse(grepl("^\\.\\.\\.", .), sub("^\\.\\.\\.", "x_", .), .),
        everything()
      ) %>%
      clean_names() %>%
      mutate(across(everything(), as.character))

    # Remove completely empty rows
    df <- df %>% filter(if_any(everything(), ~ !is.na(.) & . != ""))

    list(data = df, error = NULL)
  }, error = function(e) {
    list(data = NULL, error = conditionMessage(e))
  })
}

#' Build a column comparison matrix across files
#'
#' Creates a presence/absence matrix showing which columns appear in which
#' files, making structural differences immediately visible.
#'
#' @param files_data Named list of data frames (name = filename)
#' @return A tibble with column_name, one column per file (check/cross marks),
#'         files_present count, and in_all_files flag
build_column_matrix <- function(files_data) {
  col_info <- imap_dfr(files_data, function(df, name) {
    tibble(
      file = name,
      column_name = setdiff(names(df), "source_file")
    )
  })

  matrix_df <- col_info %>%
    mutate(present = "\u2713") %>%
    pivot_wider(
      names_from = file,
      values_from = present,
      values_fill = "\u2717"
    )

  file_cols <- setdiff(names(matrix_df), "column_name")

  matrix_df <- matrix_df %>%
    mutate(
      files_present = rowSums(across(all_of(file_cols), ~ . == "\u2713")),
      total_files = length(file_cols),
      in_all_files = files_present == total_files
    ) %>%
    arrange(files_present, column_name)

  matrix_df
}

# --- UI Components ------------------------------------------------------------

# About page content
about_content <- div(
  class = "container py-4",
  style = "max-width: 900px;",

  div(
    class = "text-center mb-4",
    h2(
      "Combine & Compare Spreadsheets",
      style = paste0("color: ", brand$navy, "; font-weight: 800;")
    ),
    p(
      class = "lead text-muted",
      "Merge multiple data collection spreadsheets and identify structural ",
      "differences before analysis"
    )
  ),

  card(
    card_header(class = "text-white", style = paste0("background-color: ", brand$blue, ";"), "The Problem"),
    card_body(
      p(
        "Public health teams frequently distribute Excel-based data collection ",
        "forms to multiple sites, facilities, or respondents. When these ",
        "spreadsheets come back, common problems arise:"
      ),
      tags$ul(
        tags$li("Column headers were renamed, reordered, or misspelled"),
        tags$li("Extra columns were added or required columns were removed"),
        tags$li("Different sites used different sheet names in the workbook"),
        tags$li(
          "Manually copying and pasting data across files is ",
          "error-prone and time-consuming"
        )
      ),
      p(
        "This tool automates the process of combining these spreadsheets while ",
        tags$strong("detecting structural differences"),
        " between files, so you can identify and resolve issues before analysis."
      )
    )
  ),

  card(
    class = "mt-3",
    card_header(class = "text-white", style = paste0("background-color: ", brand$teal, ";"), "How to Use"),
    card_body(
      tags$ol(
        class = "mb-0",
        tags$li(
          tags$strong("Upload files: "),
          "Select one or more Excel (.xlsx, .xls) or CSV (.csv) files."
        ),
        tags$li(
          tags$strong("Select sheet: "),
          "For Excel files, choose which sheet to read from each workbook. ",
          "CSV files do not require sheet selection."
        ),
        tags$li(
          tags$strong("Set header row: "),
          "Specify which row contains column headers (default: row 1). Use ",
          "this when your spreadsheets have title rows above the actual data."
        ),
        tags$li(
          tags$strong("Process: "),
          "Click ", tags$em("Process Files"), " to combine data and compare ",
          "structures across all uploaded files."
        ),
        tags$li(
          tags$strong("Review: "),
          "Check the ", tags$em("Column Comparison"), " tab to see which ",
          "columns appear in which files. Columns missing from some files are ",
          "highlighted. Review ", tags$em("File Issues"), " for errors."
        ),
        tags$li(
          tags$strong("Download: "),
          "Export the combined data as an Excel workbook containing all ",
          "analysis sheets (combined data, column comparison, and issues)."
        )
      )
    )
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      class = "mt-3",
      card_header("Supported File Formats"),
      card_body(
        class = "d-flex gap-3 flex-wrap",
        span(class = "badge fs-6", style = paste0("background-color: ", brand$blue, ";"), ".xlsx"),
        span(class = "badge fs-6", style = paste0("background-color: ", brand$blue, ";"), ".xls"),
        span(class = "badge fs-6", style = paste0("background-color: ", brand$teal, ";"), ".csv")
      )
    ),
    card(
      class = "mt-3",
      card_header("Key Features"),
      card_body(
        tags$ul(
          class = "mb-0 list-unstyled",
          tags$li(icon("check", class = "text-success"), " Column structure comparison across files"),
          tags$li(icon("check", class = "text-success"), " Missing sheet detection"),
          tags$li(icon("check", class = "text-success"), " Interactive pivot table exploration"),
          tags$li(icon("check", class = "text-success"), " Combined Excel download with all analysis sheets"),
          tags$li(icon("check", class = "text-success"), " Mixed Excel and CSV file support")
        )
      )
    )
  ),

  div(
    class = "text-center mt-4 pt-3 border-top",
    p(
      class = "text-muted mb-1",
      "Part of the ",
      tags$a(
        "Public Health Automation Clinic",
        href = "https://andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic/",
        target = "_blank",
        style = paste0("color: ", brand$blue, ";")
      )
    ),
    p(
      class = "text-muted small",
      "by ",
      tags$a(
        "Intersect Collaborations LLC",
        href = "https://www.intersectcollaborations.com/",
        target = "_blank",
        style = paste0("color: ", brand$teal, ";")
      )
    )
  )
)

# Workbench sidebar controls
workbench_sidebar <- sidebar(
  width = 300,
  title = div(
    icon("sliders"),
    " Controls",
    style = paste0("color: ", brand$navy, "; font-weight: 700;")
  ),

  fileInput(
    "files",
    label = tags$span(icon("file-excel"), " Upload Spreadsheets"),
    multiple = TRUE,
    accept = c(".xlsx", ".xls", ".csv"),
    placeholder = "Select files..."
  ),

  selectInput(
    "sheetName",
    label = tags$span(icon("layer-group"), " Sheet Name"),
    choices = NULL
  ),

  numericInput(
    "headerRow",
    label = tags$span(icon("heading"), " Header Row"),
    value = 1,
    min = 1,
    max = 100
  ),

  hr(),

  actionButton(
    "process",
    label = tags$span(icon("gears"), " Process Files"),
    class = "btn-primary w-100 mb-3",
    style = "font-weight: 600;"
  ),

  downloadButton(
    "downloadXLSX",
    label = tags$span(icon("download"), " Download Combined XLSX"),
    class = "btn-outline-primary w-100"
  ),

  hr(),

  div(
    class = "text-muted small",
    textOutput("statusText")
  )
)

# Main UI layout
ui <- page_navbar(
  title = tags$span(
    tags$img(
      src = "logo.png",
      height = "28px",
      class = "me-2",
      onerror = "this.style.display='none'"
    ),
    "Combine & Compare Spreadsheets"
  ),
  window_title = "Combine & Compare | Public Health Automation Clinic",
  theme = app_theme,
  fillable = FALSE,
  navbar_options = navbar_options(bg = brand$navy),

  header = tags$head(
    tags$style(HTML(sprintf("
      /* Brand-aligned custom styles */
      .navbar { border-bottom: 3px solid %s; }
      .nav-link.active { border-bottom: 2px solid %s !important; }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid %s;
        border-radius: 0.375rem;
        padding: 0.25rem 0.5rem;
      }
      .value-box { border-radius: 0.5rem; }
      .card { border-radius: 0.5rem; }
      .sidebar { background-color: #f8f9fa; }
    ", brand$teal, brand$blue, brand$blue)))
  ),

  nav_panel(
    title = tags$span(icon("info-circle"), " About"),
    about_content
  ),

  nav_panel(
    title = tags$span(icon("wrench"), " Workbench"),
    layout_sidebar(
      sidebar = workbench_sidebar,

      # Summary cards (dynamic)
      uiOutput("summaryCards"),

      # Tabbed results area
      navset_card_tab(
        id = "results_tabs",

        nav_panel(
          title = tags$span(icon("table"), " Combined Data"),
          div(
            class = "pt-2",
            DTOutput("dataTable")
          )
        ),

        nav_panel(
          title = tags$span(icon("columns"), " Column Comparison"),
          div(
            class = "pt-2",
            p(
              class = "text-muted small mb-2",
              icon("info-circle"),
              " This matrix shows which columns appear in which files. ",
              tags$span(style = paste0("color: ", brand$teal, "; font-weight: bold;"), "\u2713"),
              " = column present, ",
              tags$span(style = "color: #e74c3c; font-weight: bold;", "\u2717"),
              " = column missing. Columns missing from some files are sorted to the top."
            ),
            DTOutput("columnMatrix")
          )
        ),

        nav_panel(
          title = tags$span(icon("triangle-exclamation"), " File Issues"),
          div(
            class = "pt-2",
            DTOutput("issuesTable")
          )
        ),

        nav_panel(
          title = tags$span(icon("chart-bar"), " Explore Data"),
          div(
            class = "pt-2",
            rpivotTableOutput("pivotTable")
          )
        )
      )
    )
  ),

  nav_spacer(),

  nav_item(
    tags$a(
      class = "nav-link",
      href = "https://andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic/",
      target = "_blank",
      style = paste0("color: ", brand$teal, " !important; font-size: 0.85rem;"),
      icon("book-medical"),
      " Automation Clinic"
    )
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Reactive: Update sheet name choices based on uploaded files ---
  observe({
    req(input$files)

    file_info <- input$files
    sheets <- character(0)
    has_excel <- FALSE
    has_csv <- FALSE

    for (i in seq_len(nrow(file_info))) {
      ext <- tolower(tools::file_ext(file_info$name[i]))
      if (ext %in% c("xlsx", "xls")) {
        has_excel <- TRUE
        tryCatch({
          s <- excel_sheets(file_info$datapath[i])
          sheets <- unique(c(sheets, s))
        }, error = function(e) NULL)
      } else if (ext == "csv") {
        has_csv <- TRUE
      }
    }

    if (length(sheets) == 0 && has_csv) {
      sheets <- c("(CSV files: no sheet selection needed)")
    } else if (length(sheets) == 0) {
      sheets <- c("(No sheets found)")
    }

    updateSelectInput(session, "sheetName", choices = sheets)
  })

  # --- Reactive: Process all uploaded files ---
  processed <- eventReactive(input$process, {
    req(input$files)

    file_info <- input$files
    n_files <- nrow(file_info)
    sheet <- input$sheetName
    header_row <- input$headerRow

    successful_data <- list()
    issues <- tibble(
      file = character(),
      issue_type = character(),
      details = character()
    )

    withProgress(message = "Processing files...", value = 0, {
      for (i in seq_len(n_files)) {
        incProgress(1 / n_files, detail = file_info$name[i])

        fname <- file_info$name[i]
        fpath <- file_info$datapath[i]
        ext <- tolower(tools::file_ext(fname))

        # CSV files ignore the sheet selection
        use_sheet <- if (ext %in% c("xlsx", "xls")) sheet else NULL

        result <- read_spreadsheet(fpath, fname, use_sheet, header_row)

        if (!is.null(result$error)) {
          issues <- bind_rows(issues, tibble(
            file = fname,
            issue_type = if (grepl("not found", result$error, ignore.case = TRUE))
              "Missing Sheet" else "Read Error",
            details = result$error
          ))
        } else if (!is.null(result$data) && nrow(result$data) > 0) {
          result$data <- result$data %>%
            mutate(source_file = fname, .before = 1)
          successful_data[[fname]] <- result$data
        } else {
          issues <- bind_rows(issues, tibble(
            file = fname,
            issue_type = "Empty Data",
            details = "File or sheet contains no data rows"
          ))
        }
      }
    })

    # Combine all successfully read data frames
    combined <- if (length(successful_data) > 0) {
      bind_rows(successful_data)
    } else {
      tibble()
    }

    # Build column comparison matrix
    col_matrix <- if (length(successful_data) > 0) {
      build_column_matrix(successful_data)
    } else {
      tibble()
    }

    # Summary of column issues
    n_inconsistent <- if (nrow(col_matrix) > 0) {
      sum(!col_matrix$in_all_files)
    } else {
      0
    }

    # If no issues found, add an all-clear message
    if (nrow(issues) == 0) {
      issues <- tibble(
        file = "All files",
        issue_type = "None",
        details = "All files processed successfully"
      )
    }

    list(
      combined = combined,
      col_matrix = col_matrix,
      issues = issues,
      n_files = n_files,
      n_success = length(successful_data),
      n_rows = nrow(combined),
      n_cols = max(0, ncol(combined) - 1),
      n_inconsistent = n_inconsistent
    )
  })

  # --- Summary value boxes ---
  output$summaryCards <- renderUI({
    if (is.null(tryCatch(processed(), error = function(e) NULL))) {
      return(
        div(
          class = "text-center text-muted py-5",
          icon("cloud-arrow-up", class = "fa-3x mb-3"),
          h4("Upload spreadsheets and click Process to begin"),
          p("Supports .xlsx, .xls, and .csv files")
        )
      )
    }

    data <- processed()

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Files Uploaded",
        value = data$n_files,
        showcase = icon("file-excel"),
        theme = value_box_theme(bg = brand$blue, fg = brand$white)
      ),
      value_box(
        title = "Files Processed",
        value = data$n_success,
        showcase = icon("circle-check"),
        theme = value_box_theme(
          bg = if (data$n_success == data$n_files) brand$teal else "#e67e22",
          fg = brand$white
        )
      ),
      value_box(
        title = "Total Rows",
        value = format(data$n_rows, big.mark = ","),
        showcase = icon("database"),
        theme = value_box_theme(bg = brand$slate, fg = brand$white)
      ),
      value_box(
        title = "Column Mismatches",
        value = data$n_inconsistent,
        showcase = icon("triangle-exclamation"),
        theme = value_box_theme(
          bg = if (data$n_inconsistent == 0) brand$navy else "#e74c3c",
          fg = brand$ivory
        )
      )
    )
  })

  # --- Combined data table ---
  output$dataTable <- renderDT({
    req(processed(), nrow(processed()$combined) > 0)
    datatable(
      processed()$combined,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv")
      ),
      class = "stripe hover compact",
      rownames = FALSE
    )
  })

  # --- Column comparison matrix ---
  output$columnMatrix <- renderDT({
    req(processed(), nrow(processed()$col_matrix) > 0)

    df <- processed()$col_matrix
    file_cols <- setdiff(
      names(df),
      c("column_name", "files_present", "total_files", "in_all_files")
    )

    display_df <- df %>% select(-in_all_files, -total_files)

    datatable(
      display_df,
      filter = "top",
      options = list(
        pageLength = 50,
        autoWidth = TRUE,
        scrollX = TRUE,
        order = list(list(ncol(display_df) - 1, "asc"))
      ),
      class = "stripe hover compact",
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = file_cols,
        color = styleEqual(
          c("\u2713", "\u2717"),
          c(brand$teal, "#e74c3c")
        ),
        fontWeight = "bold",
        textAlign = "center"
      ) %>%
      formatStyle(
        "files_present",
        background = styleColorBar(range(df$files_present), brand$blue),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # --- File issues table ---
  output$issuesTable <- renderDT({
    req(processed())
    datatable(
      processed()$issues,
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        dom = "t"
      ),
      class = "stripe hover",
      rownames = FALSE
    ) %>%
      formatStyle(
        "issue_type",
        color = styleEqual(
          c("Missing Sheet", "Read Error", "Empty Data", "None"),
          c("#e67e22", "#e74c3c", "#f39c12", brand$teal)
        ),
        fontWeight = "bold"
      )
  })

  # --- Pivot table ---
  output$pivotTable <- renderRpivotTable({
    req(processed(), nrow(processed()$combined) > 0)
    rpivotTable(processed()$combined)
  })

  # --- Status text in sidebar ---
  output$statusText <- renderText({
    if (is.null(tryCatch(processed(), error = function(e) NULL))) {
      "Ready. Upload files and click Process to begin."
    } else {
      data <- processed()
      paste0(
        data$n_success, " of ", data$n_files, " files combined (",
        format(data$n_rows, big.mark = ","), " rows, ",
        data$n_cols, " columns)"
      )
    }
  })

  # --- Download handler ---
  output$downloadXLSX <- downloadHandler(
    filename = function() {
      sheet_label <- if (!is.null(input$sheetName) &&
                         !grepl("^\\(", input$sheetName)) {
        janitor::make_clean_names(input$sheetName)
      } else {
        "data"
      }
      paste0("combined-", sheet_label, "-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(processed())

      col_matrix_export <- processed()$col_matrix %>%
        select(-in_all_files)

      sheets_list <- list(
        "Combined Data" = processed()$combined,
        "Column Comparison" = col_matrix_export,
        "File Issues" = processed()$issues
      )

      # Create workbook with branded header style
      wb <- createWorkbook()

      header_style <- createStyle(
        fontName = "Inter",
        fontSize = 11,
        fontColour = brand$white,
        fgFill = brand$navy,
        textDecoration = "bold",
        halign = "center",
        border = "TopBottomLeftRight",
        borderColour = brand$slate
      )

      for (sheet_name in names(sheets_list)) {
        addWorksheet(wb, sheet_name)
        writeDataTable(
          wb,
          sheet_name,
          sheets_list[[sheet_name]],
          tableStyle = "TableStyleMedium2"
        )
        addStyle(
          wb,
          sheet_name,
          header_style,
          rows = 1,
          cols = seq_len(ncol(sheets_list[[sheet_name]])),
          gridExpand = TRUE
        )
        setColWidths(wb, sheet_name, cols = seq_len(ncol(sheets_list[[sheet_name]])), widths = "auto")
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# --- Run App ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
