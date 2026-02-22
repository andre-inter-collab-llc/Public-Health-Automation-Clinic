# ==============================================================================
# Stack & Check Spreadsheets
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

# Remove Shiny's default 5 MB upload limit. Uploads of any size are allowed,
# but users may experience slow performance or out-of-memory errors if total
# upload size exceeds approximately 250 MB, depending on available RAM.
options(shiny.maxRequestSize = Inf)

# Always open in the system default browser (not the IDE viewer pane).
# IDE extensions (VS Code, RStudio) can intercept browseURL, so we call
# the OS browser directly.
options(shiny.launch.browser = function(url) {
  if (.Platform$OS.type == "windows") {
    shell.exec(url)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", shQuote(url)))
  } else {
    system(paste("xdg-open", shQuote(url)))
  }
})

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

#' Build a column comparison matrix (transposed: files as rows, columns as headers)
#'
#' Each row is a file, each column header is a column name from across all files.
#' Cells contain check/cross marks. This orientation scales better with many files
#' because columns (typically 10-50) stay manageable while rows grow with files.
#'
#' @param files_data Named list of data frames (name = filename)
#' @return A list with:
#'   - matrix_df: the transposed presence/absence tibble
#'   - all_columns: character vector of all column names across files
#'   - n_total_files: total number of files
build_column_matrix <- function(files_data) {
  # Collect all unique column names (excluding source_file)
  all_columns <- unique(unlist(lapply(files_data, function(df) {
    setdiff(names(df), "source_file")
  })))

  # Build transposed matrix: one row per file
  matrix_df <- imap_dfr(files_data, function(df, fname) {
    file_cols <- setdiff(names(df), "source_file")
    row <- setNames(
      ifelse(all_columns %in% file_cols, "\u2713", "\u2717"),
      all_columns
    )
    tibble(file = fname) %>% bind_cols(as_tibble_row(row))
  })

  # Add summary column: how many of the expected columns are present
  matrix_df <- matrix_df %>%
    mutate(
      columns_present = rowSums(across(all_of(all_columns), ~ . == "\u2713")),
      columns_expected = length(all_columns),
      columns_missing = columns_expected - columns_present
    )

  list(
    matrix_df = matrix_df,
    all_columns = all_columns,
    n_total_files = length(files_data)
  )
}

#' Build a compact discrepancy summary
#'
#' Shows ONLY columns that are not present in every file (the mismatches).
#' For each mismatched column, lists which files are missing it.
#' This is the most compact view: if everything matches, it returns an empty table.
#'
#' @param files_data Named list of data frames
#' @return A tibble with: column_name, files_present, files_missing_count,
#'         files_missing (comma-separated names)
build_discrepancy_summary <- function(files_data) {
  all_files <- names(files_data)
  n_total <- length(all_files)

  col_info <- imap_dfr(files_data, function(df, fname) {
    tibble(
      file = fname,
      column_name = setdiff(names(df), "source_file")
    )
  })

  col_counts <- col_info %>%
    group_by(column_name) %>%
    summarise(
      files_present = n(),
      files_with_column = list(unique(file)),
      .groups = "drop"
    ) %>%
    filter(files_present < n_total) %>%
    mutate(
      files_missing_count = n_total - files_present,
      pct_files_present = round(100 * files_present / n_total, 1),
      files_missing = sapply(files_with_column, function(present) {
        missing <- setdiff(all_files, present)
        paste(missing, collapse = "; ")
      })
    ) %>%
    select(column_name, files_present, files_missing_count,
           pct_files_present, files_missing) %>%
    arrange(files_present, column_name)

  col_counts
}

#' Build per-file structure profiles
#'
#' One row per file showing column count, row count, and which columns
#' are missing or extra relative to the "expected" set (columns present
#' in all other files).
#'
#' @param files_data Named list of data frames
#' @return A tibble with file, n_rows, n_columns, n_missing, missing_columns,
#'         n_extra, extra_columns
build_file_profiles <- function(files_data) {
  # "Expected" columns = those present in the majority of files (>50%)
  all_files <- names(files_data)
  n_total <- length(all_files)

  col_info <- imap_dfr(files_data, function(df, fname) {
    tibble(
      file = fname,
      column_name = setdiff(names(df), "source_file")
    )
  })

  # Columns in >50% of files are "expected"
  col_freq <- col_info %>%
    group_by(column_name) %>%
    summarise(n = n(), .groups = "drop")

  expected_cols <- col_freq %>%
    filter(n > n_total / 2) %>%
    pull(column_name)

  imap_dfr(files_data, function(df, fname) {
    file_cols <- setdiff(names(df), "source_file")
    missing <- setdiff(expected_cols, file_cols)
    extra <- setdiff(file_cols, expected_cols)

    tibble(
      file = fname,
      n_rows = nrow(df),
      n_columns = length(file_cols),
      n_expected = length(expected_cols),
      n_missing = length(missing),
      missing_columns = if (length(missing) > 0) paste(missing, collapse = "; ") else "",
      n_extra = length(extra),
      extra_columns = if (length(extra) > 0) paste(extra, collapse = "; ") else ""
    )
  }) %>%
    arrange(desc(n_missing), desc(n_extra))
}

# --- UI Components ------------------------------------------------------------

# About page content
about_content <- div(
  class = "container py-4",
  style = "max-width: 900px;",

  div(
    class = "text-center mb-4",
    h2(
      "Stack & Check Spreadsheets",
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
    "Stack & Check Spreadsheets"
  ),
  window_title = "Stack & Check | Public Health Automation Clinic",
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

            navset_pill(
              id = "column_subtabs",

              nav_panel(
                title = tags$span(icon("triangle-exclamation"), " Discrepancies Only"),
                div(
                  class = "pt-3",
                  p(
                    class = "text-muted small mb-2",
                    icon("filter"),
                    " Shows only columns that are ",
                    tags$strong("not present in every file"),
                    ". If this table is empty, all files have identical column structures."
                  ),
                  uiOutput("discrepancySummaryInfo"),
                  DTOutput("discrepancyTable")
                )
              ),

              nav_panel(
                title = tags$span(icon("table-cells"), " Full Matrix"),
                div(
                  class = "pt-3",
                  p(
                    class = "text-muted small mb-2",
                    icon("info-circle"),
                    " Each row is a file, each column header is a column name. ",
                    tags$span(style = paste0("color: ", brand$teal, "; font-weight: bold;"), "\u2713"),
                    " = present, ",
                    tags$span(style = "color: #e74c3c; font-weight: bold;", "\u2717"),
                    " = missing. Files with more missing columns sort to the top."
                  ),
                  DTOutput("columnMatrix")
                )
              ),

              nav_panel(
                title = tags$span(icon("id-card"), " File Profiles"),
                div(
                  class = "pt-3",
                  p(
                    class = "text-muted small mb-2",
                    icon("info-circle"),
                    " Per-file summary showing row/column counts, and which ",
                    "expected columns are missing or which extra columns were added. ",
                    "\"Expected\" columns are those present in the majority of files."
                  ),
                  DTOutput("fileProfilesTable")
                )
              )
            )
          )
        ),

        nav_panel(
          title = tags$span(icon("triangle-exclamation"), " File Issues"),
          div(
            class = "pt-2",
            p(
              class = "text-muted small mb-2",
              icon("info-circle"),
              " Files that could ", tags$strong("not be processed"), " at all are listed here. ",
              "This is separate from column discrepancies (which appear in the Column Comparison tab). ",
              "Issue types:"
            ),
            tags$ul(
              class = "small text-muted mb-3",
              tags$li(tags$strong("Missing Sheet: ", style = "color: #e67e22;"),
                      "The selected sheet name was not found in the workbook."),
              tags$li(tags$strong("Read Error: ", style = "color: #e74c3c;"),
                      "The file could not be read (corrupt, password-protected, or unsupported format)."),
              tags$li(tags$strong("Empty Data: ", style = "color: #f39c12;"),
                      "The file or sheet contained no data rows after the header.")
            ),
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

    # Build column comparison views
    col_result <- if (length(successful_data) > 0) {
      build_column_matrix(successful_data)
    } else {
      list(matrix_df = tibble(), all_columns = character(0), n_total_files = 0)
    }

    discrepancy <- if (length(successful_data) > 0) {
      build_discrepancy_summary(successful_data)
    } else {
      tibble()
    }

    file_profiles <- if (length(successful_data) > 0) {
      build_file_profiles(successful_data)
    } else {
      tibble()
    }

    # Summary of column issues
    n_inconsistent <- nrow(discrepancy)

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
      col_result = col_result,
      discrepancy = discrepancy,
      file_profiles = file_profiles,
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

  # --- Discrepancy summary (compact view) ---
  output$discrepancySummaryInfo <- renderUI({
    req(processed())
    disc <- processed()$discrepancy
    if (nrow(disc) == 0) {
      div(
        class = "alert alert-success py-2 px-3",
        icon("circle-check"),
        tags$strong(" All files have identical column structures."),
        " No discrepancies detected."
      )
    } else {
      div(
        class = "alert alert-warning py-2 px-3",
        icon("triangle-exclamation"),
        tags$strong(paste0(" ", nrow(disc), " column(s) ")),
        "are not present in every file."
      )
    }
  })

  output$discrepancyTable <- renderDT({
    req(processed())
    disc <- processed()$discrepancy
    if (nrow(disc) == 0) return(NULL)

    datatable(
      disc,
      filter = "top",
      colnames = c(
        "Column Name" = "column_name",
        "Files Present" = "files_present",
        "Files Missing" = "files_missing_count",
        "% Present" = "pct_files_present",
        "Missing From (files)" = "files_missing"
      ),
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        scrollX = TRUE,
        order = list(list(1, "asc")),
        columnDefs = list(
          list(width = "40%", targets = 4)
        )
      ),
      class = "stripe hover compact",
      rownames = FALSE
    ) %>%
      formatStyle(
        "Files Missing",
        color = styleInterval(c(0), c(brand$teal, "#e74c3c")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "% Present",
        background = styleColorBar(c(0, 100), brand$blue),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # --- Full column matrix (transposed: files as rows) ---
  output$columnMatrix <- renderDT({
    req(processed())
    col_result <- processed()$col_result
    req(nrow(col_result$matrix_df) > 0)

    df <- col_result$matrix_df
    data_cols <- col_result$all_columns

    display_df <- df %>% select(file, all_of(data_cols), columns_present, columns_missing)

    datatable(
      display_df,
      filter = "top",
      options = list(
        pageLength = 50,
        autoWidth = FALSE,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        order = list(list(ncol(display_df) - 1, "asc"))
      ),
      extensions = c("FixedColumns"),
      class = "stripe hover compact nowrap",
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = data_cols,
        color = styleEqual(
          c("\u2713", "\u2717"),
          c(brand$teal, "#e74c3c")
        ),
        fontWeight = "bold",
        textAlign = "center"
      ) %>%
      formatStyle(
        "columns_missing",
        color = styleInterval(c(0), c(brand$teal, "#e74c3c")),
        fontWeight = "bold"
      )
  })

  # --- File profiles table ---
  output$fileProfilesTable <- renderDT({
    req(processed(), nrow(processed()$file_profiles) > 0)

    datatable(
      processed()$file_profiles,
      filter = "top",
      colnames = c(
        "File" = "file",
        "Rows" = "n_rows",
        "Columns" = "n_columns",
        "Expected" = "n_expected",
        "Missing" = "n_missing",
        "Missing Columns" = "missing_columns",
        "Extra" = "n_extra",
        "Extra Columns" = "extra_columns"
      ),
      options = list(
        pageLength = 50,
        autoWidth = TRUE,
        scrollX = TRUE,
        order = list(list(4, "desc")),
        columnDefs = list(
          list(width = "25%", targets = c(5, 7))
        )
      ),
      class = "stripe hover compact",
      rownames = FALSE
    ) %>%
      formatStyle(
        "Missing",
        color = styleInterval(c(0), c(brand$teal, "#e74c3c")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Extra",
        color = styleInterval(c(0), c(brand$teal, "#e67e22")),
        fontWeight = "bold"
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

      # Prepare all analysis sheets for export
      col_matrix_export <- processed()$col_result$matrix_df %>%
        select(-columns_expected)

      discrepancy_export <- processed()$discrepancy
      profiles_export <- processed()$file_profiles

      sheets_list <- list(
        "Combined Data" = processed()$combined,
        "Discrepancies" = if (nrow(discrepancy_export) > 0) discrepancy_export
                          else tibble(status = "All files have identical column structures"),
        "Column Matrix" = col_matrix_export,
        "File Profiles" = profiles_export,
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
