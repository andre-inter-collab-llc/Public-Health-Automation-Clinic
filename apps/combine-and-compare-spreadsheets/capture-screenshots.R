# ==============================================================================
# Screenshot Capture Script for Combine & Compare Spreadsheets
#
# Creates sample test data with structural differences, launches the app,
# and uses chromote (headless Chrome) to capture screenshots at each step.
#
# Run from the project root:
#   Rscript apps/combine-and-compare-spreadsheets/capture-screenshots.R
# ==============================================================================

library(openxlsx)
library(shiny)
library(chromote)
library(callr)

# --- Configuration -----------------------------------------------------------
app_dir   <- "apps/combine-and-compare-spreadsheets"
img_dir   <- "assets/images/04a-combine-compare"
test_dir  <- file.path(tempdir(), "test-spreadsheets")

dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

# --- Step 1: Create sample test workbooks with intentional differences --------
cat("Creating sample test data...\n")

set.seed(42)
counties <- c("Hamilton", "Butler", "Warren", "Clermont", "Montgomery")

# File 1: Perfect match to template
wb1 <- createWorkbook()
addWorksheet(wb1, "Surveillance Data")
df1 <- data.frame(
  county = sample(counties, 20, replace = TRUE),
  report_year = rep(2025, 20),
  disease_code = sample(c("A01", "A02", "B01", "B02", "C01"), 20, replace = TRUE),
  disease_name = sample(c("Disease Alpha", "Disease Beta", "Disease Gamma",
                           "Disease Delta", "Disease Epsilon"), 20, replace = TRUE),
  case_count = sample(1:150, 20),
  incidence_rate = round(runif(20, 0.5, 25.0), 2),
  population = sample(50000:500000, 20),
  date_submitted = format(Sys.Date() - sample(0:30, 20, replace = TRUE), "%m/%d/%Y"),
  stringsAsFactors = FALSE
)
writeDataTable(wb1, "Surveillance Data", df1)
saveWorkbook(wb1, file.path(test_dir, "Hamilton_County_2025.xlsx"), overwrite = TRUE)

# File 2: Perfect match (different data)
wb2 <- createWorkbook()
addWorksheet(wb2, "Surveillance Data")
df2 <- data.frame(
  county = rep("Butler", 15),
  report_year = rep(2025, 15),
  disease_code = sample(c("A01", "A02", "B01"), 15, replace = TRUE),
  disease_name = sample(c("Disease Alpha", "Disease Beta", "Disease Gamma"), 15, replace = TRUE),
  case_count = sample(1:80, 15),
  incidence_rate = round(runif(15, 0.5, 15.0), 2),
  population = rep(385000, 15),
  date_submitted = format(Sys.Date() - sample(0:30, 15, replace = TRUE), "%m/%d/%Y"),
  stringsAsFactors = FALSE
)
writeDataTable(wb2, "Surveillance Data", df2)
saveWorkbook(wb2, file.path(test_dir, "Butler_County_2025.xlsx"), overwrite = TRUE)

# File 3: Missing column (no incidence_rate) + extra column (notes)
wb3 <- createWorkbook()
addWorksheet(wb3, "Surveillance Data")
df3 <- data.frame(
  county = rep("Warren", 12),
  report_year = rep(2025, 12),
  disease_code = sample(c("A01", "B01", "C01"), 12, replace = TRUE),
  disease_name = sample(c("Disease Alpha", "Disease Gamma", "Disease Epsilon"), 12, replace = TRUE),
  case_count = sample(1:60, 12),
  population = rep(242000, 12),
  date_submitted = format(Sys.Date() - sample(0:30, 12, replace = TRUE), "%m/%d/%Y"),
  notes = sample(c("Confirmed", "Probable", "Suspect", ""), 12, replace = TRUE),
  stringsAsFactors = FALSE
)
writeDataTable(wb3, "Surveillance Data", df3)
saveWorkbook(wb3, file.path(test_dir, "Warren_County_2025.xlsx"), overwrite = TRUE)

# File 4: Different sheet name ("Sheet1" instead of "Surveillance Data")
wb4 <- createWorkbook()
addWorksheet(wb4, "Sheet1")
df4 <- data.frame(
  county = rep("Clermont", 10),
  report_year = rep(2025, 10),
  disease_code = sample(c("A01", "A02"), 10, replace = TRUE),
  disease_name = sample(c("Disease Alpha", "Disease Beta"), 10, replace = TRUE),
  case_count = sample(1:40, 10),
  incidence_rate = round(runif(10, 0.5, 10.0), 2),
  population = rep(209000, 10),
  date_submitted = format(Sys.Date() - sample(0:30, 10, replace = TRUE), "%m/%d/%Y"),
  stringsAsFactors = FALSE
)
writeDataTable(wb4, "Sheet1", df4)
saveWorkbook(wb4, file.path(test_dir, "Clermont_County_2025.xlsx"), overwrite = TRUE)

# File 5: Renamed columns (dx_code/dx_name instead of disease_code/disease_name)
wb5 <- createWorkbook()
addWorksheet(wb5, "Surveillance Data")
df5 <- data.frame(
  county = rep("Montgomery", 18),
  report_year = rep(2025, 18),
  dx_code = sample(c("A01", "A02", "B01", "B02"), 18, replace = TRUE),
  dx_name = sample(c("Disease Alpha", "Disease Beta",
                       "Disease Gamma", "Disease Delta"), 18, replace = TRUE),
  case_count = sample(1:100, 18),
  incidence_rate = round(runif(18, 0.5, 20.0), 2),
  population = rep(537000, 18),
  date_submitted = format(Sys.Date() - sample(0:30, 18, replace = TRUE), "%m/%d/%Y"),
  stringsAsFactors = FALSE
)
writeDataTable(wb5, "Surveillance Data", df5)
saveWorkbook(wb5, file.path(test_dir, "Montgomery_County_2025.xlsx"), overwrite = TRUE)

cat("Test files created in:", test_dir, "\n")

# --- Step 2: Launch Shiny app in background -----------------------------------
cat("\nLaunching Shiny app in background process...\n")

bg <- r_bg(function(app_dir) {
  options(shiny.launch.browser = FALSE)
  shiny::runApp(app_dir, port = 7777L, launch.browser = FALSE, host = "127.0.0.1")
}, args = list(app_dir = app_dir))

# Wait for app to be ready
Sys.sleep(10)
if (!bg$is_alive()) {
  cat("ERROR: App failed to start.\n")
  cat(bg$read_all_error())
  quit(status = 1)
}
cat("App running on http://127.0.0.1:7777\n")

# --- Step 3: Capture screenshots with chromote --------------------------------
cat("\nStarting headless Chrome...\n")

b <- ChromoteSession$new(width = 1400, height = 900)

take_screenshot <- function(session, filename, delay = 2) {
  Sys.sleep(delay)
  filepath <- file.path(img_dir, filename)
  session$screenshot(filepath, selector = "html")
  cat("  Saved:", filename, "\n")
}

# Screenshot 1: About page
cat("Capturing screenshots...\n")
b$Page$navigate("http://127.0.0.1:7777")
take_screenshot(b, "01-about-page.png", delay = 6)

# Screenshot 2: Navigate to Workbench tab (empty state)
b$Runtime$evaluate("
  var links = document.querySelectorAll('.navbar .nav-link');
  for (var l of links) {
    if (l.textContent.trim().includes('Workbench')) { l.click(); break; }
  }
")
take_screenshot(b, "02-workbench-empty.png", delay = 3)

# Upload the test files via CDP file input
cat("Uploading test files...\n")
test_files <- normalizePath(
  list.files(test_dir, full.names = TRUE, pattern = "\\.xlsx$"),
  winslash = "/"
)

dom_doc <- b$DOM$getDocument()
file_input <- b$DOM$querySelector(dom_doc$root$nodeId, "input[type='file']")
b$DOM$setFileInputFiles(
  files = as.list(test_files),
  nodeId = file_input$nodeId
)
Sys.sleep(4)

# Select sheet name
b$Runtime$evaluate("
  var sel = document.getElementById('sheetName');
  if (sel) {
    for (var i = 0; i < sel.options.length; i++) {
      if (sel.options[i].text === 'Surveillance Data') {
        sel.selectedIndex = i;
        $(sel).trigger('change');
        break;
      }
    }
  }
")
Sys.sleep(2)

# Screenshot 3: Files uploaded, ready to process
take_screenshot(b, "03-files-uploaded.png", delay = 2)

# Click Process Files
b$Runtime$evaluate("document.getElementById('process').click()")
take_screenshot(b, "04-combined-data.png", delay = 8)

# Navigate to Column Comparison tab
b$Runtime$evaluate("
  var tabs = document.querySelectorAll('#results_tabs .nav-link');
  for (var t of tabs) {
    if (t.textContent.includes('Column Comparison')) { t.click(); break; }
  }
")
take_screenshot(b, "05-discrepancies.png", delay = 4)

# Click Full Matrix sub-tab
b$Runtime$evaluate("
  var pills = document.querySelectorAll('.nav-pills .nav-link');
  for (var p of pills) {
    if (p.textContent.includes('Full Matrix')) { p.click(); break; }
  }
")
take_screenshot(b, "06-full-matrix.png", delay = 4)

# Click File Profiles sub-tab
b$Runtime$evaluate("
  var pills = document.querySelectorAll('.nav-pills .nav-link');
  for (var p of pills) {
    if (p.textContent.includes('File Profiles')) { p.click(); break; }
  }
")
take_screenshot(b, "07-file-profiles.png", delay = 4)

# Navigate to File Issues tab
b$Runtime$evaluate("
  var tabs = document.querySelectorAll('#results_tabs .nav-link');
  for (var t of tabs) {
    if (t.textContent.includes('File Issues')) { t.click(); break; }
  }
")
take_screenshot(b, "08-file-issues.png", delay = 3)

# Navigate to Explore Data tab
b$Runtime$evaluate("
  var tabs = document.querySelectorAll('#results_tabs .nav-link');
  for (var t of tabs) {
    if (t.textContent.includes('Explore Data')) { t.click(); break; }
  }
")
take_screenshot(b, "09-explore-data.png", delay = 5)

# --- Clean up -----------------------------------------------------------------
cat("\nCleaning up...\n")
b$close()
bg$kill()

cat("\nAll screenshots saved to:", normalizePath(img_dir), "\n")
cat("Files:\n")
cat(paste(" ", list.files(img_dir, pattern = "\\.png$")), sep = "\n")
cat("\nDone!\n")
