---
title: "Google Forms Blueprint: Public Health Automation Clinic Intake Form"
---

# Public Health Automation Clinic Intake Form

Use this document as a blueprint to create the Google Form. Each section below maps to a form section with the specified question types.

---

## Form Settings

- **Title**: Public Health Automation Clinic Intake Form
- **Description**:

> Do you spend hours on repetitive, manual tasks that a script could handle in seconds? The Public Health Automation Clinic is a free service from Intersect Collaborations. Describe your problem, and we will develop a solution using free, open source tools, primarily R, Python, and the Posit ecosystem (RStudio, Quarto, Shiny).
>
> All solutions run on your computer. No cloud AI subscriptions required.
>
> **Before submitting**, please read the full initiative description here: https://andre-inter-collab-llc.github.io/Bridgeframe-Toolkit/chapters/13-automation-intake.html
>
> Submissions are anonymous by default. All fields marked with * are required.
>
> **Need urgent help?** Contact Intersect Collaborations directly for paid consulting: https://www.intersectcollaborations.com/contact

- **Collect email addresses**: No (anonymous by default)
- **Limit to 1 response**: No
- **Allow response editing**: Yes
- **Confirmation message**: "Thank you for your submission! Your problem has been received and will be reviewed as time and capacity allow. If you provided contact information, you may hear from us with follow-up questions. All solutions developed through the free service are anonymized, generalized, and published publicly so others facing similar challenges can benefit."

---

## Section 1: About You (Optional)

**Section Header**: Contact Information (Optional)
**Section Description**: You may submit anonymously. Provide contact information only if you would like follow-up on your specific problem.

### Q1: Name
- **Type**: Short text
- **Required**: No
- **Description**: "Your name (optional)"

### Q2: Email Address
- **Type**: Short text (email validation)
- **Required**: No
- **Description**: "e.g., your.email@example.com"

### Q3: Organization / Agency
- **Type**: Short text
- **Required**: No
- **Description**: "e.g., State Department of Health, County Health District"

### Q4: Role / Job Title
- **Type**: Short text
- **Required**: No
- **Description**: "e.g., Epidemiologist, Cancer Registrar, Data Analyst, Program Manager"

---

## Section 2: The Problem

**Section Header**: Describe Your Problem
**Section Description**: Tell us about the repetitive, tedious, or error-prone task you want to automate or streamline. The more detail you provide, the better we can help.

### Q5: Problem Title *
- **Type**: Short text
- **Required**: Yes
- **Description**: "Give your problem a brief title (e.g., 'Monthly lab report data extraction')"

### Q6: Problem Description *
- **Type**: Long text (paragraph)
- **Required**: Yes
- **Description**: "Describe the task in detail. What are you doing? What makes it tedious or error-prone? What would the ideal outcome look like? Example: Every month I receive ~200 PDF lab reports and manually type patient demographics into a spreadsheet. It takes about 8 hours and I frequently make typos..."

### Q7: Problem Category *
- **Type**: Checkboxes (select all that apply)
- **Required**: Yes
- **Options**:
  - Data entry / data extraction
  - Data cleaning / transformation
  - Data analysis / statistical reporting
  - File management (renaming, converting, organizing)
  - Report generation (epidemiological or surveillance)
  - Data quality checks / validation
  - Geocoding / mapping
  - Data merging / linking across sources
  - Business process automation (project reporting, grant tracking, workload analysis)
  - Other (please specify in the description above)

### Q8: Current Workflow Steps *
- **Type**: Long text (paragraph)
- **Required**: Yes
- **Description**: "Walk us through the steps you currently follow. Number each step if possible. Example: 1. Download PDF reports from the portal, 2. Open each PDF and locate patient name, DOB, diagnosis, 3. Type each field into Excel spreadsheet, 4. Repeat for each report..."

---

## Section 3: Volume and Frequency

**Section Header**: How Big Is This Problem?
**Section Description**: Understanding the scale helps us prioritize and design an appropriate solution.

### Q9: How often do you perform this task? *
- **Type**: Multiple choice
- **Required**: Yes
- **Options**:
  - Daily
  - Multiple times per week
  - Weekly
  - Biweekly
  - Monthly
  - Quarterly
  - Annually
  - Ad hoc / as needed

### Q10: Approximately how long does this task take each time?
- **Type**: Multiple choice
- **Required**: No
- **Options**:
  - Less than 30 minutes
  - 30 minutes to 1 hour
  - 1 to 3 hours
  - 3 to 8 hours (half to full day)
  - More than one full day
  - Multiple days

### Q11: Approximately how many records, files, or items are involved per cycle?
- **Type**: Short text
- **Required**: No
- **Description**: "e.g., 200 PDF files, 5,000 rows, 15 spreadsheets"

---

## Section 4: Your Technical Environment

**Section Header**: Tools and Technical Environment
**Section Description**: Knowing what tools you have access to helps us design a solution you can actually use.

### Q12: What software do you currently use for this task? *
- **Type**: Checkboxes (select all that apply)
- **Required**: Yes
- **Options**:
  - Microsoft Excel
  - Microsoft Access
  - Microsoft Word
  - Google Sheets
  - Google Docs
  - SAS
  - R / RStudio
  - Python
  - Epi Info
  - REDCap
  - SPSS
  - Stata
  - ArcGIS / QGIS
  - Power BI / Tableau
  - Other (please specify)

### Q13: What additional software do you have access to (even if not currently using for this task)?
- **Type**: Checkboxes (select all that apply)
- **Required**: No
- **Options**:
  - R / RStudio
  - Python (Anaconda, standalone, etc.)
  - Microsoft Power Automate
  - Microsoft Power Query (in Excel)
  - Google Apps Script
  - Command line / terminal access
  - SQL / database access
  - None of the above / Not sure

### Q13b: Does your agency permit use of AI/LLM tools?
- **Type**: Multiple choice
- **Required**: No
- **Description**: "Solutions from the clinic run locally and do not require AI access. However, knowing your environment helps us understand your constraints."
- **Options**:
  - Yes, we have approved AI tools (e.g., ChatGPT, Copilot, Claude, Gemini)
  - Yes, but only specific approved tools (please specify in 'Anything else' at the end)
  - No, AI tools are not permitted at my agency
  - I am not sure
  - Not applicable

### Q14: Can you install new software on your computer? *
- **Type**: Multiple choice
- **Required**: Yes
- **Options**:
  - Yes, I have full administrative access
  - Yes, but I need to request IT approval (and it is usually granted)
  - No, but my IT department can install software if I request it
  - No, my organization strictly limits software installation
  - I am not sure

### Q15: What operating system do you use?
- **Type**: Multiple choice
- **Required**: No
- **Options**:
  - Windows
  - macOS
  - Linux
  - Chromebook / Chrome OS
  - I am not sure

---

## Section 5: Sample Data and Permissions

**Section Header**: Sample Data
**Section Description**: A de-identified example of your input and expected output dramatically speeds up solution development. Do NOT send real patient data or personally identifiable information (PII).

### Q16: Can you point us to a publicly available example that resembles your data?
- **Type**: Long text (paragraph)
- **Required**: No
- **Description**: "Find a publicly available dataset or file online that resembles the structure of your data. This could be a sample dataset from a government open data portal (e.g., data.gov, CDC WONDER), Kaggle, or any publicly accessible file. Paste the URL here. Do NOT share your actual data, even if de-identified. If no public example exists, describe the data structure (column names, data types, number of rows, file format). Example: https://data.cdc.gov/... or 'My data has columns: FacilityID, ReportDate, CancerSite, Stage, PatientAge, Gender, with ~5,000 rows per quarter in CSV format'"

### Q17: Can you describe what the output should look like?
- **Type**: Long text (paragraph)
- **Required**: No
- **Description**: "Describe the expected result. Example: A clean Excel spreadsheet with columns: PatientID, Facility, DiagnosisDate, CancerSite, Stage..."

---

## Section 6: Follow-Up

**Section Header**: Follow-Up
**Section Description**: All solutions from the free service are anonymized, generalized, and published publicly. No identifying details about you or your organization are included. If you need a private solution, contact Intersect Collaborations for paid consulting.

### Q18: Would you be willing to test a proposed solution and provide feedback?
- **Type**: Multiple choice
- **Required**: No
- **Options**:
  - Yes (requires providing contact info above)
  - No, just publish the solution when it is ready
  - I submitted anonymously

### Q19: Anything else we should know?
- **Type**: Long text (paragraph)
- **Required**: No
- **Description**: "Any additional context, constraints, deadlines, or details that would help us understand your problem."

---

## Form Logic Notes

- If Q16 = "Yes, I will upload a sample with this form" → Show Q17
- If Q16 = any other option → Hide Q17 (optional: still show but note it is optional)
- If Q18 = "Yes" but Q2 (email) is blank → Show validation message: "Please provide your email address in Section 1 if you would like to test solutions and provide feedback."

---

## Google Forms Creation Checklist

- [ ] Create form at https://forms.google.com
- [ ] Set title and description text from above
- [ ] Create all 6 sections with headers and descriptions
- [ ] Add all questions with correct types and options (including Q13b for AI/LLM)
- [ ] Mark required fields (Q5, Q6, Q7, Q8, Q9, Q12, Q14)
- [ ] Enable file upload for Q17 is no longer needed (replaced with URL/text field)
- [ ] Configure confirmation message
- [ ] Set response destination (Google Sheets for tracking)
- [ ] Test form end-to-end
- [ ] Copy shareable link and update chapter 13 placeholder URL
- [ ] Update LinkedIn post with form link
