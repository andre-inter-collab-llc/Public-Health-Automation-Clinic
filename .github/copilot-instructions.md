# Copilot Instructions for Public Health Automation Clinic

## Project Overview

**Public Health Automation Clinic** is an open-source, local-first initiative that helps public health professionals automate repetitive manual workflows. It is published as a Quarto Book to GitHub Pages.

The public repository packages:

1. **Preface and book front matter** in `index.qmd`
2. **Clinic model and scope** in `chapters/01-automation-intake.qmd` (and future chapters as the book grows)

Internal development assets are kept locally in `communications/` and are not part of public publishing scope.

### Core Philosophy

- **Local-first execution**: prioritize solutions that run on local machines using open tools.
- **Public-good publishing**: solutions are anonymized, generalized, and shared publicly.
- **Practical translation**: convert vague workflow pain points into clear, implementable automation steps.
- **Public health context first**: optimize for surveillance, reporting, quality checks, and workforce time savings.
- **Author Persona**: Content reflects André van Zyl's dual expertise (Epidemiologist & Data Scientist). Use **first-person voice** for personal sections (About the Author, Preface) and professional/instructional tone for chapter content.
- **Development Tools Separation**: The tools used to *build* this book (VS Code, GitHub Copilot, Quarto, Mermaid) should not be referenced in the main content; the focus is practical public health automation.

## Book Structure

**Project Type**: Quarto Book published to GitHub Pages.

### Front Matter
- **Cover/Index** (`index.qmd`): Book landing page with preface — the challenge, the solution, who this is for, how to use this book (full TOC), local-first philosophy, community vision, about the author.
- `index.qmd` is the **only place** that lists target audiences ("Who This Is For"). Chapter content should not duplicate this.

### Part I: The Clinic
1. **The Automation Clinic** (`chapters/01-automation-intake.qmd`) — The single comprehensive chapter covering: the problem (death by manual process), focus areas (data management, business process), local-first philosophy detail, the initiative model, what qualifies, submission formats and examples, how it works (Mermaid workflow), what to provide, setting expectations, paid services, the automation mindset, community of practice, CancerSurv examples, and the submission call to action.

### Part II: Desktop Automation
File renaming, format conversion, directory organization, batch operations on local files. The starting point for most automation journeys. Each solution published as a subchapter.

### Part III: Data Access and Integration
Connecting to databases, working with APIs (Google Sheets, REDCap, CDC data portals), importing and exporting data, transferring files between systems. Each solution published as a subchapter.

### Part IV: Data Cleaning and Transformation
Standardizing messy datasets, deduplication, edit rule validation, exception reporting. Turning raw data into analysis-ready inputs. Each solution published as a subchapter.

### Part V: Data Analysis and Reporting
Recurring surveillance analyses, reproducible reports with Quarto and R Markdown, interactive dashboards with Shiny, automated chart and table generation. Each solution published as a subchapter.

### Part VI: Business Process Automation
Project status reports, grant milestone tracking, workload distribution, cross-agency data exchange. Automating the work around the analytical work. Each solution published as a subchapter.

### Part VII: The Automation Mindset
7. **From Manual to Automated** — The optimization hierarchy (eliminate, automate, standardize). How to think about your own workflows. Getting started with scripting.
8. **Building a Community of Practice** — Contributing solutions. The pattern library vision. Workforce development. Public GitHub repository plans.
9. **Automation and Public Health Education** — Gaps in MPH curricula. R and Python as general-purpose tools beyond statistics. The case for teaching "automating the work around the work."

### Appendices
- **A: Tool Recommendations** — Commercial vs Open Source/Public Health tools comparison. When to recommend which tool.
- **B: Submission Templates and Examples** — Detailed templates for User Story, GPS, and Situational Protocol formats. Worked examples.
- **D: Development Tools and AI Transparency** — How the book is built, the role of AI in development, and the commitment to delivering AI-independent solutions.
- **C: Glossary** — Key terms for automation, public health data, and the tools ecosystem. **Always the last appendix in the book.**

### Back Matter
- **References** (if/when bibliography is added)

### Content Deduplication Rules
- **"Who This Is For"** lives only in `index.qmd`. Do not repeat the audience list in chapter content.
- **"The Challenge" summary** lives in `index.qmd` (brief hook). The detailed problem statement with concrete examples lives in Chapter 1.
- **"Local-First Philosophy"** has a brief summary in `index.qmd` and a detailed callout box in Chapter 1 under Focus Areas.
- **"Community of Practice"** has a brief summary in `index.qmd` and a detailed section in Chapter 1.

### Planned _quarto.yml Structure

```yaml
book:
  chapters:
    - index.qmd
    - part: "The Clinic"
      chapters:
        - chapters/01-automation-intake.qmd
    - part: "Desktop Automation"
      chapters:
        - chapters/02-desktop-automation.qmd
    - part: "Data Access and Integration"
      chapters:
        - chapters/03-data-access-integration.qmd
    - part: "Data Cleaning and Transformation"
      chapters:
        - chapters/04-data-cleaning-transformation.qmd
    - part: "Data Analysis and Reporting"
      chapters:
        - chapters/05-data-analysis-reporting.qmd
    - part: "Business Process Automation"
      chapters:
        - chapters/06-business-process-automation.qmd
    - part: "The Automation Mindset"
      chapters:
        - chapters/07-from-manual-to-automated.qmd
        - chapters/08-community-of-practice.qmd
        - chapters/09-automation-education.qmd
    - part: "Appendices"
      chapters:
        - chapters/A-tool-recommendations.qmd
        - chapters/B-submission-templates.qmd
        - chapters/D-development-tools.qmd
        - chapters/C-glossary.qmd  # Glossary is always last
```

> **Note**: The current content in `chapters/01-automation-intake.qmd` is the complete Chapter 1. Solutions Library parts (II–VI) each start with a single placeholder chapter. As submissions are received and solutions are published, additional subchapters will be added under the appropriate part. Mindset chapters (7–9) will be developed from content seeds in Chapter 1 and from community feedback.

## Repository Structure

```
Public-Health-Automation-Clinic/
├── _quarto.yml               # Book configuration (chapters, output formats, theme)
├── _brand.yml                 # Intersect Collaborations branding (colors, fonts, logo)
├── index.qmd                  # Book landing page / preface
├── README.md
├── LICENSE
├── .nojekyll
├── .gitignore
├── publish.bat                # Internal operational tooling (not public-facing)
├── .github/
│   ├── copilot-instructions.md
│   └── workflows/
│       └── publish.yml        # GitHub Actions workflow for auto-deployment
├── assets/
│   ├── branding/
│   │   ├── logos/
│   │   ├── icons/
│   │   └── images/
│   └── styles/
│       └── custom.scss
├── chapters/                  # Book chapters (.qmd files)
│   └── 01-automation-intake.qmd  # Chapter 1: The Automation Clinic
├── communications/            # Local internal only; not committed/published
│   ├── google-forms-automation-clinic-blueprint.md
│   └── linkedin-automation-clinic-launch-post.md
└── _site/                     # Generated output (in .gitignore)
```

## Publishing Model

- Local render: `quarto render`
- Local preview: `quarto preview`
- GitHub Pages publish: handled by `.github/workflows/publish.yml`
- Output directory: `_site/`

### GitHub Actions Workflow

The `.github/workflows/publish.yml` handles automated deployment. On push to `main`, the workflow:
1. Checks out the repository
2. Sets up Quarto
3. Renders and publishes to `gh-pages` branch

### Key Files for Publishing
- `.nojekyll`: Prevents GitHub from processing with Jekyll
- `.github/workflows/publish.yml`: GitHub Actions workflow
- `_publish.yml`: Created by first `quarto publish` run (if used)
- `_freeze/`: Stores computation results (commit to version control if present)

## Privacy and Handling Rules

- Treat everything in `communications/` as internal development context.
- Do not commit `communications/` files to GitHub and do not include them in public site publishing.
- `communications/` content should only be backed up to `GDRIVE_DEST=G:\My Drive\Intersect_Collaborations_LLC\04_projects\Public-Health-Automation-Clinic`.
- Treat `publish.bat` as internal operational tooling; do not surface it in public-facing documentation.

## Authoring and Editing Standards

### File Types
- Use `.qmd` for chapter-like content and any content that may include code execution or citations.
- Use `.md` for communications and operational docs.

### YAML Frontmatter
Every `.qmd` file must begin with YAML frontmatter containing at minimum a `title` field:
```yaml
---
title: "Page Title Here"
---
```

### Heading Hierarchy
- The first content heading in each document must use a single `#` (Heading 1).
- Do not manually number headings. Quarto's `number-sections: true` setting handles automatic numbering for HTML output.

### Writing Style
- Keep language clear and instructional, with examples grounded in public health operations.
- Include working external links when referencing tools, standards, programs, or organizations.
- Avoid adding unnecessary architecture or product features beyond the stated scope.
- **No Dashes for Punctuation**: Never use em dashes (—) or en dashes (–) in content. Rewrite sentences to use commas, colons, semicolons, or parentheses instead.

### Hyperlinks and External References
When mentioning organizations, companies, tools, or external resources, **include hyperlinks** to the relevant websites:
- **Companies and Organizations**: Link to main website or relevant subpage.
- **Tools and Software**: Link to the product or documentation page when first mentioned in a chapter.
- **Standards and Frameworks**: Link to the authoritative source.
- **Format**: Use standard Markdown link syntax: `[Display Text](URL)`. Prefer HTTPS URLs.
- **Maintenance**: When updating content, verify that existing hyperlinks remain valid.

### Diagrams (Mermaid)
- Use Mermaid (text-based) for workflow diagrams, process flows, and logic models.
- Use `flowchart TD` for system or process flows.
- Use `flowchart LR` for Inputs → Activities → Outputs → Outcomes patterns.

### Code Examples
- **Style**: Modern `tidyverse` conventions for R; `pandas`/standard library for Python.
- **Context**: Use public health examples (surveillance data, registry operations, reporting cycles) rather than generic business examples.
- **Local-first**: All code examples must run locally without cloud dependencies.

## Content Patterns

### User Story Formats for Public Health
Three alternative formats are used throughout the book:

1. **User Story** (As a [role], I want [action], so that [outcome])
2. **GPS (Given-Person-Should)**: "Given [context], the [role] should [action] to [outcome]."
3. **Situational Protocol**: "When [trigger], the [process/system] shall [action] within [constraint]."

### Tool Ecosystem Guidance
Always present tools with a public health context. Prioritize free and open source options:

| Category | Recommended Tools | Notes |
|:---------|:-----------------|:------|
| **Data Analysis** | R (tidyverse), Python (pandas) | Core clinic tools |
| **Reporting** | Quarto, R Markdown | Reproducible documents |
| **Interactive Tools** | R Shiny | Code provided; hosting not included |
| **IDE** | RStudio, VS Code | Free, full-featured |
| **Visualization** | ggplot2, Plotly, QGIS | QGIS for spatial epi |
| **Data Collection** | REDCap, KoBoToolbox | HIPAA-compliant options |
| **Project Management** | GanttProject, OpenProject | Free desktop tools |
| **Version Control** | Git, GitHub | For solution sharing |

### CancerSurv Case Study
Use CancerSurv examples in callout boxes to illustrate concepts:
```markdown
::: {.callout-note title="CancerSurv Example"}
In the CancerSurv project, [specific example]...
:::
```

## Content Scope Guidance

This repository focuses on:

- intake and triage workflow definitions
- automation use-case framing
- public, reusable workflow guidance
- a growing library of published automation solutions

This repository does not aim to host full software products, large proprietary data, or organization-specific implementation details.

## Communications Guidance

The `communications/` folder contains LinkedIn posts, form blueprints, and other outreach content. This content is **separate from the book** and not included in the Quarto build.

### File Naming Conventions
- **Form blueprints**: `google-forms-[topic].md`
- **Posts**: `linkedin-[topic]-post.md`
- **Articles**: `linkedin-article-[topic].qmd` (if Quarto rendering is needed)

### Content Guidelines
When editing posts and blueprint content:

- Keep audience focus on epidemiologists, registrars, analysts, and program teams.
- Emphasize repetitive task automation, quality improvement, and time recovery.
- Maintain clear expectations for free community support versus paid consulting.
- Keep solutions anonymous and generalizable.
- Keep files local-only under `communications/` and exclude them from public publication.
- **Tone**: Authoritative, insightful, accessible. Highlight common friction points and present automation as the practical solution.
- **Hashtags**: #PublicHealth #HealthInformatics #Automation #DataScience #Epidemiology #OpenSource #RStats #Python #PublicHealthData #ProcessImprovement #HealthIT

## Branding Configuration

The book uses Intersect Collaborations branding via `_brand.yml` for unified appearance. Brand elements (colors, fonts, logos) are defined in the repository root and automatically applied by Quarto.

### Logo Files
Place in `assets/branding/`:
- `logos/`: Main logo files
- `icons/`: Favicon and app icons
- `images/`: Cover image and other branded graphics
