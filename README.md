# Public Health Automation Clinic

[![Quarto Publish](https://github.com/andre-inter-collab-llc/Public-Health-Automation-Clinic/actions/workflows/publish.yml/badge.svg)](https://github.com/andre-inter-collab-llc/Public-Health-Automation-Clinic/actions/workflows/publish.yml)

**Public Health Automation Clinic** is an open-source, local-first initiative that helps public health professionals automate repetitive manual workflows. It is published as a [Quarto Book](https://quarto.org/docs/books/) to GitHub Pages.

**Published book:** <https://andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic/>

## Core Idea

1. Public health professionals submit repetitive workflow problems.
2. Solutions are built using free, open-source tools, primarily R and Python.
3. Solutions are anonymized, generalized, and published so others can reuse them.

## Book Structure

| Part | Description |
|:-----|:------------|
| **The Clinic** | The initiative model, what qualifies, how to submit, and what to expect. |
| **Desktop Automation** | File renaming, format conversion, directory organization, batch operations. |
| **Data Access and Integration** | Connecting to databases, APIs, importing/exporting data. |
| **Data Cleaning and Transformation** | Standardizing datasets, deduplication, validation, exception reporting. |
| **Data Analysis and Reporting** | Reproducible reports, dashboards, automated charts and tables. |
| **Business Process Automation** | Status reports, grant tracking, workload distribution. |
| **The Automation Mindset** | The optimization hierarchy, community of practice, workforce development. |
| **Appendices** | Tool recommendations, submission templates, development tools, glossary. |

## Published Solutions

| Solution | Chapter | App |
|:---------|:--------|:----|
| Stack and Check Spreadsheets | `chapters/04a-stack-and-check-spreadsheets.qmd` | `apps/stack-and-check-spreadsheets/` |

## Submit a Problem

See [Chapter 1: The Automation Clinic](https://andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic/chapters/01-automation-intake.html) for submission options and detailed guidance on how to describe your problem.

## Local Preview

**Prerequisite:** [Quarto](https://quarto.org/docs/get-started/)

```bash
quarto preview
```

## Render

```bash
quarto render
```

## Built with AI. Owned by You.

This project uses AI tools (GitHub Copilot) to accelerate development, but every published solution is standard R or Python code that runs locally without any AI dependency. No AI subscription, cloud API, or large language model is required to use the solutions. For full details on the technology stack, AI-assisted development practices, and authorship principles, see [Appendix D: Development Tools and AI Transparency](https://andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic/chapters/D-development-tools.html).

## Contributing

Issues and pull requests are welcome. See the book's [submission guidance](https://andre-inter-collab-llc.github.io/Public-Health-Automation-Clinic/chapters/01-automation-intake.html) for how to submit a workflow problem.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE).
