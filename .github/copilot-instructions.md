# Copilot Instructions for Public Health Automation Clinic

## Project Overview

**Public Health Automation Clinic** is an open-source, local-first initiative that helps public health professionals automate repetitive manual workflows.

The public repository packages one core public asset:

1. **Clinic model and scope** in `chapters/13-automation-intake.qmd`

Internal development assets are kept locally in `communications/` and are not part of public publishing scope.

## Core Philosophy

- **Local-first execution**: prioritize solutions that run on local machines using open tools.
- **Public-good publishing**: solutions are anonymized, generalized, and shared publicly.
- **Practical translation**: convert vague workflow pain points into clear, implementable automation steps.
- **Public health context first**: optimize for surveillance, reporting, quality checks, and workforce time savings.

## Authoring and Editing Standards

- Use `.qmd` for chapter-like content and `.md` for communications and operational docs.
- Keep language clear and instructional, with examples grounded in public health operations.
- Include working external links when referencing tools, standards, programs, or organizations.
- Avoid adding unnecessary architecture or product features beyond the stated scope.

## Repository Structure

```
Public-Health-Automation-Clinic/
├── _quarto.yml
├── index.qmd
├── README.md
├── LICENSE
├── .gitignore
├── .nojekyll
├── .github/
│   ├── copilot-instructions.md
│   └── workflows/
│       └── publish.yml
├── chapters/
│   └── 13-automation-intake.qmd
└── communications/ (local internal only; not committed/published)
```

## Publishing Model

- Local render: `quarto render`
- Local preview: `quarto preview`
- GitHub Pages publish: handled by `.github/workflows/publish.yml`

## Privacy and Handling Rules

- Treat everything in `communications/` as internal development context.
- Do not commit `communications/` files to GitHub and do not include them in public site publishing.
- `communications/` content should only be backed up to `GDRIVE_DEST=G:\My Drive\Intersect_Collaborations_LLC\04_projects\Public-Health-Automation-Clinic`.
- Treat `publish.bat` as internal operational tooling; do not surface it in public-facing documentation.

## Content Scope Guidance

This repository focuses on:

- intake and triage workflow definitions
- automation use-case framing
- public, reusable workflow guidance

This repository does not aim to host full software products, large proprietary data, or organization-specific implementation details.

## Communications Guidance

When editing posts and blueprint content:

- keep audience focus on epidemiologists, registrars, analysts, and program teams
- emphasize repetitive task automation, quality improvement, and time recovery
- maintain clear expectations for free community support versus paid consulting
- keep solutions anonymous and generalizable
- keep files local-only under `communications/` and exclude them from public publication
