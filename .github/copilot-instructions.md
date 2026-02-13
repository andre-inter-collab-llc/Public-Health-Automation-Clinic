# Copilot Instructions for Public Health Automation Clinic

## Project Overview

**Public Health Automation Clinic** is an open-source, local-first initiative that helps public health professionals automate repetitive manual workflows.

The repository packages three core assets:

1. **Clinic model and scope** in `chapters/13-automation-intake.qmd`
2. **Google Form intake blueprint** in `communications/google-forms-automation-clinic-blueprint.md`
3. **Launch and engagement messaging** in `communications/linkedin-automation-clinic-launch-post.md`

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
├── publish.bat
├── .github/
│   ├── copilot-instructions.md
│   └── workflows/
│       └── publish.yml
├── chapters/
│   └── 13-automation-intake.qmd
└── communications/
    ├── google-forms-automation-clinic-blueprint.md
    └── linkedin-automation-clinic-launch-post.md
```

## Publishing Model

- Local render: `quarto render`
- Local preview: `quarto preview`
- GitHub Pages publish: handled by `.github/workflows/publish.yml`
- Windows helper: `publish.bat` performs backup, render, and `quarto publish gh-pages --no-prompt`

## Content Scope Guidance

This repository focuses on:

- intake and triage workflow definitions
- automation use-case framing
- reusable communication assets

This repository does not aim to host full software products, large proprietary data, or organization-specific implementation details.

## Communications Guidance

When editing posts and blueprint content:

- keep audience focus on epidemiologists, registrars, analysts, and program teams
- emphasize repetitive task automation, quality improvement, and time recovery
- maintain clear expectations for free community support versus paid consulting
- keep solutions anonymous and generalizable
