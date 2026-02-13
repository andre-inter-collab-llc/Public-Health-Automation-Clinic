# Public Health Automation Clinic

Public Health Automation Clinic is an open-source initiative to help public health professionals automate repetitive manual tasks with local-first tools.

## What this repository contains

- A standalone chapter: `chapters/13-automation-intake.qmd`
- Intake form implementation blueprint: `communications/google-forms-automation-clinic-blueprint.md`
- Launch and engagement messaging: `communications/linkedin-automation-clinic-launch-post.md`

## Core idea

1. People submit repetitive workflow problems.
2. Solutions are built using free tools, primarily R and Python.
3. Solutions are anonymized and published so others can reuse them.

## Local preview

Prerequisite: [Quarto](https://quarto.org/docs/get-started/)

```bash
quarto preview
```

## Render

```bash
quarto render
```

## Publish

This repository includes a GitHub Actions workflow for publishing to GitHub Pages from `main`.

For a one-command local workflow on Windows (backup, render, publish):

```bat
publish.bat
```

Useful options:

```bat
publish.bat --render-only
publish.bat --skip-backup
publish.bat --backup-only
publish.bat --dest "G:\My Drive\Intersect_Collaborations_LLC\04_projects\Public-Health-Automation-Clinic"
```

## Contributing

Issues and pull requests are welcome.

## License

This project is licensed under the MIT License. See `LICENSE`.
