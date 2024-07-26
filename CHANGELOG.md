# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html). Dates formatted as YYYY-MM-DD as per [ISO standard](https://www.iso.org/iso-8601-date-and-time-format.html).

<!-- TODO: Delete the CHANGELOG entries for the reproduction template, but use their structure to create entries for your project, i.e.

## v0.1.0 - Date

**Contributors:** [Researcher name]

[One-sentence summary of release]

### Added

*

### Changed

*

### Removed

*

### Fixed

*
-->

## v1.0.4 - 2024-07-26

**Contributors:** Amy Heather

Minor improvements following the third reproducibility assessment ([Lim et al. 2020](https://github.com/pythonhealthdatascience/stars-reproduce-lim-2020)).

### Changed

* Add references to .yaml header in `reproduction_report.qmd`, `reproduction_success.qmd` and `study_publication.qmd` and set echo as false for `scope.qmd`
* Amended grammar in `reporting.qmd` bullet list
* Updated `references.bib`

## v1.0.3 - 2024-07-19

**Contributors:** Amy Heather

Minor improvements following the second reproducibility assessment ([Huang et al. 2019](https://github.com/pythonhealthdatascience/stars-reproduce-huang-2019)).

### Added

* Interactive and non-interactive time-to-completion figures add to `reproduction_success.qmd`, with `time_to_complete.py` producing figures
* Add R-related items to `.gitignore`
* Add option to not show limit on time from `calculate_times()`

### Changed

* Corrected spelling of "artefacts" in `badges.qmd` and add sentence to cite original study in `scope.qmd`
* Updated `references.bib`
* Add `matplotlib` to `requirements.txt`

## v1.0.2 - 2024-07-03

**Contributors:** Amy Heather

Minor improvements following the first reproducibility assessment ([Shoaib and Ramamohan 2022](https://github.com/pythonhealthdatascience/stars-reproduce-shoaib-2022)).

### Added

* Template page (`reflections.qmd`) for reflections on facilitators and barriers to reproduction, and for full list of troubleshooting steps

### Changed

* Add `.vscode/` to `.gitignore`
* Add citation to protocol on quarto site home page
* Order of criteria in `badges.qmd` (so it is a consistent order between the dictionary of criteria names and results, making it easier when conducting the assessment to look between the two)
* Amended `TODO:` comments in CHANGELOG and GHCR GitHub action
* Add missing `<!--- -->` marker in `artefacts.qmd`
* Improved the "succesfully reproduced" template sentence in `reproduction_report.qmd`

## v1.0.1 - 2024-06-19

**Contributors:** Amy Heather

Fix to citation file to prevent error on Zenodo.

### Fixed

* Completed `CITATION.cff` to reflect this repository (rather than to be a blank example), to prevent error when syncing with Zenodo

## v1.0.0 0 2024-06-19

**Contributors:** Amy Heather, with review of the protocol and associated artefacts (such as this template) from Tom Monks, Alison Harper and Nav Mustafee

Initial release of template repository. 🌱