# Biodiversity Assessment in Mangrove Forests in Kenya

## Overview

Mangrove forests support diverse biological communities and provide important ecological functions and services. However, biodiversity assessments in these ecosystems often focus on selected taxa and locations, limiting the completeness of biodiversity inventories and the ability to assess patterns of biodiversity over time.

This repository contains analysis scripts for a study evaluating the state of mangrove biodiversity assessment in Kenya and examining the potential of environmental DNA (eDNA) metagenomics to complement conventional biodiversity surveys.

The study combines a systematic literature review of published biodiversity records with eDNA-based analysis. The literature review synthesises existing records to characterise taxonomic representation, the geographical distribution of assessments, survey methods, and temporal patterns in biodiversity documentation. The eDNA analysis evaluates taxonomic composition, habitat associations, and the correspondence between molecular detections and historically documented biodiversity.

The study aims to inform the development of more comprehensive biodiversity inventories and monitoring approaches for Kenya's mangrove forests.

## Study summary

The systematic literature review identified 27 publications documenting biodiversity in 15 mangrove forest formations in Kenya. Four forests, representing approximately 6% of the country's mangrove cover, accounted for 63.12% of the publications. The review documented 1,044 taxa belonging to 255 families, with *Teleostei*, *Aves*, *Chromadorea*, and *Malacostraca* accounting for 84.5% of recorded taxa.

The eDNA analysis generated approximately 5.93 million contigs, of which 1.65% were clustered into operational taxonomic units (OTUs) at a sequence similarity threshold of at least 98%. Following filtering, 2,330 OTUs were retained for analysis. After excluding unexpected OTUs, 502 OTUs belonging to 305 families remained. Among 320 animal OTUs, 63.13% belonged to *Teleostei*.

Comparison of marine-associated biodiversity records identified complementary taxonomic coverage between the literature review and eDNA analysis. Sixty-seven families were shared between the two approaches, while 233 were detected exclusively through eDNA and 183 were documented exclusively in the literature.

These findings indicate that published records provide an incomplete representation of mangrove biodiversity in Kenya. They also demonstrate the potential of eDNA to complement conventional surveys, although gaps in reference databases constrain taxonomic resolution.

*The numerical results presented here are from the study abstract and should be interpreted in conjunction with the methods, filtering criteria, and results reported in the manuscript.*

## Repository contents

The repository contains the main Quarto analysis document and supporting R scripts.

| File or directory          | Description                                                                                                                                     |
| -------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `eDNA_analysis_script.qmd` | Main Quarto document describing the data processing, statistical analyses, and visualisation workflow for the literature review and eDNA study. |
| `scripts/`                 | Supporting R scripts used for specific analytical tasks, including spatial mapping and other data-processing or analytical procedures.          |

The Quarto document includes code for analysing historical biodiversity records, summarising survey methods, examining spatial and temporal patterns, validating taxonomic records, and analysing eDNA-derived biodiversity data.

The precise role of each supporting script should be checked against its contents before individual scripts are run independently.

## Analytical workflow

The analysis is organised into two complementary components.

### 1. Systematic literature review

The literature review component synthesises published biodiversity records from Kenyan mangrove forests.

Analyses include:

* Summarising the number of publications and their distribution among mangrove forest formations.
* Characterising historically documented taxa at different taxonomic levels.
* Examining the taxonomic groups represented in existing assessments.
* Summarising the survey methods used in historical biodiversity studies.
* Evaluating the spatial distribution of biodiversity assessments and the concentration of publications among sites.
* Examining temporal patterns in publication frequency and the recording of taxa.
* Assessing differences in taxonomic representation among sites using contingency tables and chi-square tests.
* Evaluating the distribution of assessment effort using measures such as the Gini coefficient and Lorenz curve.
* Examining temporal trends in newly documented taxa using the Mann–Kendall test.

The analyses combine descriptive statistics, graphical summaries, and statistical tests to characterise the distribution and development of mangrove biodiversity knowledge in Kenya.

### 2. Environmental DNA analysis

The eDNA component processes molecular biodiversity data and evaluates their contribution to existing biodiversity inventories.

#### Data processing and taxonomic validation

The workflow includes:

* Importing and cleaning taxonomic and sequence-derived data.
* Merging abundance tables and associated taxonomic information.
* Applying filtering criteria to remove low-abundance or otherwise excluded records.
* Standardising taxonomic names and completing taxonomic information where possible.
* Assigning habitat categories to detected taxa.
* Comparing eDNA-derived taxonomic records with regional biodiversity inventories.
* Checking taxonomic names against reference sources, including the Global Biodiversity Information Facility (GBIF), the World Register of Marine Species (WoRMS), the Ocean Biodiversity Information System (OBIS), and FishBase, where applicable.
* Classifying records as expected or unexpected relative to the reference inventory used in the analysis.

Taxonomic validation depends on the completeness of reference inventories and the availability of reliable taxonomic assignments. An unmatched detection should not automatically be interpreted as a newly recorded species, because incomplete reference data and differences in taxonomic resolution can also explain unmatched records.

#### Biodiversity and community analyses

The downstream analyses examine the taxonomic composition and distribution of eDNA-derived detections. These include:

* Summarising taxonomic richness across major taxonomic groups.
* Examining habitat associations among detected taxa.
* Comparing taxonomic representation among sampling locations and sample types.
* Assessing alpha diversity using indices such as Shannon diversity, Simpson diversity, and Chao1 richness.
* Evaluating differences in diversity among sites and sample types using the statistical procedures implemented in the analysis.
* Generating figures and summary tables to describe taxonomic composition and biodiversity patterns.

The workflow uses `phyloseq` and `microeco` data structures for parts of the downstream microbiome and biodiversity analyses.

### 3. Comparison of historical and molecular records

The final analytical component compares the taxonomic coverage of the literature review and eDNA datasets.

The comparison identifies:

* Taxonomic groups documented by both approaches.
* Taxa or families recorded exclusively in the literature.
* Taxa or families detected exclusively through eDNA.
* Differences in taxonomic representation that may reflect the complementary coverage of conventional and molecular surveys.

The comparison is intended to evaluate the contribution of eDNA to biodiversity documentation rather than establish that molecular detections are equivalent to confirmed species occurrences.

## Data availability

## Data availability

The eDNA sequencing data are associated with NCBI BioProject **PRJNA1543622**, subject to the release status of the associated records. Other data supporting the findings of this study are available from the corresponding author upon reasonable request.

The analysis scripts are available in this repository. Reproducing the analyses also requires the relevant input data and supporting files in the directory structure expected by the scripts.

The Quarto document also refers to processed datasets and reference inventories stored in local directories. These include files under `data/` and `inventory_data/`, such as taxonomic tables, literature-review records, habitat classifications, and biodiversity inventory files.

**The complete analysis cannot be run from the Quarto document and supporting scripts alone if these input files are unavailable.** Before attempting to reproduce the analyses, ensure that the required datasets are accessible and placed in the directory structure expected by the code.

Where input datasets cannot be redistributed, their sources and access procedures should be documented separately.

## Software requirements

The analyses were developed in R and documented using Quarto.

### Required software

* [R](https://www.r-project.org/)
* [RStudio](https://posit.co/download/rstudio-desktop/) or another R-compatible development environment
* [Quarto](https://quarto.org/) for rendering the `.qmd` document

An R version compatible with the required packages should be used. The exact R and package versions should be recorded for a reproducible release of the workflow.

### R packages

The Quarto document uses packages from the R ecosystem for data processing, statistical analysis, taxonomic validation, biodiversity analysis, and visualisation.

Packages referenced in the code include:

**Data processing and visualisation**

* `tidyverse`
* `janitor`
* `readr`
* `dplyr`
* `tidyr`
* `purrr`
* `stringr`
* `lubridate`
* `reshape2`
* `ggrepel`
* `ggpubr`
* `ggpmisc`
* `patchwork`
* `pheatmap`
* `treemapify`
* `viridis`
* `viridisLite`
* `colorspace`
* `ggsankey`
* `ggsankeyfier`
* `ggExtra`

**Statistical analysis**

* `ineq`
* `Kendall`
* `trend`
* `FactoMineR`
* `factoextra`
* `broom`
* `vcd`
* `gtsummary`

**Taxonomic and biodiversity analysis**

* `phyloseq`
* `biomformat`
* `rgbif`
* `worrms`
* `microeco`
* `metacoder`
* `aplot`
* `magrittr`
* `gt`

**Package management**

* `pacman`

These packages are drawn from the package-loading commands in the analysis document. Additional packages may be required by supporting scripts.

### Installing packages

The `pacman` package can be installed from the R console if it is not already available:

```r
install.packages("pacman")
```

The packages required by the main document can then be installed using the package-loading commands in the relevant code sections. For example:

```r
pacman::p_load(
  tidyverse,
  janitor,
  ggpubr,
  ggpmisc,
  ggrepel,
  broom,
  patchwork,
  viridis,
  Kendall,
  trend,
  FactoMineR,
  factoextra,
  pheatmap,
  reshape2
)
```

The eDNA section loads additional packages, including `phyloseq`, `biomformat`, `rgbif`, `worrms`, and `microeco`. Install these packages before running the corresponding sections.

The package list above is not a substitute for a tested dependency lockfile. Package versions and dependencies should be checked against the complete code before attempting a fresh installation.

## Running the analysis

### 1. Obtain the repository

Clone the repository from GitHub:

```bash
git clone https://github.com/fredoket/Biodiversity-assessment-in-mangrove-forests-in-Kenya.git
```

Move into the repository directory:

```bash
cd Biodiversity-assessment-in-mangrove-forests-in-Kenya
```

Alternatively, download the repository as a ZIP archive from GitHub.

### 2. Prepare the input data

Ensure that all required input datasets are available and stored in the locations referenced by the Quarto document.

The code uses relative paths, including paths under:

```text
data/
inventory_data/
scripts/
```

The `scripts/` directory should contain the supporting scripts referenced by the main document. In particular, the study-area mapping section calls `scripts/eDNA_sampling_area_map.R`.

If the required data files are stored elsewhere, either place them in the expected locations or modify the relevant file paths in the code.

### 3. Open the Quarto document

Open `eDNA_analysis_script.qmd` in RStudio or another environment that supports Quarto.

Review the setup and data-import sections to confirm that the required packages and input files are available.

### 4. Run the analyses

Run the document sequentially, following the order of the code chunks. Some sections rely on objects or datasets created in earlier sections.

The eDNA workflow includes data preparation and taxonomic validation steps that may require an internet connection to query external taxonomic services. These operations can take substantial time and memory when processing large taxon lists.

Where the required processed dataset has already been generated and is available, the relevant downstream analyses may be run directly using that dataset, as described in the Quarto document.

### 5. Render the document

From the repository root, render the Quarto document using:

```bash
quarto render eDNA_analysis_script.qmd
```

The document specifies HTML output. Successful rendering depends on the availability of the required data, packages, supporting scripts, and any external services used by the analysis.

Some code chunks save figures to directories such as `figures/` or to an output directory defined by a supporting script. Ensure that these directories exist or are created by the code before saving results.

## Outputs

Depending on the sections executed and the availability of the required input data, the workflow generates or displays:

* Summaries of publications and historical biodiversity records.
* Taxonomic richness summaries at different taxonomic levels.
* Summaries of survey methods and their representation among taxonomic groups.
* Spatial and temporal summaries of biodiversity assessments.
* Statistical test results and trend analyses.
* Taxonomic validation and habitat-classification tables.
* eDNA-derived taxonomic and diversity summaries.
* Figures illustrating biodiversity patterns, publication frequency, temporal trends, and taxonomic composition.

The exact output files depend on the code sections executed and the output paths configured in the Quarto document and supporting scripts.

## Reproducibility and limitations

Several considerations are important when reproducing or extending these analyses:

1. **Input data availability:** The scripts depend on processed data and reference inventories that may not be included in the repository. Reproduction requires access to the relevant files.
2. **External databases:** Taxonomic validation may depend on online services and reference databases. Database contents and taxonomic assignments can change over time.
3. **Computational requirements:** Some data-merging and taxonomic validation operations may require substantial memory, particularly when processing large lists of taxa.
4. **Taxonomic uncertainty:** Reference database gaps and differences in taxonomic resolution can affect the interpretation of eDNA detections.
5. **Analytical dependencies:** Some sections depend on objects or datasets created earlier in the workflow. Running isolated code chunks may therefore require additional preparation.
6. **Version control:** The R version, package versions, input data versions, and external database access dates should be documented to improve reproducibility.

Environmental DNA is treated here as a complementary source of biodiversity information. Molecular detections should be interpreted in the context of sampling design, taxonomic assignment confidence, reference database coverage, and conventional biodiversity records.

## Citation

If you use this code or adapt the workflow in your research, please cite the associated study once its publication details are available.

**Manuscript title:** *State of mangrove biodiversity assessment in Kenya and the potential of environmental DNA in complementing surveys.*

A complete citation, including authors, publication year, journal, and DOI, should be added following publication.

For reuse of specific scripts, please cite the associated study and acknowledge the original source of the code.

## Licence

A software licence has not yet been specified. The terms under which the code may be reused, modified, or redistributed should be stated here once a licence has been selected and added to the repository.

## Contact

For questions about the analytical workflow, data requirements, or implementation, contact the corresponding author through the contact information provided in the associated manuscript.
