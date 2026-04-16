# Carefully Considering Culture: Analyzing LLM Alignment in Single- and Multi-Cultural Settings using Cultural Consensus Theory

Krishna Pothugunta, John P. Lalor
Department of IT, Analytics, and Operations, University of Notre Dame

[[`Paper`](TODO-ARXIV-URL)] [[`BibTeX`](#citation)]

This repository contains the code and data for the paper *Carefully Considering Culture: Analyzing LLM Alignment in Single- and Multi-Cultural Settings using Cultural Consensus Theory*. We apply Cultural Consensus Theory (CCT) from cultural anthropology to evaluate how well Large Language Models (LLMs) align with human cultural consensus. Using the World Values Survey (WVS) Wave 7 across 10 countries and 12 domains, we compare an ensemble of 10 LLMs against human respondents and identify three distinct regimes of model behavior: *Consensus Gap*, *Heterogeneity Gap*, and *Consensus Inflation*.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Data Setup](#data-setup)
- [Reproducing Paper Results](#reproducing-paper-results)
- [License](#license)
- [Citation](#citation)
- [Acknowledgments](#acknowledgments)

## Overview

Standard approaches to evaluating LLM cultural alignment rely on group-level aggregation or distributional metrics, which obscure intra-group heterogeneity. CCT instead models culture as a distribution of shared meanings, estimating both (i) a group-level consensus answer for each survey item and (ii) each respondent's cultural competence score, defined as the degree of alignment with that consensus. This repository provides the full pipeline to reproduce our analysis: collecting WVS responses from an ensemble of LLMs, fitting CCT models jointly on humans and LLMs, and computing the two alignment metrics introduced in the paper — Consensus Consistency (CC) and Difference in Variance (∆VE).

The ensemble consists of 10 models (GPT-OSS:120B, Llama3.1:70B, Llama3:70B, Qwen2.5vl:72B, Qwen2.5vl:32B, Qwen2.5vl:7B, Qwen3:32B, Qwen:7B, Phi3:instruct, and GPT-4o), each queried with 6 prompt templates (baseline, explicit, parentheses, directive, chain-of-thought, roleplay) for each of the 10 countries. Countries are grouped into *single-culture* (Japan, Armenia, Germany, Greece, Netherlands) and *multi-culture* (Colombia, Mexico, Malaysia, Peru, United States) based on the Ethnic Fractionalization Index.

## Installation

The pipeline uses both R (for CCT fitting via AnthroTools) and Python (for data processing, statistical testing, and figure generation). Python 3.10+ and R 4.0+ are required.

### Python dependencies

```bash
pip install -r requirements.txt
```

### R dependencies

From an R session, install the following packages on first run:

```r
install.packages("here")
install.packages("devtools")
devtools::install_github("alastair-JL/AnthroTools")
```

We use AnthroTools version 2.0 for CCT estimation.

## Data Setup

### World Values Survey (WVS)

We use WVS Wave 7, which is freely available to registered users. To access the data:

1. Register at the [WVS website](https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp) and download the Wave 7 country-pooled datafile.
2. Place the downloaded file in `TODO-DATA-FOLDER/human/` (see the full list of 65 participating countries in Appendix A of the paper).
3. Run the preprocessing script to filter respondents with missing values and to exclude domains with fewer than four items:

```bash
python TODO-PREPROCESS-SCRIPT.py
```

This produces the human response matrices H of shape (N_H × M) for each country-domain pair, where M is the number of questions in the domain.

### LLM responses

The full set of LLM responses used in the paper is already provided in this repository under `TODO-DATA-FOLDER/llm/`. The folder is organized by domain and country, with one CSV file per country-domain pair containing responses from all 10 models across all 6 prompt templates:

```
TODO-DATA-FOLDER/llm/
├── <DOMAIN_1>/
│   ├── <COUNTRY_1>_10LLMs.csv
│   ├── <COUNTRY_2>_10LLMs.csv
│   └── ... (10 countries)
├── <DOMAIN_2>/
│   └── ...
└── ... (12 domains)
```

Each `<COUNTRY>_10LLMs.csv` file contains the LLM response matrix L of shape (60 × M), where 60 = 10 models × 6 prompt templates and M is the number of questions in the domain. The 10 models are GPT-OSS:120B, Llama3.1:70B, Llama3:70B, Qwen2.5vl:72B, Qwen2.5vl:32B, Qwen2.5vl:7B, Qwen3:32B, Qwen:7B, Phi3:instruct, and GPT-4o. The 6 prompt templates (baseline, explicit, parentheses, directive, chain-of-thought, roleplay) are described in Appendix A of the paper. All responses were collected with temperature set to 0 via a university-hosted Open WebUI instance for the open-source models and via the OpenAI API for GPT-4o.

## Reproducing Paper Results

After preparing the human and LLM response matrices, reproduce the paper's results in the following order.

### Step 1: Fit CCT models (R)

Run the two R scripts to fit the CCT models and write per-domain summaries.

```bash
Rscript TODO-R-SCRIPTS-FOLDER/CCT_Competence.R
Rscript TODO-R-SCRIPTS-FOLDER/CCT_CC_VE.R
```

`CCT_Competence.R` fits a joint CCT model on `J = [H, L]` and writes per-respondent competence scores to `TODO-OUTPUT-FOLDER/Competence/competence_<DOMAIN>.csv`. `CCT_CC_VE.R` fits separate CCT models on H and L, computes CC (Equation 3) and ∆VE (Equation 4), and writes the results to `TODO-OUTPUT-FOLDER/CC_VE/CC_VE_<DOMAIN>.csv`.

### Step 2: Generate paper tables and figures (Jupyter)

From the repository root:

```bash
jupyter lab
```

Run the notebooks in the following order.

1. `Figure1.ipynb` — Produces Paper Figure 1, the per-domain mean competence heatmaps for humans and the 10 models, aggregated over single-culture and multi-culture countries.
2. `Table2.ipynb` — Produces Paper Table 2, the mean CC and ∆VE by domain, split by single- vs. multi-culture grouping.
3. `Figure2.ipynb` — Produces Paper Figure 2, the CC vs. ∆VE scatter plot that visualizes the three regimes (Consensus Gap, Heterogeneity Gap, Consensus Inflation).
4. `Table3.ipynb` — Runs Welch two-sample t-tests comparing single- vs. multi-culture groups on CC and ∆VE, applies Benjamini–Hochberg FDR correction across the 12 domains, and produces Paper Table 3.

The Welch tests assume 5 countries per group (5 single-culture, 5 multi-culture). To reproduce the full appendix versions of these statistics (Tables 7 and 8), run the corresponding notebooks in `TODO-NOTEBOOKS-FOLDER/appendix/`.

## License

TODO-LICENSE (e.g., MIT, Apache 2.0).

## Citation

If you use this code or the findings from our paper in your research, please cite:

```bibtex
@inproceedings{pothugunta2025cct,
  title     = {Carefully Considering Culture: Analyzing {LLM} Alignment in Single- and Multi-Cultural Settings using Cultural Consensus Theory},
  author    = {Pothugunta, Krishna and Lalor, John P.},
  year      = {2025},
  note      = {TODO-VENUE}
}
```

## Acknowledgments

This material is based upon work supported by the National Science Foundation under Grant No. IIS-2403438, as well as the Center for Research Computing, the Human-centered Analytics Lab, and the Mendoza College of Business at the University of Notre Dame. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the authors and do not necessarily reflect the views of the National Science Foundation or the University of Notre Dame.
