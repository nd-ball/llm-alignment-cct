# ACL ARR: Carefully Considering Culture: Analyzing LLM Alignment in Single- and Multi-Cultural Settings using Cultural Consensus Theory

## Python / Jupyter: Generate Table 1, Table 2, Welch tests and Figure 1

After running the R scripts to produce domain-level and country-level summaries, use the Jupyter notebooks in `ARR_March` to:

- Build **Table 1**: **Competence Scores** between **Single-culture** vs **Multi-culture** country groups (per domain).
- Build **Table 2**: **Agreement (CC)** and **VE Difference (LLM − Human)** between **Single-culture** vs **Multi-culture** country groups (per domain).
- Build **Figure 1**: Scatter plot (4 quadrants heatmap) to show trade-off between **Agreement (CC)** and **VE Difference (LLM − Human)** per domain.
- Build **Welch two-sample tests**: Separate tables to compare **Agreement (CC)** and **VE Difference (LLM − Human)** between **Single-culture** vs **Multi-culture** country groups (per domain).

Run these commands during the first run
- install.packages("here")
- install.packages("devtools")
- install_github('alastair-JL/AnthroTools')

### Expected outputs from R
Make sure `CCT_Competence.R` and `CCT_CC_VE.R` have created the domain/country summaries under:

- `ARR_March/Output/summaries/Competence`
- `ARR_March/Output/summaries/CC_VE`

Each domain is written as a separate CSV (e.g., `competence_EVN.csv` and `CC_VE_EVN.csv`).

### Notebooks
Run notebooks in this order:

1. **Build Table 1 (Competence Scores)**
   - Notebook: `ARR_March/Table1.ipynb`
   - Reads: per-domain CSVs from `ARR_March/summaries/`
   **Displays:** Table 1 in the notebook output

2. **Build Table 2 (Agreement (CC) and VE Difference: LLM − Human)**
   - Notebook: `ARR_March/Table2.ipynb`
   - Reads: per-domain CSVs from `ARR_March/summaries/`
   **Displays:** Table 2 in the notebook output

3. **Build Figure 1 (Agreement (CC) and VE Difference: LLM − Human Trade-off)**
   - Notebook: `ARR_March/Figure1.ipynb`
   - Reads: per-domain CSVs from `ARR_March/summaries/`
   **Displays:** Figure 1 in the notebook output

4. **Welch tests + FDR**
   - Notebook: `ARR_March/Welch_Tests.ipynb`
   - Runs Welch two-sample tests (Single vs Multi) per domain for:
     - CC (Agreement) → Table 6 stats
     - VE_Diff (LLM − Human) → Table 7 stats
   - Applies FDR correction across domains and **displays** final formatted tables

> Note: the Welch tests assume **5 countries in each group** (5 single / 5 multi).

### How to run Jupyter
From the repo root:

```bash
pip install -r requirements.txt
jupyter lab