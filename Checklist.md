## Checklist

### [ ] 1. Data Collection

#### Primary Dataset: Crime Data
- [ ] https://daten.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002042_00004083.csv

#### Secondary Dataset: Income Data
- [ ] https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_317.csv
- [ ] Verify joinable with primary dataset

#### Tertiary Dataset: Accident Data
- [ ] https://daten.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00000718_00001783.csv
- [ ] Exclude language specific variables to reduce file size and enable Github upload
- [ ] Create date variable (if necessary)
- [ ] Verify joinable with primary dataset

- [ ] Verify dataset requirements:
  - [ ] Few hundred+ observations
  - [ ] 12+ variables
  - [ ] Date variables (YYYY-MM-DD format)
  - [ ] Geographic locations (municipality names/codes)
  - [ ] Mix of numeric and categorical variables
  - [ ] Document source and format

---

### [ ] 2. Data Preparation

- [ ] Load all datasets into R
- [ ] Inspect data structure (`str()`, `summary()`, `head()`)
- [ ] Handle missing values
- [ ] Standardize municipality names/codes for joining
- [ ] Convert date variables to proper format
- [ ] Create derived variables (e.g., crime rate per capita)
- [ ] **Document all cleaning steps in text (not just code comments)**

---

### [ ] 3. Data Merging

- [ ] Join primary and secondary datasets
- [ ] **HIGHLIGHT the joining code in report** ⭐
- [ ] Verify merge success
- [ ] Document join key and join type used

---

### [ ] 4. Exploratory Data Analysis

#### Summary Statistics
- [ ] Calculate descriptive statistics
- [ ] Create summary tables

#### Visualizations (5-6 different types)
- [ ] Crime rate distribution across municipalities
- [ ] Income distribution
- [ ] Scatter plot: Crime rate vs. Income
- [ ] Time series (if applicable)
- [ ] Geographic map of Canton Zurich
- [ ] Categorical comparisons

#### Plot Quality Checks
- [ ] No pie charts
- [ ] Use transparency (alpha) appropriately
- [ ] Avoid excessive stacked bar plots
- [ ] Show full axis ranges
- [ ] Consistent scales across comparable graphs
- [ ] Show distributions, not just means
- [ ] Keep plots simple and readable
- [ ] Add titles, labels, legends

---

### [ ] 5. Statistical Modeling

- [ ] Choose appropriate model (e.g., `lm()`, `glm()`, `t.test()`)
- [ ] Fit model(s)
- [ ] Display model summary
- [ ] **Interpret results in text**
- [ ] Create diagnostic plots
- [ ] Optional: Model comparison via CV

---

### [ ] 6. Chapter of Choice

- [ ] Select package NOT covered in main course
  - Suggestions: `sf`, `tmap`, `leaflet`, `gt`, `kableExtra`, `plotly`, `patchwork`, `corrplot`
- [ ] Implement creative analysis/visualization
- [ ] **Label section as "Chapter of Choice"** ⭐
- [ ] Explain added value

---

### [ ] 7. Generative AI Reflection (¼ to ½ page)

- [ ] Which AI tools used? (ChatGPT, Claude, etc.)
- [ ] What tasks were helpful?
- [ ] How did you verify correctness?
- [ ] What didn't work?
- [ ] What did you learn?

---

### [ ] 8. Story & Narrative

- [ ] Write compelling introduction
- [ ] State research question and objectives
- [ ] Articulate hypotheses
- [ ] Document data sources with citations
- [ ] Interpret each analytical step
- [ ] Draw conclusions
- [ ] Discuss limitations
- [ ] Provide recommendations

---

### [ ] 9. R Markdown Document

#### Setup
- [ ] Create `.Rmd` file (or `.qmd`)
- [ ] Configure YAML header
- [ ] Set global chunk options (echo=FALSE, message=FALSE, warning=FALSE)

#### Code Management
- [ ] Hide most code chunks
- [ ] **Show code for data joining**
- [ ] Show code for "Chapter of Choice"
- [ ] Use meaningful chunk names
- [ ] Comment complex sections

#### Sections
- [ ] Title page
- [ ] Introduction
- [ ] Data Sources & Preparation
- [ ] Exploratory Analysis
- [ ] Statistical Modeling
- [ ] **Chapter of Choice** (clearly labeled)
- [ ] Generative AI Reflection
- [ ] Conclusions & Limitations

#### Quality Control
- [ ] Document length: 15-25 pages (MAX 30)
- [ ] All plots have captions
- [ ] Tables well-formatted
- [ ] Text concise and engaging
- [ ] Knit successfully

---

### [ ] 10. Submission Package

#### File Structure
```
[GroupNumber]_[LastName1]_[LastName2].zip
├── README.txt
├── Data/
│   ├── crime_data.csv
│   ├── income_data.xlsx
│   └── [other data files]
├── Scripts/
│   └── analysis.Rmd
└── Output/
    └── final_report.html
```

#### README.txt
- [ ] Project title and authors
- [ ] Brief description
- [ ] Reproduction instructions
- [ ] R version and required packages
- [ ] Data sources
- [ ] Special notes

#### Final Checks
- [ ] Test reproducibility from scratch
- [ ] All paths relative (not absolute)
- [ ] All packages loaded
- [ ] Data files included
- [ ] Correct file naming
- [ ] Only one team member uploads
- [ ] Upload to correct Ilias folder
- [ ] **Submit before February 19, 2026, 5 PM**

## Notes & Ideas
