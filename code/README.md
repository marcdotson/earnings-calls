# Code

Scripts with prefixes (e.g., `01_import-data.R`, `02_clean-data.R`) and
functions in `/code/source`.

- `01_data-wrangling.R` import, clean, and join the raw transcripts and
  financial performance data.
- `02_tokenizing.R` tokenize and remove stop words from complete
  `call_data`.
- `03_eda.R` visualizations and other exploratory data analysis.
- `04_word-embeddings.R` produce word embeddings for our corpus.
- `05_modeling.R` models using our data, beginning with clustering on
  word embeddings.
