# Text Analysis of Marketing Language in Financial Earnings Call
Transcripts

## Abstract

Earnings calls provide an opportunity to present a company’s financial
results, but they are also used to sell investors on the company’s
future success. When and how frequently marketing is referenced during
earnings calls remains an open question. We develop a marketing term
dictionary to use for text analysis within financial contexts. Employing
this dictionary, we conduct a text analysis of over 130,000 earnings
call transcripts from 2001-2020, exploring how frequently marketing
terms are used, the underlying sentiment that accompanies marketing term
usage, and how marketing terms relate to objective company performance
metrics such as earnings per share (EPS). We demonstrate that in
general, marketing terms are positively associated with positive
sentiment, inversely associated with negative sentiment, and positively
associated with higher firm performance, with stronger effects for
consumer focused and marketing-centric companies.

## Project Organization

- `/code` Scripts with prefixes (e.g., `01_import-data.R`,
  `02_clean-data.R`) and functions in `/code/src`.
- `/data` Simulated and real data, the latter not pushed.
- `/figures` PNG images and plots.
- `/output` Output from model runs, not pushed.
- `/presentations` Presentation slides.
- `/private` A catch-all folder for miscellaneous files, not pushed.
- `/renv` Project library, once initialized (see below).
- `/writing` Case studies and the paper.
- `renv.lock` Information on the reproducible environment.

## Reproducible Environment

Every package you install lives in your system library, accessible to
all projects. However, packages change. Add a reproducible environment
by creating a project library using the `{renv}` package.

- Initialize the project library *once* using `renv::init()`.
- Once you’ve installed packages, add them to the project library using
  `renv::snapshot()`.
- If a project library already exists, install the associated packages
  with `renv::restore()`.

For more details on using GitHub, Quarto, etc. see [Research Assistant
Training](https://github.com/marcdotson/ra-training).
