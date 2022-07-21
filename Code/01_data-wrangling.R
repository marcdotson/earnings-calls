# Earnings Calls ----------------------------------------------------------
# Load packages.
library(tidyverse)
library(GICS) # Install using devtools::install_github("bautheac/GICS").

# Import all earning call transcripts and separate the doc_id.
transcripts <- read_rds(here::here("Data", "transcripts.rds")) |> 
  tibble() |> 
  separate(doc_id, into = c("gvkey", "call_date", "title"), sep = "_")

# Import copywrite statements to exclude from the transcripts.
copywrite <- read_rds(here::here("Data", "copywrite_statements.rds"))

# Clean and filter so we have transcripts that have the correct quarter and year.
call_data <- transcripts |> 
  mutate(
    title = str_to_lower(title),                                     # Make titles lowercase.
    text = str_replace_all(text, "\r?\n|\r", " "),                   # Remove carriage returns.
    first_line = str_trunc(text, 1000) |> str_to_lower()             # Extract the first line.
  ) |> 
  # Filter transcripts that are unlikely to be earning calls.
  anti_join(
    bind_rows(
      filter(transcripts, grepl("abstract|(event brief)", title)),   # Filter transcripts with "abstract" or "event brief".
      filter(transcripts, grepl(str_c(
        "(full year)|annual", "|",                                   # Filter transcripts with "full year", "annual" or
        "preliminary|interim|fiscal", "|",                           # "preliminary", "interim", "fiscal" or
        "(half year)|(year end)|yearend"                             # "half year", "year end", or "yearend"
      ), title), !grepl("quarter|q\\d|q\\d\\d|\\dq|\\d\\dq", title)) # but without "quarter", "q#", "q##", "#q", "##q".
    ), 
    by = c("gvkey", "call_date", "title")
  ) |>
  # Extract the quarter from the title or the beginning of the text.
  mutate(
    call_date = lubridate::mdy(call_date),
    quarter = str_extract(title, str_c(
      "((\\w+)(?=\\squarter))", "|",                                 # Extract word before "quarter" or
      "((\\w+)(?=-quarter))", "|",                                   # word before "-quarter" or
      "((?<=quarter-\\s)(\\w+)|(\\d))", "|",                         # word or "#" after "quarter-" or
      "((q\\d)|(q\\d\\d)|(\\dq)|(\\d\\dq))"                          # "q#", "q##", "#q", "##q".
    )),
    quarter = case_when(
      str_detect(quarter, "first|1st|q1|q01|1q|01q") ~ "1",
      str_detect(quarter, "second|2nd|q2|q02|2q|02q") ~ "2",
      str_detect(quarter, "third|3rd|q3|q03|3q|03q") ~ "3",
      str_detect(quarter, "fourth|4th|q4|q04|4q|04q") ~ "4"
    ),
    quarter = ifelse(!is.na(quarter), quarter, str_extract(first_line, str_c(
      "((\\w+)(?=\\squarter))", "|",                                 # Extract word before "quarter" or
      "((\\w+)(?=-quarter))", "|",                                   # word before "-quarter" or
      "((?<=quarter-\\s)(\\w+)|(\\d))", "|",                         # word or "#" after "quarter-" or
      "((q\\d)|(q\\d\\d)|(\\dq)|(\\d\\dq))"                          # "q#", "q##", "#q", "##q".
    ))),
    quarter = case_when(
      str_detect(quarter, "1|first|1st|q1|q01|1q|01q") ~ "1",
      str_detect(quarter, "2|second|2nd|q2|q02|2q|02q") ~ "2",
      str_detect(quarter, "3|third|3rd|q3|q03|3q|03q") ~ "3",
      str_detect(quarter, "4|fourth|4th|q4|q04|4q|04q") ~ "4"
    ),
    # Extract the year from the title or the beginning of the text.
    year = str_extract(title, str_c(
      "(20\\d\\d)", "|",                                             # Extract year "20##" or
      "((?<=q\\d\\s)(\\d\\d))", "|",                                 # "##" after "q#" or
      "((?<=q\\d\\d\\s)(\\d\\d))", "|",                              # "##" after "q##" or
      "((?<=\\dq\\s)|(?<=\\dq)(\\d\\d))", "|",                       # "##" after "#q" or
      "((?<=\\d\\dq\\s)|(?<=\\d\\dq)(\\d\\d))", "|",                 # "##" after "##q" or
      "((?<=')(\\d\\d))", "|",                                       # "##" after "'" or
      "((?<=fy\\s)(\\d\\d)|(?<=fy)(\\d\\d))"                         # "##" after "fy " or "fy".
    )),
    year = ifelse(!is.na(year), year, str_extract(first_line, str_c(
      "(20\\d\\d)", "|",                                             # Extract year "20##" or
      "((?<=q\\d\\s)(\\d\\d))", "|",                                 # "##" after "q#" or
      "((?<=q\\d\\d\\s)(\\d\\d))", "|",                              # "##" after "q##" or
      "((?<=\\dq\\s)|(?<=\\dq)(\\d\\d))", "|",                       # "##" after "#q" or
      "((?<=\\d\\dq\\s)|(?<=\\d\\dq)(\\d\\d))", "|",                 # "##" after "##q" or
      "((?<=')(\\d\\d))", "|",                                       # "##" after "'" or
      "((?<=fy\\s)(\\d\\d)|(?<=fy)(\\d\\d))"                         # "##" after "fy " or "fy".
    ))),
    # Clean up quarter and year.
    quarter = as.numeric(quarter),
    year = str_pad(year, 3, side = c("left"), pad = "0"),
    year = str_pad(year, 4, side = c("left"), pad = "2"),
    year = as.numeric(year),
    year = ifelse(year > 2021, NA, year),
    # If there is no year, quarters 1 and 2 have a FY = calendar year - 1, otherwise use calendar year.
    year = ifelse(is.na(year) & quarter %in% c(1, 2), lubridate::year(call_date) - 1, year),
    year = ifelse(is.na(year) & quarter %in% c(3, 4), lubridate::year(call_date), year)
  ) |>
  # Remove extraneous copywrite statements.
  mutate(
    text = str_remove_all(text, copywrite[1]),
    text = str_remove_all(text, copywrite[2]),
    text = str_remove_all(text, copywrite[3]),
    text = str_remove_all(text, copywrite[4]),
    text = str_remove_all(text, copywrite[5])
  ) |> 
  drop_na(quarter) |>
  drop_na(year) |> 
  # Finally, ensure UTF-8 encoding for the titles and text.
  mutate(
    title = iconv(title, to = "UTF-8"),
    text = iconv(text, to = "UTF-8")
  )
  
call_data

# Firm Performance --------------------------------------------------------
# Create a plain text file (.txt) with one GVKEY code per line
# for pulling quarterly revenue and industry data from Computstat.
call_data |> 
  count(gvkey) |> 
  select(gvkey) |> 
  write_tsv(here::here("Private", "Compustat GVKEYs.txt"), col_names = FALSE)

# Import the GICS standards and rename variables to match Compustat data.
data(standards)
gics <- standards |> 
  rename(
    gsector = `sector id`,
    sector = `sector name`,
    ggroup = `industry group id`,
    group = `industry group name`,
    gind = `industry id`,
    industry = `industry name`,
    gsubind = `subindustry id`,
    sub_industry = `subindustry name`
  )

# Import Compustat fundamentals quarterly with the following filters:
# - Consolidation Level: C
# - Industry Format: INDL
# - Data Format: STD
# - Population Source: D
# - Quarter Type: Fiscal View
# - Currency: USD
# - Company Status: Active and Inactive
firm_data <- read_csv(here::here("Data", "Compustat Fundamentals Quarterly.csv")) |> 
  mutate(
    gvkey = str_pad(gvkey, 6, side = c("left"), pad = "0"),
    name = conm,
    year = fyearq,
    quarter = fqtr,
    revenue = revtq
  ) |> 
  # Use the GICS standards to get sector, group, industry, and sub-industry names.
  left_join(select(gics, gsector, sector) |> distinct(), by = "gsector") |> 
  left_join(select(gics, ggroup, group) |> distinct(), by = "ggroup") |> 
  left_join(select(gics, gind, industry) |> distinct(), by = "gind") |> 
  left_join(select(gics, gsubind, sub_industry) |> distinct(), by = "gsubind") |> 
  select(gvkey, tic, name, year, quarter, revenue, sector, group, industry, sub_industry)

firm_data

# Expected Firm Performance -----------------------------------------------
# Import analyst forecasts from IBES (see Python code for query and link
# to Compustat data).
ibes_data <- read_csv(here::here("Data", "IBES.csv")) |> 
  mutate(
    gvkey = str_pad(gvkey, 6, side = c("left"), pad = "0"),
    year = fyearq,
    quarter = fqtr,
    earnings = act,
    forecast = medest
  ) |> 
  select(gvkey, year, quarter, earnings, forecast, contains("sue"))

ibes_data

# Join Data ---------------------------------------------------------------
# Join the earnings calls, firm performance, and expected performance data.
call_data <- call_data |>
  inner_join(firm_data, by = c("gvkey", "year", "quarter")) |> 
  inner_join(ibes_data, by = c("gvkey", "year", "quarter")) |> 
  # Drop any observations that don't have outcome data.
  drop_na(revenue, earnings, forecast) |> 
  # Add an id and differences.
  mutate(
    id = row_number(),
    difference = earnings - forecast
  ) |>
  # Make year_quarter variable to arrange data chronologically by firm.
  unite(year_quarter, year, quarter, remove = FALSE) |> 
  # Lead outcome data by firm.
  group_by(gvkey) |> 
  arrange(year_quarter) |> 
  mutate(
    revenue_lead = lead(revenue, order_by = year_quarter),
    earnings_lead = lead(earnings, order_by = year_quarter),
    difference_lead = lead(difference, order_by = year_quarter)
  ) |> 
  ungroup() |>
  # Select variables in order.
  select(
    id, gvkey, tic, name, sector, group, industry, sub_industry,
    call_date, year, quarter, revenue, earnings, forecast, difference,
    contains("lead"), title, text, contains("sue")
  )

call_data

# Write call data.
write_rds(call_data, here::here("Data", "call_data.rds"))

