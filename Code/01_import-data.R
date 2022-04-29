# Earnings Calls ----------------------------------------------------------
# Load libraries.
library(tidyverse)

# Confirm that we can filter on no "quarter" in title. So far, only 
# transcripts that aren't quarterly earnings calls don't have some
# reference to quarter in their titles.
readtext::readtext(here::here("Data", "*.txt")) %>%
  tibble() %>%
  separate(doc_id, into = c("gvkey", "call_date", "title"), sep = "_") %>%
  mutate(quarter = str_extract(title, "Q\\d|(\\w+)(?=\\sQuarter)")) %>% 
  filter(is.na(quarter)) %>%
  select(title, quarter) %>%
  as.data.frame()

# Import all .txt files in Data.
call_data <- readtext::readtext(here::here("Data", "*.txt")) %>% 
  tibble() %>% 
  separate(doc_id, into = c("gvkey", "call_date", "title"), sep = "_") %>% 
  mutate(
    # Extract year and quarter from the title and remove carriage returns.
    call_date = lubridate::mdy(call_date),
    year = str_extract(title, "20\\d\\d|(?<=Q\\d\\s)(\\d\\d)|(?<=FY\\s)(\\d\\d)"),
    quarter = str_extract(title, "Q\\d|(\\w+)(?=\\sQuarter)"),
    text = str_replace_all(text, "\r?\n|\r", " ")
  ) %>% 
  # Filter on no "quarter" along with "Abstract|Event Brief" in title.
  drop_na(quarter) %>%
  filter(!grepl("Abstract|Event Brief", title)) %>% 
  # Remove non-UTF-8 characters.
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>%
  mutate(
    # Use the call_date for year if it isn't present in the title.
    year = ifelse(is.na(year), lubridate::year(call_date), year),
    # Clean up year and quarter.
    year = str_pad(year, 3, side = c("left"), pad = "0"),
    year = str_pad(year, 4, side = c("left"), pad = "2"),
    year = as.numeric(year),
    quarter = str_replace_all(quarter, "Q", ""),
    quarter = str_replace_all(quarter, "(F|f)irst", "1"),
    quarter = str_replace_all(quarter, "(S|s)econ", "2"),
    quarter = str_replace_all(quarter, "(T|t)hird", "3"),
    quarter = str_replace_all(quarter, "(F|f)ourth", "4"),
    quarter = as.numeric(quarter)
  )
  # filter(!is.na(quarter)) %>%
  # select(title, call_date, year, quarter) %>%
  # as.data.frame()
  # # write_csv(here::here("Private", "test.csv"))

# Confirm that we can filter on "Abstract|Event Brief" in title to 
# remove most duplicate earnings calls.
call_data %>% 
  count(gvkey, call_date) %>%
  filter(n != 1) %>%
  left_join(call_data, by = c("gvkey", "call_date")) %>% 
  select(gvkey, call_date, title) %>% 
  as.data.frame()

call_data

# Firm Performance --------------------------------------------------------
# Does the assumption of 2000 being the earliest earnings call hold?
call_data %>% arrange(call_date)

# Create a plain text file (.txt) with one GVKEY code per line
# for pulling quarterly revenue data from Computstat.
call_data %>% 
  count(gvkey) %>% 
  select(gvkey) %>% 
  write_tsv(here::here("Private", "Compustat GVKEYs.txt"), col_names = FALSE)

# Import Compustat fundamentals quarterly.
firm_data <- read_csv(here::here("Data", "Compustat Fundamentals Quarterly.csv")) %>% 
  mutate(
    gvkey = str_pad(gvkey, 6, side = c("left"), pad = "0"),
    year = fyearq,
    quarter = fqtr,
    revenue = revtq
  ) %>% 
  select(gvkey, year, quarter, revenue)
  
firm_data

# Join Data ---------------------------------------------------------------
# Join the earnings calls and firm performance data.
data <- call_data %>% 
  inner_join(firm_data, by = c("gvkey", "year", "quarter")) %>% 
  select(gvkey, call_date, year, quarter, revenue, title, text)

data

# Write data.
write_rds(data, here::here("Data", "data.rds"))

