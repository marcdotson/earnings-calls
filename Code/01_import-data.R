# Earnings Calls ----------------------------------------------------------
# Load libraries.
library(tidyverse)

# Import all .txt files in Data.
call_data <- readtext::readtext(here::here("Data", "*.txt")) %>% 
  tibble() %>% 
  separate(doc_id, into = c("gvkey", "date", "title"), sep = "_") %>% 
  mutate(
    text = str_replace_all(text, "\r?\n|\r", " "),
    date = lubridate::mdy(date),
    year = lubridate::year(date),
    quarter = lubridate::quarter(date)
  )

# There are a number of duplicate earnings calls as well as
# transcripts for events between quarterly earnings calls. It may
# not be worth trying to filter down to a single earnings call 
# per quarter.

# Investigate duplicate earnings calls.
call_data %>% 
  count(gvkey, date) %>% 
  filter(n != 1) %>% 
  left_join(call_data, by = c("gvkey", "date"))

# call_data %>% 
#   mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>%
#   filter(!grepl("Abstract|Event Brief", title)) %>% 
#   count(gvkey, date) %>% 
#   filter(n != 1) %>% 
#   left_join(call_data, by = c("gvkey", "date"))

# Remove repeat earnings calls and non-UTF-8 characters.
call_data <- call_data %>% 
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>%
  filter(!grepl("Abstract|Event Brief", title))

call_data

# Firm Performance --------------------------------------------------------
# Assuming earnings calls don't start before 2000.

# Create a plain text file (.txt) with one GVKEY code per line
# so we can get the firm performance for each firm by quarter
# from 2000 to the present as a CSV all at once.

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
# The challenge in joining the call_data and firm_data is that
# the quarter in which the earnings call (or other transcript) is
# made doesn't match the quarterly revenue exactly.

# - Are they required to have the earnings call within a certain time
# after the quarter ends?
# - Check to see if they always have the earnings call in the quarter
# following?

# Join the earnings calls and firm performance data.
joint_data <- call_data %>% 
  inner_join(firm_data, by = c("gvkey", "year", "quarter")) %>% 
  select(gvkey, date, year, quarter, revenue, title, text)

# Write data.
write_rds(call_data, here::here("Data", "call_data.rds"))
write_rds(joint_data, here::here("Data", "joint_data.rds"))

