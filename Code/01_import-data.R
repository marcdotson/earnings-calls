# Load libraries.
library(tidyverse)
library(tidytext)

text <- read_lines(here::here("Data", "001111_1-28-2004._Q3 2004 Activision Earnings Conference Call.txt"))

list_of_files <- list.files(path = here::here("Data"),
                            recursive = TRUE,
                            pattern = "\\.txt$",
                            full.names = TRUE)

list_of_files

