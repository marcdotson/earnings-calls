# Tokenize and Remove Stop Words ------------------------------------------
# Load packages.
library(tidyverse)
library(tidytext)

# Import call data.
call_data <- read_rds(here::here("Data", "call_data.rds")) |>
  mutate(
    title_text = str_c(title, text, " ")                 # Combine title and text.
   # title_text = str_replace_all(text, "[:punct:]", " ")  # Strip punctuation to deal with contractions.
  ) |> 
  select(id, title_text)                                  # Select only id and title_text to reduce memory demand.

call_data


# Not sure where to put this, it might be better in the 01_data-wrangling.
# I was trying to cut out superfluous/confounding language within the transcripts,
# initially I was looking into cutting out names, but then I realized that most
# of the transcripts have some sort of copywrite blurb at the end. I tried
# to do some regex magic, but gave up and did it the naive way.
# I only found 5 different types of blurbs, there may be more but this covers
# the vast majority of transcripts. The other transcripts just seem to end
# with the operator thanking everyone and wishing them a good day.

# Not sure the best way to do this, as 4 of the 5 blurbs are rather long.
blurb1 <- "CCBN reserves the right to make changes to documents, content, or other information on this web site without obligation to notify any person of such changes. In the conference calls upon which Event Transcripts are based, companies may make projections or other forward-looking statements regarding a variety of items. Such forward-looking statements are based upon current expectations and involve risks and uncertainties. Actual results may differ materially from those stated in any forward-looking statement based on a number of important factors and risks, which are more specifically identified in the companies' most recent SEC filings. Although the companies may indicate and believe that the assumptions underlying the forward-looking statements are reasonable, any of the assumptions could prove inaccurate or incorrect and, therefore, there can be no assurance that the results contemplated in the forward-looking statements will be realized. THE INFORMATION CONTAINED IN EVENT TRANSCRIPTS IS A TEXTUAL REPRESENTATION OF THE APPLICABLE COMPANY'S CONFERENCE CALL AND WHILE EFFORTS ARE MADE TO PROVIDE AN ACCURATE TRANSCRIPTION, THERE MAY BE MATERIAL ERRORS, OMISSIONS, OR INACCURACIES IN THE REPORTING OF THE SUBSTANCE OF THE CONFERENCE CALLS. IN NO WAY DOES CCBN ASSUME ANY RESPONSIBILITY FOR ANY INVESTMENT OR OTHER DECISIONS MADE BASED UPON THE INFORMATION PROVIDED ON THIS WEB SITE OR IN ANY EVENT TRANSCRIPT. USERS ARE ADVISED TO REVIEW THE APPLICABLE COMPANY'S CONFERENCE CALL ITSELF AND THE APPLICABLE COMPANY'S SEC FILINGS BEFORE MAKING ANY INVESTMENT OR OTHER DECISIONS."

blurb2 <- "Thomson Financial reserves the right to make changes to documents, content, or other information on this web site without obligation to notify any person of such changes. In the conference calls upon which Event Transcripts are based, companies may make projections or other forward-looking statements regarding a variety of items. Such forward-looking statements are based upon current expectations and involve risks and uncertainties. Actual results may differ materially from those stated in any forward-looking statement based on a number of important factors and risks, which are more specifically identified in the companies' most recent SEC filings. Although the companies may indicate and believe that the assumptions underlying the forward-looking statements are reasonable, any of the assumptions could prove inaccurate or incorrect and, therefore, there can be no assurance that the results contemplated in the forward-looking statements will be realized. THE INFORMATION CONTAINED IN EVENT TRANSCRIPTS IS A TEXTUAL REPRESENTATION OF THE APPLICABLE COMPANY'S CONFERENCE CALL AND WHILE EFFORTS ARE MADE TO PROVIDE AN ACCURATE TRANSCRIPTION, THERE MAY BE MATERIAL ERRORS, OMISSIONS, OR INACCURACIES IN THE REPORTING OF THE SUBSTANCE OF THE CONFERENCE CALLS. IN NO WAY DOES THOMSON FINANCIAL OR THE APPLICABLE COMPANY OR THE APPLICABLE COMPANY ASSUME ANY RESPONSIBILITY FOR ANY INVESTMENT OR OTHER DECISIONS MADE BASED UPON THE INFORMATION PROVIDED ON THIS WEB SITE OR IN ANY EVENT TRANSCRIPT. USERS ARE ADVISED TO REVIEW THE APPLICABLE COMPANY'S CONFERENCE CALL ITSELF AND THE APPLICABLE COMPANY'S SEC FILINGS BEFORE MAKING ANY INVESTMENT OR OTHER DECISIONS."

# blurb3 has an extra space at the start of a sentence
blurb3 <- "Thomson Financial reserves the right to make changes to documents, content, or other information on this web site without obligation to notify any person of such changes.  In the conference calls upon which Event Transcripts are based, companies may make projections or other forward-looking statements regarding a variety of items. Such forward-looking statements are based upon current expectations and involve risks and uncertainties. Actual results may differ materially from those stated in any forward-looking statement based on a number of important factors and risks, which are more specifically identified in the companies' most recent SEC filings. Although the companies may indicate and believe that the assumptions underlying the forward-looking statements are reasonable, any of the assumptions could prove inaccurate or incorrect and, therefore, there can be no assurance that the results contemplated in the forward-looking statements will be realized.  THE INFORMATION CONTAINED IN EVENT TRANSCRIPTS IS A TEXTUAL REPRESENTATION OF THE APPLICABLE COMPANY'S CONFERENCE CALL AND WHILE EFFORTS ARE MADE TO PROVIDE AN ACCURATE TRANSCRIPTION, THERE MAY BE MATERIAL ERRORS, OMISSIONS, OR INACCURACIES IN THE REPORTING OF THE SUBSTANCE OF THE CONFERENCE CALLS. IN NO WAY DOES THOMSON FINANCIAL OR THE APPLICABLE COMPANY OR THE APPLICABLE COMPANY ASSUME ANY RESPONSIBILITY FOR ANY INVESTMENT OR OTHER DECISIONS MADE BASED UPON THE INFORMATION PROVIDED ON THIS WEB SITE OR IN ANY EVENT TRANSCRIPT. USERS ARE ADVISED TO REVIEW THE APPLICABLE COMPANY'S CONFERENCE CALL ITSELF AND THE APPLICABLE COMPANY'S SEC FILINGS BEFORE MAKING ANY INVESTMENT OR OTHER DECISIONS."

blurb4 <- "CCBN reserves the right to make changes to documents, content, or other information on this web site without obligation to notify any person of such changes.  In the conference calls upon which Event Transcripts are based, companies may make projections or other forward-looking statements regarding a variety of items. Such forward-looking statements are based upon current expectations and involve risks and uncertainties. Actual results may differ materially from those stated in any forward-looking statement based on a number of important factors and risks, which are more specifically identified in the companies' most recent SEC filings. Although the companies may indicate and believe that the assumptions underlying the forward-looking statements are reasonable, any of the assumptions could prove inaccurate or incorrect and, therefore, there can be no assurance that the results contemplated in the forward-looking statements will be realized.  THE INFORMATION CONTAINED IN EVENT TRANSCRIPTS IS A TEXTUAL REPRESENTATION OF THE APPLICABLE COMPANY'S CONFERENCE CALL AND WHILE EFFORTS ARE MADE TO PROVIDE AN ACCURATE TRANSCRIPTION, THERE MAY BE MATERIAL ERRORS, OMISSIONS, OR INACCURACIES IN THE REPORTING OF THE SUBSTANCE OF THE CONFERENCE CALLS. IN NO WAY DOES CCBN ASSUME ANY RESPONSIBILITY FOR ANY INVESTMENT OR OTHER DECISIONS MADE BASED UPON THE INFORMATION PROVIDED ON THIS WEB SITE OR IN ANY EVENT TRANSCRIPT. USERS ARE ADVISED TO REVIEW THE APPLICABLE COMPANY'S CONFERENCE CALL ITSELF AND THE APPLICABLE COMPANY'S SEC FILINGS BEFORE MAKING ANY INVESTMENT OR OTHER DECISIONS. Copyright 200[0-9], CCBN, Inc. All Rights Reserved."

blurb5 <- "NO PORTION OF THIS TRANSCRIPTION MAY BE COPIED, SOLD OR RETRANSMITTED WITHOUT THE EXPRESS WRITTEN AUTHORITY OF FDCH-eMedia, Inc."


# Saving the number of transcripts containing each blurb for validity.

# 14286 transcripts
blurb1_transcripts <- call_data |>
  mutate(blurb = str_detect(title_text, blurb1)) |> 
  summarise(n= sum(blurb==TRUE))

blurb1_transcripts

# 108718 transcripts
blurb2_transcripts <- call_data |>
  mutate(blurb = str_detect(title_text, blurb2)) |> 
  summarise(n= sum(blurb==TRUE))

blurb2_transcripts

# 4662 transcripts
blurb3_transcripts <- call_data |>
  mutate(blurb = str_detect(title_text, blurb3)) |> 
  summarise(n= sum(blurb==TRUE))

blurb3_transcripts

# 1842 transcripts
blurb4_transcripts <- call_data |>
  mutate(blurb = str_detect(title_text, blurb4)) |> 
  summarise(n= sum(blurb==TRUE))

blurb4_transcripts

# 571 transcripts
blurb5_transcripts <- call_data |>
  mutate(blurb = str_detect(title_text, blurb5)) |> 
  summarise(n= sum(blurb==TRUE))

blurb5_transcripts

# This should leave 452 transcripts without a copywrite blurb. From looking at a 
# not-so-random sample of about 20 transcripts, I didn't find any other obvious
# copywrite blurbs. I also didn't mess with the copywrite parts at the beginning.
# These blurbs at the end were low-hanging fruit and contain a sizable amount of
# words, so I figured it was worth rerunning everything to remove them.

# Blurb removal
call_data <- call_data |>
  mutate(title_text = str_remove_all(title_text, blurb1)) |>
  mutate(title_text = str_remove_all(title_text, blurb2)) |>
  mutate(title_text = str_remove_all(title_text, blurb3)) |>
  mutate(title_text = str_remove_all(title_text, blurb4)) |>
  mutate(title_text = str_remove_all(title_text, blurb5))




# # Mutate can take a bit to run, so we can save it as temp_call.rds.
# # write_rds(call_data, here::here("Data", "temp_call.rds"))
# call_data <- read_rds(here::here("Data", "temp_call.rds"))

# Import L&M generic stop words (not including clmd and lmn marketing terms).
generic_stopwords <- read_rds(here::here("Data", "generic_stopwords_long.rds"))

generic_stopwords

# Tokenize call data in sets to avoid memory loss and limits.
# (If we need to revisit tokenizing, consider how to parallelize.)
num_splits <- 30
for (i in seq_along(1:num_splits)) {
  # Specify the start and end rows for each of the sets.
  start <- round(nrow(call_data)/num_splits * (i - 1)) + 1
  end <- round(nrow(call_data)/num_splits * i)
  
  # Name assignment is based on index variable.
  tokens_name <- str_c("tokens_", i, ".rds")
  
  # Tokenize and assign to a separate object each iteration.
  tokens <- call_data |> 
    # Slice the data.
    slice(start:end) |> 
    # Strip punctuation to deal with contractions.
    unnest_tokens(word, title_text, strip_punct = TRUE) |> 
    # Clean up contractions.
    mutate(
      word = case_when(
        word == "ll" ~ "will",
        word == "ve" ~ "have",
        word == "t" ~ "not",
        word == "d" ~ "had",
        word == "s" ~ "is",
        word == "re" ~ "are",
        TRUE ~ word
      )
    ) |> 
    # Remove generic stop words.
    anti_join(generic_stopwords) |> 
    # Save tokens with associated id.
    write_rds(here::here("Data", tokens_name))

    # Remove objects to reduce memory demand.
    rm(tokens, tokens_name)
}

# Bind Data ---------------------------------------------------------------
# Remove data that no longer needs to be held in memory.
rm(call_data, generic_stopwords)

# Re-import complete call_data except for text.
call_data <- read_rds(here::here("Data", "call_data.rds")) |> 
  select(-text)

# Bind sliced tokenized data.
word_tokens <- NULL
for (i in 1:num_splits) {
  # Recreate saved file names.
  tokens_name <- str_c("tokens_", i, ".rds")
  
  # Import saved files.
  tokens <- read_rds(here::here("Data", tokens_name)) |>
    group_by(id) |> 
    # Nesting words by id, reducing memory demand (this can always be undone with unnest()).
    nest(words = c(word)) |> 
    # Rejoin firm data.
    left_join(call_data, by = "id")
  
  # Bind sliced data to main data frame.
  word_tokens <- word_tokens |>
    bind_rows(tokens)
  
  # Remove rows from call_data that have been joined. This reduces memory demand and 
  # highlights any documents which might have been missed.
  call_data <- call_data |> anti_join(word_tokens, by = "id")
  
  # Delete sliced tokenized data.
  unlink(here::here("Data", tokens_name))
}

# Ungroup word_tokens.
word_tokens <- word_tokens |> ungroup()

word_tokens

# Write word_tokens.
write_rds(word_tokens, here::here("Data", "word_tokens.rds"))

