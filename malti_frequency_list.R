library(tidyr)
library(readr)
library(rvest)
library(xml2)
library(dplyr)
library(purrr)
library(stringr)

getwd()

# the frequency list is availabel at https://www.clarin.eu/resource-families/maltese-fiction-wordlist under a MS-NC-No ReD licence
file <- read.csv("MalteseFictionFrequencyList.txt")

raw_data <- file %>%
  mutate(
    frequency = if_else(
      is.na(frequency),
      as.numeric(str_extract(word, "\\d+$")),
      frequency
    ),
    word = str_remove(word, "\\s+\\d+$")
  ) %>% 
  arrange(desc(frequency)) %>%
  mutate(word = str_replace_all(word, "’", "'"))

data_filtered <- raw_data %>%
  filter(frequency > 122)


words <- data_filtered$word

# This dictionary is available at https://github.com/ragelmalti/Maltese-to-English-Dictionary/blob/master/malti-english.dsl.dz
lines <- readLines("malti-english.dsl")

words <- c()
defs <- c()

current_word <- NULL
current_def <- c()

for (line in lines) {
  
  line <- trimws(line, "right")
  
  if (line == "") next
  
  # NEW ENTRY (not indented)
  if (!grepl("^\\s", line)) {
    
    if (!is.null(current_word)) {
      words <- c(words, current_word)
      defs <- c(defs, paste(current_def, collapse = " "))
    }
    
    current_word <- line
    current_def <- c()
    
  } else {
    current_def <- c(current_def, trimws(line))
  }
}

# last entry
if (!is.null(current_word)) {
  words <- c(words, current_word)
  defs <- c(defs, paste(current_def, collapse = " "))
}

dict_raw <- data.frame(
  word = words,
  definition = defs,
  stringsAsFactors = FALSE
)

dict <- dict_raw[-c(1:3), ] 

dict$definition <- gsub("\\[.*?\\]", "", dict$definition)
dict$definition <- gsub("\\s+", " ", dict$definition)
dict$definition <- trimws(dict$definition)

head(dict)

first_dict <- data_filtered %>%
  left_join(dict, by = "word", relationship = "many-to-many") %>%
  mutate(
    link = paste0("https://en.wiktionary.org/wiki/", word, "#Maltese")
  )

xml <- read_xml("BasicDictionary.xml")

entries <- xml_find_all(xml, ".//entry")

dict_xml <- map_dfr(entries, function(e) {
  english <- xml_text(xml_find_first(e, ".//form/orth"))
  word <- xml_text(xml_find_first(e, ".//cit/quote"))
  
  tibble(
    word = word,
    definition = tolower(english)
  )
})

dict_collapsed <- dict_xml %>%
  mutate(order_id = row_number()) %>%   # preserve original order
  group_by(word) %>%
  summarise(
    definition = paste0(
      "(", seq_along(definition), ") ", definition,
      collapse = " "
    ),
    .groups = "drop"
  )

df_final <- first_dict %>%
  left_join(dict_collapsed, by = "word") %>%
  mutate(
    definition = coalesce(definition.x, definition.y)
  ) %>%
  select(-definition.x, -definition.y)

write.csv(df_final, "malt_freq.csv", row.names = FALSE)
