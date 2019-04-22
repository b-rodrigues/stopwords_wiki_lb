library(tidyverse)
library(xml2)
library(furrr)
library(tidytext)

# used wikiextractor (https://github.com/attardi/wikiextractor) to get the data in a
# simpler format


doc_paths <- list.files(path = "text/AA/", pattern = "*", all.files = TRUE, recursive = TRUE, full.names = TRUE)

parse_docs <- function(doc_path){

    doc <- read_lines(doc_path)

    indices <- grepl("<doc id", doc) %>% which(isTRUE(.))

    indices2 <- c(indices[-1] - 1, length(doc))

    article_lines <- map2(indices, indices2,  ~seq(.x,.y))

    doc_list <- map(article_lines, ~`[`(doc, .))

    is_empty_character <- function(char){
        if(char == "") TRUE else FALSE
    }

    doc_list %>%
        map(., ~discard(., is_empty_character)) %>%
        #map(., ~unlist(str_split(., boundary("sentence")))) %>%
        map(tibble) %>%
        map(., ~rename(., sentence = `<chr>`)) %>%
        map(., ~mutate(., title = pull(.[2, 1]))) %>%
        map(., ~mutate(., title = tolower(title))) %>%
        map(., ~slice(., -(1:2))) %>%
        map(., ~slice(., -nrow(.))) %>%
        map(., ~mutate(., line = seq_len(nrow(.)))) %>%
        map(., ~filter(., max(line) > 2)) %>%
        bind_rows() %>%
        mutate(sentence = tolower(sentence)) %>%
        mutate(sentence = str_replace_all(sentence, "d'", "d "))
}

plan(multiprocess, workers = 12)

tidy_wiki <- future_map_dfr(doc_paths, parse_docs)

token_wiki <- tidy_wiki %>%
    unnest_tokens(word, sentence)

token_wiki_tf_idf <- token_wiki %>%
    filter(!grepl("^portal:.*", title)) %>%
    group_by(title) %>%
    count(word) %>%
    bind_tf_idf(word, title, n)

benchmark_stopwords <- token_wiki_tf_idf %>%
    filter(!grepl("\\d{1,}", word)) %>%
    filter(tf_idf < 0.0003) %>%
    pull(word) %>%
    unique()

# ipa tables : https://www.phon.ucl.ac.uk/home/wells/ipa-unicode.htm#numbers

words_positions <- functions(sentence){

}


doc <- read_lines(doc_paths[1])

indices <- grepl("<doc id", doc) %>% which(isTRUE(.))

indices2 <- c(indices[-1] - 1, length(doc))

article_lines <- map2(indices, indices2,  ~seq(.x,.y))

doc_list <- map(article_lines, ~`[`(doc, .))

is_empty_character <- function(char){
    if(char == "") TRUE else FALSE
}

doc_list %>%
    map(., ~discard(., is_empty_character)) %>%
    map(., ~unlist(str_split(., boundary("sentence")))) %>%
    map(tibble) %>%
    map(., ~rename(., sentence = `<chr>`)) %>%
    map(., ~mutate(., title = pull(.[2, 1]))) %>%
    map(., ~mutate(., title = tolower(title))) %>%
    map(., ~slice(., -(1:2))) %>%
    map(., ~slice(., -nrow(.))) %>%
    map(., ~mutate(., line = seq_len(nrow(.)))) %>%
    map(., ~filter(., max(line) > 2)) %>%
    bind_rows() %>%
    mutate(sentence = tolower(sentence)) %>%
    mutate(sentence = str_replace_all(sentence, "d'", "d "))
