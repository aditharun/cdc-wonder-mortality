library(tidyverse)

df <- "../raw-data/standard-pop/standard-population.txt" %>% read.table(., colClasses = "character")

df <- df %>% mutate(V1 = str_sub(V1, -7, -1)) %>% as_tibble() %>% mutate(idx = (1:n() ) - 1) %>% arrange(desc(idx)) %>% type.convert(as.is = TRUE)

df <- df %>% magrittr::set_colnames(c("Population", "Age")) %>% relocate(Age) %>% arrange(Age)

write_tsv(df, "../raw-data/age_2000_standard_population_final.tsv")