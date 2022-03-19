library(tidyverse)
library(sf)

files_df <- list.files("data/ksh_raw/", full.names = TRUE) %>% 
  # merge all files from ksh_raw folder to 1 df
  enframe(value = "file_name", name = NULL) %>% 
  mutate(
    time = map_dbl(file_name, function(file_name) {
      read.csv(file_name, check.names = FALSE) %>% 
        pull() %>% 
        .[[2]] %>% 
        str_remove_all("\\D") %>% 
        as.numeric()
    }),
    data = map(file_name, function(file_name) {
      rio::import(file_name, encoding = "UTF-8") %>% 
        {set_names(., str_c("v", 1:ncol(.)))} %>% 
        select(- last_col()) %>% 
        tibble() %>% 
        mutate_all(iconv, from = "latin1", to = "utf-8") %>% # encoding solution!
        janitor::row_to_names(1) %>% 
        janitor::clean_names()
    })
  ) %>% 
  arrange(time)

ksh_df <- files_df %>%
  select(- file_name) %>% 
  group_by(time) %>% 
  group_split() %>% 
  map_df(~ {tibble(time = .$time[1], data = reduce(.$data, full_join))}) %>% 
  unnest() %>% 
    distinct() %>% 
  mutate_at(- (1:2), str_remove_all, " ") %>% 
  mutate_at(- (1:2), str_replace_all, ",", ".") %>% 
  mutate_at(- (1:2), as.numeric)
                         
ksh_df <- granatlib::Hungarian_map_admin8 %>% # match areas
  tibble() %>% 
  select(terulet = 1) %>% 
  left_join(ksh_df) %>% 
  rename(town = terulet)

save(ksh_df, file = "data/ksh_df.RData")

