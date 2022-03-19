library(tidyverse)
load("data/kozfog_raw_df.RData")

extract_mins <- function(x) {
  out <- map(x, function(elem) {
    if (str_detect(elem, "-")) {
      
      c(rep("-", str_count(elem, "-")), str_remove_all(elem, "-"))
    } else {
      elem
    }
  })
  reduce(out, c)
}

kozfog_df <- kozfog_raw_df %>% 
  mutate(
    data = map(data, function(d) {
      enframe(d, value = "raw", name = NULL) %>% 
        slice(2:18) %>% 
        transmute(
          category = gsub("\r\n.*", "", raw),
          values = gsub(".*\r\n", "", raw),
          values = str_split(values, "  |%"),
          values = map(values, extract_mins),
          values = map(values, function(v) keep(v, ~ str_remove_all(., " ") != "")),
        )
    })
  ) %>% 
  unnest()

kozfog_df <- kozfog_df %>% 
  mutate(
    values = map(values, set_names, 2013:2021),
    values = map(values, enframe, name = "time")
    ) %>% 
  unnest() %>% 
  pivot_wider(names_from = category) %>% 
  select(-link) %>% 
  janitor::clean_names() %>% 
  mutate_at(-(1:2), str_trim) %>% 
  mutate_at(-(1:2), str_replace, ",", ".") %>% 
  mutate_at(-(1:2), str_remove, " ") %>% 
  mutate_at(-(1:2), as.numeric) %>% 
  mutate(
    time = as.numeric(time),
    settlement = str_replace_all(settlement, "\xe1", "á")
  )

save(kozfog_df, file = "data/kozfog_df.RData")