library(tidyverse)
library(rvest)

settlement_df <- str_c("http://kozfoglalkoztatas.bm.hu/statisztika/terkep/@", LETTERS, "_MENU.HTM") %>% 
  map(read_html) %>% 
  map(html_elements, "a") %>% 
  map_df(function(x) tibble(settlement = html_text(x), link = html_attr(x, "href")))

safely_read <- possibly(read_html, NA, quiet = FALSE)

sleepy_read <- function(x) {
  out <- safely_read(x)
  while (is.na(out)) {
    message("60 sec : ", x)
    Sys.sleep(60)
    out <- safely_read(x)
  }
  i <<- i + 1
  setTxtProgressBar(pb, i)
  out
}

Sys.sleep(3)

i <- 0
pb <- txtProgressBar(min = i, max = nrow(settlement_df), initial = 0, width = 50, style = 3)

kozfog_raw_df <- settlement_df %>% 
  mutate(
    link = str_c("http://kozfoglalkoztatas.bm.hu/statisztika/terkep/", link),
    data = map(link, sleepy_read),
    data = map(data, html_elements, "tr"),
    data = map(data, html_text)
  )

close(pb)

save(kozfog_raw_df, file = "data/kozfog_raw_df.RData")


