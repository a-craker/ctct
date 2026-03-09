library(rvest)
library(janitor)
library(tidyverse)

scrape <- function(page_num) {
  url <- paste0("https://results.finishtime.co.za/results.aspx?CId=35&RId=5668&EId=1&dt=0&PageNo=", page_num)
  
  raw_table <- read_html(url) %>%
    html_node("#ctl00_Content_Main_tblResults") %>%
    html_table(header = FALSE) 
  
  # Column names
  col_names <- raw_table[1, ] %>% as.character() %>% make.unique()
  
  clean_table <- raw_table %>%
    slice(-1) %>%
    set_names(col_names) %>%
    # unnecessary cols
    select(-any_of(c("Fav", "Share", "", "Name.1"))) %>% 
    mutate(page_source = page_num)
  
  return(clean_table)
}


cycle_tour <- 1:431 %>%
  map_df(~{
    message(paste("scraping page", .x))
    Sys.sleep(1)
    scrape(.x)
  }) %>% 
  clean_names()


write_csv(cycle_tour, "data/ctct_2026.csv")
