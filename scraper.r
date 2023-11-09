
#2023-11-07-p3
library(rvest)
library(tidyr)
library(dplyr)
library(readr)
library(fs)  # For working with the file system
library(purrr)
library(magrittr)
library(stringr)


get_price_info <- function(...) {
  args <- list(...)
  if (grepl(args$prices, 'sold')) {
    price = 'sold'
  } else {
    price <-   read_html(args$links) |>
      html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "price", " " ))]') |>
      html_text()
  }
  
  return(as.character(price))
}


# get the latest data file stored
folder_path <- "data"
file_list <-
  fs::dir_ls(folder_path, full.names = TRUE, pattern = "\\.csv$")
file_info <- file_info(file_list)
latest_file_index <- which.max(file_info$modification_time)
latest_file_path <- file_list[latest_file_index]
latest_data <- read_csv(latest_file_path)
n_old_pages <- latest_data |>
  head(1) |>
  pull(n_page)

list_page <- 'https://fotohandeldelfshaven.nl/winkel/?orderby=date'

get_df <- function(list_page) {
  titles <-   read_html(list_page) |>
    html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "woocommerce-loop-product__title", " " ))]') |>
    html_text()
  
  links <- read_html(list_page) |>
    html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "hongo-LoopProduct-link", " " ))]') |>
    html_attr("href")
  
  text <- read_html(list_page) |>
    html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "product-content-wrap", " " ))]') |>
    html_text()
  
  
  prices <-   read_html(list_page) |>
    html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "price", " " ))]') |>
    html_text()
  
  df_item <- data.frame(titles, links, text, prices)
  df_item
}


n_pages <- read_html(list_page) %>%
  html_element(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "page-numbers", " " )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]') %>%
  html_text() %>%
  as.numeric()

df_item_first <- get_df(list_page)

n_new_pages = 1 + n_pages - n_old_pages

for (p in 2:n_new_pages) {
  #Sys.sleep(1)
  page <-
    paste('https://fotohandeldelfshaven.nl/winkel/page/',
          p,
          '/?orderby=date',
          sep = '')
  print(page)
  tem_df <-  get_df(page)
  df_item_first <- bind_rows(df_item_first, tem_df)
}


df_item_first <- df_item_first |>
  mutate(n_page = n_pages)


all_recent_data <- bind_rows(df_item_first, latest_data) |>
  distinct_all() |>
  mutate(prices = str_to_lower(str_replace_all(prices, " ", "")),
         prices = str_to_lower(str_replace_all(prices, "price:", "")),
  ) %>% 
  distinct()
# Apply the function to each row and store the results in a new column
#all_recent_data2 <- all_recent_data %>%
all_recent_data_sold_info <- all_recent_data %>%
  mutate(
    new_price = pmap_chr(., get_price_info),
    new_price = str_to_lower(str_replace_all(new_price, " ", "")),
    new_price = str_to_lower(str_replace_all(new_price, "price:", "")),
    date_sold = if_else(
      (new_price =='sold') &( prices!='sold') ,
      as.character(Sys.Date()),
      if_else(date_sold!='not_sold_sold_before',date_sold,'not_sold_sold_before')
    ))

today_date <- Sys.Date()
all_recent_data_sold_info |> write_csv(paste('data/fotohandeldelfshaven', '_', today_date, '.csv', sep = ''))

