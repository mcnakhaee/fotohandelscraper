
library(rvest)
library(tidyr)
library(dplyr)
library(readr)
list_page <- 'https://fotohandeldelfshaven.nl/winkel/?orderby=date'
get_df <- function(list_page){
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
  
  df_item <- data.frame(titles,links,text,prices)
  df_item
}



df_item_first <- get_df(list_page)




for (p in 2:243) {
  #Sys.sleep(1)
  page <- paste('https://fotohandeldelfshaven.nl/winkel/page/',p,'/?orderby=date',sep = '')
  print(page)
  tem_df <-  get_df(page)
  df_item_first <- bind_rows(df_item_first,tem_df)
} 



#main > div.col-xs-12.pull-right.hongo-content-right-part.hongo-shop-content-part.col-lg-9.col-md-12.col-sm-12 > ul > li.product.type-product.post-172883.status-publish.first.outofstock.product_cat-accessories-hasselblad.product_cat-filters-accessories-hasselblad.product_cat-hasselblad.has-post-thumbnail.purchasable.product-type-simple > div.product-content-wrap > div.hongo-rich-snippet.display-none


today_date <- Sys.Date()
df_item_first |> write_csv(paste('fotohandeldelfshaven',today_date,sep = '_'))