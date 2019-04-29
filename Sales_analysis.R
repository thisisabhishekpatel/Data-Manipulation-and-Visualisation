# Business Analysis with R -----

# Salaes Analysis ----

# 1.0 load library ----


library(tidyverse)
library(lubridate)

# theme_tq

library(tidyquant)


# excel files

library(readxl)
library(writexl)




# 2.0 importing files ----

bikes_tbl <- read_excel( path = "00_data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel(path = "00_data/bike_sales/data_raw/orderlines.xlsx")


# 3.0 examine data ----

bikes_tbl
glimpse(bikes_tbl)

bikeshops_tbl
glimpse(bikeshops_tbl)

orderlines_tbl
glimpse(orderlines_tbl)


# 4.0 joining data ----

# left join

bikes_tbl

orderlines_tbl

left_join(bikes_tbl,orderlines_tbl, by = c("bike.id" = "product.id"))

bikes_orderline_tbl_left <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

    
bikes_orderline_tbl_left

bikes_orderline_tbl_left %>% glimpse()

# 5.0 data wrangling ----

bikes_orderline_wrangeled_tbl <- bikes_orderline_tbl_left %>%
    
    separate(description,
             into = c("category.1", "category.2", "frame material"),
             sep = " - ",
             remove = TRUE) %>%
    
    separate(location,
             into = c("city", "state"),
             sep = ",",
             remove = FALSE) %>%
    
    mutate(total.price = quantity * price) %>%
    



    select(-"..1", -location) %>%
    select(-ends_with(".id")) %>%
    
    bind_cols(bikes_orderline_tbl_left %>% select(order.id)) %>%
    
    select(contains("date"), contains(".id"), contains("order"),
           quantity, price, total.price, everything()) %>%
    
    rename(order_date = order.date) %>%
    
    
    set_names(names(.) %>% toupper()) %>%
    set_names(names(.) %>% str_replace_all("\\.","_"))

bikes_orderline_wrangeled_tbl %>% glimpse()


# 6.0 business insights

# sales by year (manipulate)


sales_by_year <- bikes_orderline_wrangeled_tbl %>%
    
    select(ORDER_DATE, TOTAL_PRICE) %>%
    mutate(year = year(ORDER_DATE)) %>%
    
    group_by(year) %>%
    summarise(sales = sum(TOTAL_PRICE)) %>%
    ungroup() %>%
    
    mutate(sales_text = scales::dollar(sales))

# visulise


sales_by_year %>%
    
    ggplot(aes(x = year , y = sales))+
    
    # geometric
    
    geom_col(fill = "#2c3e50") + 
    geom_label(aes(label = sales_text))+
    geom_smooth(method = "lm", se = FALSE) +
    
    # formating
    
    theme_tq()+
    scale_y_continuous(labels = scales::dollar)+
    labs( 
        title = "Revenue by year" ,
        subtitle = "Upword Trend",
        x = "",
        y = "Revenue"
        
        
        
    )

# sales by year with category 2

bikes_sales_by_year_category2 <- bikes_orderline_wrangeled_tbl %>%
    
    select(ORDER_DATE, TOTAL_PRICE, CATEGORY_2) %>%
    mutate(year = year(ORDER_DATE)) %>%
    
    group_by(year, CATEGORY_2) %>%
    summarise(sales = sum(TOTAL_PRICE)) %>%
    ungroup() %>%
    
    # formting
    
    mutate(sales_text = scales::dollar(sales))

bikes_sales_by_year_category2

# visulise

bikes_sales_by_year_category2 %>%
    
    ggplot(aes(x = year , y = sales, fill = CATEGORY_2)) +
    geom_col() +
    geom_smooth(method = "lm", se = FALSE)+
    
    
    facet_wrap(~ CATEGORY_2, ncol = 3) +
    
    # formating
    
    theme_tq()+
    scale_fill_tq()+
    scale_y_continuous(labels = scales::dollar)+
    labs(
        title = "Revenue by Year and Category",
        Subtitle = "Each product category has upward trend",
        x = "",
        y = "Revenue",
        fill = "Product seceondary category"
    )

# 7.0 file write

# create folder

fs::dir_create("00_data/bike_sales/data_wrangled_students")

# excel

bikes_orderline_wrangeled_tbl %>%
    write_xlsx("00_data/bike_sales/data_wrangled_students/bikes_orderline.xlsx")

# CSV

bikes_orderline_wrangeled_tbl %>%
    write_csv("00_data/bike_sales/data_wrangled_students/bikes_orderline.csv")

# RDS

bikes_orderline_wrangeled_tbl %>%
    write_rds("00_data/bike_sales/data_wrangled_students/bikes_orderline.rds")



    



                  
                  
                  
                  
    
    
    


    
    

      
    
