# Business Analysis With R----

# data wrangling -----

library(tidyverse)
library(readxl)


bike_tbl <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderline_tbl <- read_excel("00_data/bike_sales/data_raw/bikeshops.xlsx")
bike_orderline_tbl <- read_excel(path = "00_data/bike_sales/data_wrangled_students/bikes_orderline.xlsx")


bike_tbl


orderline_tbl


bike_orderline_tbl

# 1.0 selecting columns with select() -----

bike_orderline_tbl %>%
    select(order.id, quantity)

bike_orderline_tbl %>%
    select(1:3)

bike_
    select(starts_with("order"))


# reduce columns

bike_orderline_tbl %>%
    select(order.id, customer.id)

# rearrange columns

bike_orderline_tbl %>%
    select(quantity, order.id, quantity, everything())

bike_orderline_tbl %>%
    select(ends_with(".id"))


bike_orderline_tbl %>%
    select(contains("quantity"))

bike_orderline_tbl

# pull()

bike_orderline_tbl %>%
    pull(TOTAL_PRICE) %>%
    mean()

bike_orderline_tbl %>%
    pull(MODEL)


# select_if

bike_orderline_tbl %>%
    select_if(is.character)

bike_orderline_tbl %>%
    select_if(is.numeric)

bike_orderline_tbl %>%
    select_if(~ !is.numeric(.))



# 2.0 arranging with arrange() and dec()-----

bike_tbl %>%
    select(model,price) %>%
    arrange(desc(price))%>%
    view()


# 3.0 filtering rows with filter()-----

# 3.1 filter(): formula filtering

bike_tbl 
bike_tbl %>%
    select(model,price) %>%
    filter(price > mean(price))

bike_tbl %>%
    select(model, price) %>%
    filter((price > 5000) | (price < 1000)) %>%
    arrange(desc(price)) %>%
    view()

bike_tbl %>%
    select(model, price) %>%
    filter(price > 6000, model %>% str_detect("Supersix"))

bike_orderline_tbl  %>%
    filter(CATEGORY_2 %in% c("Over Mountain", "Trail"))

bike_orderline_tbl %>%
    filter(CATEGORY_2 == "Over Mountain")

bike_orderline_tbl %>%
    filter(CATEGORY_2 != "Over Mountain")

bike_orderline_tbl %>%
    filter(!(CATEGORY_2 %in% c("Over Mountain", "Trail")))



# 3.2 slice(): filtering with row numbers----

bike_tbl %>%
    arrange(desc(price)) %>%
    slice(1:5)

bike_orderline_tbl %>%
    arrange(price) %>%
    slice(1:5)

bike_tbl %>%
    arrange(desc(price)) %>%
    #slice(97:92) %>%
    slice((nrow(.)-4):nrow(.))

# 3.3 distinct():unique values

bike_orderline_tbl %>%
    distinct(CATEGORY_1)

bike_orderline_tbl %>%
    distinct(CATEGORY_1, CATEGORY_2)

bike_orderline_tbl %>%
    distinct(BIKESHOP_NAME, CITY, STATE)



# 4.0 adding columns with mutate() ------

bike_orderline_prices <- bike_orderline_tbl %>%
    select(ORDER_DATE, MODEL, QUANTITY, PRICE) %>%
    mutate(total_price = QUANTITY * PRICE) 

# transformation columns 

bike_orderline_prices %>%
    mutate(total_price_log = log(total_price)) %>%
    mutate(total_price_squrt = total_price^0.5)

# adding flag

bike_orderline_prices %>%
    mutate(is_Supersix = MODEL %>% str_to_lower() %>% str_detect("supersix")) %>%
    filter(is_Supersix)

# binning with ntile()

bike_orderline_prices %>%
    mutate(total_price_binned = ntile(total_price, 3))


# case_when() - more flexible binning


# numeric to categorical

bike_orderline_prices %>%
    mutate(total_price_binned = ntile(total_price, 3)) %>%
    mutate(total_price_binned2 = case_when(
        total_price > quantile(total_price , 0.66) ~ "High",
        total_price > quantile(total_price , 0.33) ~ "Medium",
        TRUE ~ "Low"
    ))

# text to categorical


bike_orderline_prices %>%
    mutate(bike_type = case_when(
        MODEL %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix",
        MODEL %>% str_to_lower() %>% str_detect("jekeyll") ~ "Jekyll",
        TRUE ~ "Not Supersix or Jekyll"
    ))

# 5.0 grouping and summarise with group_by() and summarize().-----

bike_orderline_tbl %>%
    summarise(
        revenue = sum(TOTAL_PRICE)
    )

bike_orderline_tbl %>%
    group_by(CATEGORY_1) %>%
    summarise(revenue = sum(TOTAL_PRICE))

bike_orderline_tbl %>%
    group_by(CATEGORY_1, CATEGORY_2) %>%
    summarise(revenue = sum(TOTAL_PRICE)) %>%
    ungroup() %>%
    arrange(desc(revenue))

bike_orderline_tbl %>%
    group_by(CATEGORY_1, CATEGORY_2,`FRAME MATERIAL`) %>%
    summarise(revenue = sum(TOTAL_PRICE)) %>%
    ungroup() %>%
    arrange(desc(revenue))

# summary fucntions

bike_orderline_tbl %>%
    group_by(CATEGORY_1, CATEGORY_2) %>%
    summarise(
        count = n(),
        avg = mean(TOTAL_PRICE),
        med = median(TOTAL_PRICE),
        sd = sd(TOTAL_PRICE),
        min = min(TOTAL_PRICE),
        max = max(TOTAL_PRICE)
    ) %>%
    ungroup() %>%
    arrange(desc(count))

# summarise_all() = detect missing values

bike_orderline_missing <- bike_orderline_tbl %>%
    mutate(TOTAL_PRICE = c(rep(NA, 4), TOTAL_PRICE[5:nrow(.)]))

bike_orderline_missing %>%
    summarise_all(~sum(is.na(.)))

bike_orderline_missing %>%
    summarise_all(~ sum(is.na(.))/ length(.))

bike_orderline_missing %>%
    filter(!is.na(TOTAL_PRICE))

# 6.0 renaming columns

# 6.1 rename : one column at a time ----

bikeshop_revenue_tbl <- bike_orderline_tbl %>%
    select(BIKESHOP_NAME, CATEGORY_1, TOTAL_PRICE) %>%
    group_by(BIKESHOP_NAME, CATEGORY_1) %>%
    summarise(sales = sum(TOTAL_PRICE)) %>%
    ungroup() %>%
    arrange(desc(sales))

bikeshop_revenue_tbl %>%
    rename(
        'Bikeshop Name' = BIKESHOP_NAME,
        'Primary Category' = CATEGORY_1,
        Sales = sales
    )

# 6.2 set_name : all columns at once----

bikeshop_revenue_tbl %>%
    set_names(c("Bikeshop Name", "Primary Category", "Sales"))

bikeshop_revenue_tbl %>%
    set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())


# 7.0 reshaping (Pivoting) data with spread() and gether()---

# 7.1 spread(): long to wide----

bikeshop_revenue_formated_tbl <- bikeshop_revenue_tbl %>%
    
    spread(key = CATEGORY_1, value = sales) %>%
    arrange(desc(Mountain)) %>%
    rename('BIkeshop Name' = BIKESHOP_NAME) %>%
    
    mutate(
        Mountain = scales::dollar(Mountain),
        Road = scales::dollar(Road)
    )
 bikeshop_revenue_formated_tbl
 
 
# 7.2 gather(): wide to long

bikeshop_revenue_formated_tbl %>%
    gather(key = "category_1", value = "sales", Mountain, Road) %>%
    
    mutate(sales = sales %>% str_remove_all("\\$|,") %>% as.double()) %>%
    arrange(desc(sales))


# 8.0 Joining data bt key with left join

orderlines_tbl

bike_tbl

orderlines_tbl %>%
    left_join(y = bike_tbl, by = c("product.id" = "bike.id"))

# 9.0 Bind tibbles by cols and rows

# 9.1 bind_cols()-----

bike_orderline_tbl %>%
    select(-contains("order")) %>%
    
    bind_cols(
        bike_orderline_tbl %>% select(ORDER_ID)
    )

# 9.2 bind_rows()----

train_tbl <- bike_orderline_tbl %>%
    slice(1:(nrow(.)/2))

train_tbl

test_tbl <- bike_orderline_tbl %>%
    slice((nrow(.)/2 +1 ):nrow(.))

test_tbl

train_tbl %>%
    bind_rows(test_tbl)

# 10 separate and unite------

bike_orderline_tbl %>%
    select(ORDER_DATE) %>%
    mutate(ORDER_DATE = as.character(ORDER_DATE)) %>%
    
    separate(col = ORDER_DATE, into = c("year", "month","day"), sep = "-", remove = FALSE) %>%
    
    mutate(
        year = as.numeric(year),
        month = as.numeric(month),
        day = as.numeric(day),
    ) %>%
    
    #umite
    unite(ORDER_DATE_united, year, month, day, sep = "-", remove = FALSE) %>%
    mutate(ORDER_DATE_united = as.Date(ORDER_DATE_united))



