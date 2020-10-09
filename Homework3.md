Homework 3
================
Nidhi Patel
10/6/2020

#### Problem 0

Follow the style rubric\!\!\!\!

#### Problem 1

###### Describe data

``` r
data("instacart")
```

Instacart dataset logs 1384617 observations over 15 Instacart online
grocery order variables. It not only keeps track of the user, order
products, reorders, day and times of orders, but also organizes each
product into departments and aisles they would be in store. For example,
the product, Grated Pecorino Romano Cheese, is found in the specialty
cheese aisle, which falls into the dairy eggs department. The least
ordered department is bulk, with 1,359 orders, while the most ordered
department is produce with 409,087 orders.

###### How many aisles? What is the most ordered aisle?

``` r
instacart %>% 
  count(aisle) %>% 
  arrange(desc(n))
```

    ## # A tibble: 134 x 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

There are 134 aisles. The fresh vegetables aisle is most ordered from
with 150609 orders.

###### Plot aisle orders

``` r
instacart %>% 
  count(aisle) %>% 
  filter(n > 10000) %>% 
  mutate( #this reorders the x axis.
    aisle = factor(aisle), #make it a factor
    aisle = fct_reorder(aisle, n) 
  ) %>% 
  ggplot(aes(x = aisle, y = n)) +
  labs(
    y = "number of items",
    title = "number of items ordered in each aisle"
) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

<img src="Homework3_files/figure-gfm/P1plot-1.png" width="90%" />

###### Top orders from baking ingredients, dog food care and packaged vegetable fruits aisles

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  group_by(aisle, product_name) %>% #so, the order of aisle, product_name matters?
  summarize(n = n()) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(aisle, rank) %>% 
  knitr::kable()
```

    ## `summarise()` regrouping output by 'aisle' (override with `.groups` argument)

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

###### When are apples and ice cream typically ordered?

``` r
dow = tibble(
    order_dow = 0:6,
    day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thurday", "Friday", "Saturday")
  )

 instacart = left_join(
    instacart, 
    dow,
    by = "order_dow"
  )
 
instacart %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
  group_by(product_name, day, order_dow) %>% 
  summarize(mean_hod = mean(order_hour_of_day)) %>% 
  arrange(order_dow) %>% 
  select(product_name, day, mean_hod) %>% 
  pivot_wider(
    names_from = "day",
    values_from = "mean_hod"
  ) %>% 
  knitr::kable()
```

    ## `summarise()` regrouping output by 'product_name', 'day' (override with `.groups` argument)

| product\_name    |   Sunday |   Monday |  Tuesday | Wednesday |  Thurday |   Friday | Saturday |
| :--------------- | -------: | -------: | -------: | --------: | -------: | -------: | -------: |
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 |  15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apples | 13.44118 | 11.36000 | 11.70213 |  14.25000 | 11.55172 | 12.78431 | 11.93750 |

#### Problem 2

###### Tidy and data wrangle

``` r
accel =
  read_csv("./accel_data.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "minute",
    names_prefix = "activity_",
    values_to = "activity"
  ) %>% 
  mutate(
    day = factor(day),
    minute = factor(minute),
    week = factor(week),
    day_id = factor(day_id),
    weekday = ifelse(day == "Saturday" | day == "Sunday", rep(FALSE), TRUE)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

The original version of this dataset contained 1436 columns and 25 rows.
Activity was spread across 1440 columns and subsequently tidied into
columns: minutes and activities. The resulting data has 50400 rows with
`rncol(accel)` activity observations. The variables ‘week’, ‘day\_id’,
‘minute’ and ‘day’ were converted to factors. These were converted to
factors to ease further wrangling and visualization. Activity was kept
as an integer. Finally a logical variable of ‘weekday’ was added to
differentiate weekdays from weekends.

###### Total activity variable for each day

``` r
activity = 
  accel %>% 
  group_by(day, week) %>% 
  summarize(tot_act = sum(activity)) %>% 
  pivot_wider(
    names_from = "day",
    values_from = "tot_act"
  ) %>% 
  select(week, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
```

    ## `summarise()` regrouping output by 'day' (override with `.groups` argument)
