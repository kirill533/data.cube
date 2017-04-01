library(lubridate)
library(data.table)

declare_data <- function() {

  base_fact = data.table(
    "date" = as.Date(c("2014-01-01", "2014-02-01", "2014-03-01","2014-04-01")),
    "category" = c("A", "B", "C", "D"),
    "amount" = c(1, 2, 4, 8)
  )

  base_dim_category = data.table(
    "category" = c("A", "B", "C", "D", "E"),
    "label" =  c("apple", "blueberry", "cantaloupe", "date","e-fruit"),
    "size" = c(2, 1, 4, 1, 0)
  )

  base_dim_size = data.table(
    size = c(0, 1, 2, 4, 8),
    label = c("invisible", "small", "medium", "large", "very large")
  )

  src_sales = data.table(
    "id" = c(1,2,3,4,5,6,7,8,9),
    "date" = as.Date(c('2015-01-01','2015-01-02','2015-01-03','2015-01-04','2015-01-05','2015-02-01','2015-02-01','2015-03-01','2015-04-01')),
    "location" = c('here','here','here','here','there','there','there','there','unknown'),
    "item" = c('apricot','plum','goat','apricot','shirt','jacket','apricot','apricot','apricot'),
    "quantity" = c(1,2,1,2,2,1,2,2,2),
    "price" = c(3,1,1,6,20,50,6,6,6),
    "discount" = c(0,0,0,0,10,10,0,50,50)
  )

  fact_sales = data.table(
    "id" = integer(),
    "date_key" = integer(),
    "item_key" = integer(),
    "category_key" = integer(),
    "department_key" = integer(),
    "quantity" = integer(),
    "price" = integer(),
    "discount" = integer()
  )

  fact_sales_denorm = data.table(
    "id" = integer(),
    "date_key" = integer(),
    "date" = as.Date(character()),
    "item_key" = integer(),
    "item_name" = character(),
    "item_unit_price" = integer(),
    "category_key" = integer(),
    "category_name" = character(),
    "department_key" = integer(),
    "department_name" = character(),
    "quantity" = integer(),
    "price" = integer(),
    "discount" = integer()
  )

  m = matrix(c(
    c(1, "produce", 1, "grocery"),
    c(2, "dairy",   1, "grocery"),
    c(3, "bakery",  1, "grocery"),
    c(4, "meat",    1, "grocery"),
    c(5, "hygiene", 2, "body"),
    c(6, "formal",  3, "fashion"),
    c(7, "casual",  3, "fashion")
  ), 4, 7)

  dim_category = data.table(
    "category_key" = as.integer(m[1,]),
    "name" = as.character(m[2,]),
    "department_key" = as.integer(m[3,]),
    "department" = as.character(m[4,])
  )

  m = matrix(c(
    c( 1, "apricot",   1, "produce",  3),
    c( 2, "plum",      1, "produce",  2),
    c( 3, "carrot",    1, "produce",  1),
    c( 4, "celery",    1, "produce",  2),
    c( 5, "milk",      2, "dairy",    2),
    c( 6, "cheese",    2, "dairy",    5),
    c( 7, "bread",     3, "bakery",   3),
    c( 8, "rolls",     3, "bakery",   1),
    c( 9, "chicken",   4, "meat",     4),
    c(10, "beef",      4, "meat",     8),
    c(11, "goat",      4, "meat",     7),
    c(12, "soap",      5, "hygiene",  1),
    c(13, "lotion",    5, "hygiene",  5),
    c(14, "shirt",     6, "formal",  20),
    c(15, "pants",     6, "formal",  30),
    c(16, "jacket",    7, "casual",  50),
    c(17, "shorts",    7, "casual",  25)
  ), 5, 17)

  dim_item = data.table(
    "item_key" = as.integer(m[1,]),
    "name" = as.character(m[2,]),
    "category_key" = as.integer(m[3,]),
    "category" = as.character(m[4,]),
    "unit_price" = as.integer(m[5,])
  )

  m = matrix(c(
    c(1, "grocery", "Michael"),
    c(2, "body",    "Marek"),
    c(3, "fashion", "Sebastian")
  ), 3, 3)

  dim_department = data.table(
    "department_key" = as.integer(m[1,]),
    "name" = as.character(m[2,]),
    "manager" = as.character(m[3,])
  )

  # generate fact table data
  dft_values = data.table::copy(src_sales)
  dft_values = merge(dft_values, dim_item[, .(item_key, name, item_name=name, category_key, item_unit_price = unit_price)], by.x = 'item', by.y = 'name')
  dft_values = merge(dft_values, dim_category[, .(category_key, category_name=name, department_key)], by = 'category_key')
  dft_values = merge(dft_values, dim_department[, .(department_key, department_name=name)], by = 'department_key')

  ft_values = dft_values[, .(
    id,
    date_key = as.integer(format(date, '%Y%m%d')),
    item_key,
    category_key,
    department_key,
    quantity,
    price,
    discount
  )]

  dft_values = dft_values[, .(
    id,
    date_key = as.integer(format(date, '%Y%m%d')),
    date,
    item_key,
    item_name,
    item_unit_price,
    category_key,
    category_name,
    department_key,
    department_name,
    quantity,
    price,
    discount
  )]

  fact_sales = rbind(fact_sales, ft_values)
  fact_sales_denorm = rbind(fact_sales_denorm, dft_values)

  # create date dimension
  dim_date = data.table(
     "date_key" = as.integer(),
     "date" = as.Date(as.character()),
     "year" = as.integer(),
     "quarter" = as.integer(),
     "month" = as.integer(),
     "month_name" = as.character(),
     "month_sname" = as.character(),
     "day" = as.integer())

  start = as.Date('2014-01-01')
  end = as.Date('2016-12-31')

  date_range = as.Date(seq.Date(start, end, by='day'))

  dim_date = data.table(
    date_key = as.integer(format(date_range, '%Y%m%d')),
    date = date_range,
    year = lubridate::year(date_range),
    quarter = lubridate::quarter(date_range),
    month = lubridate::month(date_range),
    month_name = as.character(lubridate::month(date_range, label= T, abbr=F)),
    month_sname = as.character(lubridate::month(date_range, label= T)),
    day = lubridate::day(date_range)
  )


  save(base_fact, file = 'data/base_fact.rda')
  save(base_dim_category, file = 'data/base_dim_category.rda')
  save(base_dim_size, file = 'data/base_dim_size.rda')
  save(fact_sales, file = "data/fact_sales.rda")
  save(fact_sales_denorm, file = 'data/fact_sales_denorm.rda')
  save(src_sales, file = 'data/src_sales.rda')
  save(dim_category, file = 'data/dim_category.rda')
  save(dim_department, file = 'data/dim_department.rda')
  save(dim_item, file = 'data/dim_item.rda')
  save(dim_date, file = 'data/dim_date.rda')

}

tinyDemoDataWarehouseDb <- function() {
  file = tempfile(fileext = '.db')

  ddb = dplyr::src_sqlite(file, create = T)

  db = ddb$con

  DBI::dbWriteTable(db, 'base_fact', base_fact)
  DBI::dbWriteTable(db, 'base_dim_category', base_dim_category)
  DBI::dbWriteTable(db, 'base_dim_size', base_dim_size)
  DBI::dbWriteTable(db, 'fact_sales', fact_sales)
  DBI::dbWriteTable(db, 'fact_sales_denorm', fact_sales_denorm)
  DBI::dbWriteTable(db, 'src_sales', src_sales)
  DBI::dbWriteTable(db, 'dim_category', dim_category)
  DBI::dbWriteTable(db, 'dim_department', dim_department)
  DBI::dbWriteTable(db, 'dim_item', dim_item)
  DBI::dbWriteTable(db, 'dim_date', dim_date)

  ddb
}

tinyDemoDataWarehouse <- function(ddb) {

  cube = data.cube(ddb, json_schema = 'data/dw.json')

  cube
}

tinyFlatDemoDataWarehouse <- function(ddb) {
  dw.t = dplyr::tbl(ddb, 'fact_sales_denorm')
  dw.t
}

