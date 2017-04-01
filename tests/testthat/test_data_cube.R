library(data.table)
library(DBI)
library(RSQLite)

context('SchemaBasicsTestCase')

test_that('test_aggregate_no_params', {
  db = tinyDemoDataWarehouseDb()
  dwh = tinyDemoDataWarehouse(db)
  dwh_flat = tinyFlatDemoDataWarehouse(db)

  res = select(dwh)

  expect_null(select(dwh))

})

test_that('test_aggregate_measure', {
  db = tinyDemoDataWarehouseDb()
  dwh = tinyDemoDataWarehouse(db)
  dwh_flat = tinyFlatDemoDataWarehouse(db)
  dwh.f.dt = as.data.frame(dwh_flat)

  expect_equal(sum(dwh.f.dt$quantity), select.data.cube(dwh, rows = ('quantity')))

})
