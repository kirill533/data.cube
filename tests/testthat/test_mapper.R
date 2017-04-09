context('MapperTestCase')

self.setUp <- function(json = "mapper_test.json") {
  source('tests/common.R')

  self.provider <<- create_provider(json)

  self.cube <<- self.provider$cube("sales")

  naming <<- list(
    "dimension_prefix" = "dim_",
    "dimension_suffix" = "_dim"
  )

  self.naming <<- distill_naming(naming)
  self.mapper <<- StarSchemaMapper$new(cube = self.cube, naming = self.naming)

  self.mapper$mappings <<- list(
    "product.name" = "product.product_name",
    "product.category" = "product.category_id",
    "subcategory.name.en" = "subcategory.subcategory_name_en",
    "subcategory.name.sk" = "subcategory.subcategory_name_sk"
  )
}


test_that('test_logical_reference', {

  self.setUp()

  dim = self.provider$dimension("date")
  attr = Attribute$new(name="month", dimension=dim)
  testthat::expect_equal("date.month", attr$ref)

  dim = self.provider$dimension("product")
  attr = Attribute$new(name="category", dimension=dim)
  testthat::expect_equal("product.category", attr$ref)

  dim = self.provider$dimension("flag")
  attr = Attribute$new(name="flag", dimension=dim)
  testthat::expect_equal("flag", attr$ref)

  attr = Attribute$new(name="measure", dimension=NULL)
  testthat::expect_equal("measure", attr$ref)
})



self.assertMapping <- function(expected, logical_ref, mapper=NULL) {
  # Create string reference by concatentanig table and column name.
  # No schema is expected (is ignored).


  attr = self.cube$attribute(logical_ref)
  mapper = nvl(mapper, self.mapper)
  ref = mapper$getitem(attr)
  sref = paste0(ref$table, ".", ref$column)

  testthat::expect_equal(expected, sref)
}


test_that('test_physical_refs_dimensions', {

  self.setUp()

  # Testing correct default mappings of dimensions (with and without
  # explicit default prefix) in physical references.

  # No dimension prefix
  self.mapper$naming$dimension_prefix = ""
  self.mapper$naming$dimension_suffix = ""
  self.assertMapping("date.year", "date.year")
  self.assertMapping("sales.flag", "flag")
  self.assertMapping("sales.amount", "amount")

  # With prefix
  self.mapper$naming$dimension_prefix = "dm_"
  self.assertMapping("dm_date.year", "date.year")
  self.assertMapping("dm_date.month_name", "date.month_name")
  self.assertMapping("sales.flag", "flag")
  self.assertMapping("sales.amount", "amount")

})

test_that('test_physical_refs_flat_dims', {
  self.setUp()

  self.cube$fact = NULL

  self.assertMapping("sales.flag", "flag")
})




test_that('test_physical_refs_facts', {
  # """Testing correct mappings of fact attributes in physical references"""

  self.setUp()

  fact = self.cube$fact
  self.cube$fact = NULL
  self.assertMapping("sales.amount", "amount")
  # self.assertEqual("sales.flag", sref("flag.flag"))
  self.cube$fact = fact
})

test_that('test_physical_refs_with_mappings_and_locales', {
  #   """Testing mappings of mapped attributes and localized attributes in
  # physical references"""

  self.setUp("mapper_test_multilang.json")

  self.cube_de <<- self.provider$cube("sales_de")
  self.cube_sk <<- self.provider$cube("sales_sk")

  self.mapper$mappings = self.cube$mappings
  # Test defaults
  # Localized mapper is localizing to 'sk', non-localized mapper is
  # localizing to default 'en'
  #
  # Mapper with locale that we have
  sk_mapper = StarSchemaMapper(self.cube_sk, self.naming)

  # Mapper with locale that we don't have
  de_mapper = StarSchemaMapper(self.cube_de, self.naming)

  self.assertMapping("dim_date_dim.month_name", "date.month_name")

  self.assertMapping("dim_category_dim.category_name_en",
                     "product.category_name")

  self.assertMapping("dim_category_dim.category_name_sk",
                     "product.category_name", sk_mapper)

  # This should default to 'en' since we don't have 'de' locale and the
  # 'en' locale is the default one
  self.assertMapping("dim_category_dim.category_name_en",
                     "product.category_name", de_mapper)

  # Test with mapping
  self.assertMapping("dim_product_dim.product_name", "product.name")
  self.assertMapping("dim_product_dim.category_id", "product.category")

  # The product name is not localized, we should get the same for any
  # mapper
  self.assertMapping("dim_product_dim.product_name", "product.name",
                     sk_mapper)
  self.assertMapping("dim_product_dim.product_name", "product.name",
                     de_mapper)

  self.assertMapping("dim_category_dim.subcategory_name_en",
                     "product.subcategory_name")
  self.assertMapping("dim_category_dim.subcategory_name_sk",
                     "product.subcategory_name",
                     sk_mapper)
  self.assertMapping("dim_category_dim.subcategory_name_en",
                     "product.subcategory_name",
                     de_mapper)

})

