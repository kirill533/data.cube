context('MapperTestCase')

source('tests/common.R')

self.provider = create_provider("mapper_test.json")

self.cube = self.provider$cube("sales")

naming = list(
  "dimension_prefix" = "dim_",
  "dimension_suffix" = "_dim"
)

self.naming = distill_naming(naming)
self.mapper = StarSchemaMapper$new(cube = self.cube, naming = self.naming)

self.mapper$mappings = list(
  "product.name" = "product.product_name",
  "product.category" = "product.category_id",
  "subcategory.name.en" = "subcategory.subcategory_name_en",
  "subcategory.name.sk" = "subcategory.subcategory_name_sk"
)

test_that('test_logical_reference', {
  dim = self.provider$dimension("date")
  attr = Attribute$new("month", dimension=dim)
  testthat::expect_equal("date.month", attr$ref)

  dim = self.provider$dimension("product")
  attr = Attribute$new("category", dimension=dim)
  testthat::expect_equal("product.category", attr$ref)

  dim = self.provider$dimension("flag")
  attr = Attribute$new("flag", dimension=dim)
  testthat::expect_equal("flag", attr$ref)

  attr = Attribute$new("measure", dimension=None)
  testthat::expect_equal("measure", attr$ref)
})



# def assertMapping(self, expected, logical_ref, mapper=None):
#   """Create string reference by concatentanig table and column name.
# No schema is expected (is ignored)."""
#
# attr = self.cube.attribute(logical_ref)
# mapper = mapper or self.mapper
# ref = mapper[attr]
# sref = ref[1] + "." + ref[2]
#
# self.assertEqual(expected, sref)
#
# def test_physical_refs_dimensions(self):
#   """Testing correct default mappings of dimensions (with and without
# explicit default prefix) in physical references."""
#
# # No dimension prefix
# self.mapper.naming.dimension_prefix = ""
# self.mapper.naming.dimension_suffix = ""
# self.assertMapping("date.year", "date.year")
# self.assertMapping("sales.flag", "flag")
# self.assertMapping("sales.amount", "amount")
#
# # With prefix
# self.mapper.naming.dimension_prefix = "dm_"
# self.assertMapping("dm_date.year", "date.year")
# self.assertMapping("dm_date.month_name", "date.month_name")
# self.assertMapping("sales.flag", "flag")
# self.assertMapping("sales.amount", "amount")
#
# def test_physical_refs_flat_dims(self):
#   self.cube.fact = None
# self.assertMapping("sales.flag", "flag")
#
# def test_physical_refs_facts(self):
#   """Testing correct mappings of fact attributes in physical references"""
#
# fact = self.cube.fact
# self.cube.fact = None
# self.assertMapping("sales.amount", "amount")
# # self.assertEqual("sales.flag", sref("flag.flag"))
# self.cube.fact = fact
#
# def test_physical_refs_with_mappings_and_locales(self):
#   """Testing mappings of mapped attributes and localized attributes in
# physical references"""
#
# self.mapper.mappings = self.cube.mappings
# # Test defaults
# # Localized mapper is localizing to 'sk', non-localized mapper is
# # localizing to default 'en'
# #
# # Mapper with locale that we have
# sk_mapper = StarSchemaMapper(self.cube, self.naming, locale="sk")
#
# # Mapper with locale that we don't have
# de_mapper = StarSchemaMapper(self.cube, self.naming, locale="de")
#
# self.assertMapping("dim_date_dim.month_name", "date.month_name")
#
# self.assertMapping("dim_category_dim.category_name_en",
#                    "product.category_name")
#
# self.assertMapping("dim_category_dim.category_name_sk",
#                    "product.category_name", sk_mapper)
#
# # This should default to 'en' since we don't have 'de' locale and the
# # 'en' locale is the default one
# self.assertMapping("dim_category_dim.category_name_en",
#                    "product.category_name", de_mapper)
#
# # Test with mapping
# self.assertMapping("dim_product_dim.product_name", "product.name")
# self.assertMapping("dim_product_dim.category_id", "product.category")
#
# # The product name is not localized, we should get the same for any
# # mapper
# self.assertMapping("dim_product_dim.product_name", "product.name",
#                    sk_mapper)
# self.assertMapping("dim_product_dim.product_name", "product.name",
#                    de_mapper)
#
# self.assertMapping("dim_category_dim.subcategory_name_en",
#                    "product.subcategory_name")
# self.assertMapping("dim_category_dim.subcategory_name_sk",
#                    "product.subcategory_name",
#                    sk_mapper)
# self.assertMapping("dim_category_dim.subcategory_name_en",
#                    "product.subcategory_name",
#                    de_mapper)
#
