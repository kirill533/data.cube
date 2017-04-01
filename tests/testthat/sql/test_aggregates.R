context('AggregatesTestCase')

test_that('setUp', {

})


test_that('test_unknown_function', {

})
        browser = self.workspace.browser("unknown_function")

        with self.assertRaisesRegex(ArgumentError, "Unknown.*function"):
            browser.aggregate()

test_that('test_explicit', {

})
        browser = self.workspace.browser("default")
        result = browser.aggregate()
        summary = result.summary
        self.assertEqual(60, summary["amount_sum"])
        self.assertEqual(16, summary["count"])

test_that('test_post_calculation', {

})
        browser = self.workspace.browser("postcalc_in_measure")

        result = browser.aggregate(drilldown=["year"])
        cells = list(result.cells)
        aggregates = sorted(cells[0].keys())
        self.assertSequenceEqual(['amount_sma', 'amount_sum', 'count', 'year'],
                                 aggregates)
