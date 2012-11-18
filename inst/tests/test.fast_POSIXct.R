load_all('holstius')

context('fast_POSIXct')

local_tz <- 'America/Los_Angeles'

test_that('Leap day', {
	x <- ISOdate(2000, 02, 29, 01, 02, 03, tz=local_tz)
	expect_equal(x, fast_POSIXct(format(x), local_tz))
})

test_that('Daylight savings', {
	x <- ISOdate(2000, 06, 01, 01, 02, 03, tz=local_tz)
	expect_equal(x, fast_POSIXct(format(x), local_tz))
})
