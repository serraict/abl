library('RUnit')
library('testthat')

test.suite <- defineTestSuite("all tests",
                              dirs = file.path("src/tests"),
                              testFileRegexp = '\\.[R|r]')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)