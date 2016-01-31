context("EMLParser tests")
test_that("dataone library loads", {
  library(dataone)
})

test_that("EMPParser methods work", {
  emlFile <- system.file("extdata/sample2-eml.xml", package="dataone")
  emlData <- readLines(emlFile)
  expect_gt(length(emlData), 0)
  emlRaw <- charToRaw(paste(emlData, collapse="\n"))
  # Suppress "deprecated" warnings
  suppressWarnings(sciObj <- new("D1Object", id="1234", format="eml://ecoinformatics.org/eml-2.1.1,", data=emlRaw))
  suppressWarnings(emp <- EMLParser(sciObj))
  
  suppressWarnings(result <- documented.entityNames(emp)[1])
  expect_match(result, "All_Fish_All_Years_20130410.csv")
  
  suppressWarnings(result <- documented.d1Identifiers(emp))
  expect_match(result, "http://sbc.lternet.edu/external/Reef/Data/Kelp_Forest_Community_Dynamics/all_fish_all_years_20130208.csv")
  
  suppressWarnings(result <- documented.sizes(emp))
  expect_match(result, "3.6")
  
  suppressWarnings(result <- data.formatFamily(emp, 1))
  expect_match(result, "text/simpleDelimited")
  
  suppressWarnings(result <- data.tableFieldDelimiter(emp, 1))
  expect_match(result, ",")
  
  suppressWarnings(result <- data.tableQuoteCharacter(emp, 1))
  expect_match(result, "\"")
  
  suppressWarnings(result <- data.characterEncoding(emp, 1))
  expect_match(result, "ASCII")
  
  suppressWarnings(result <- data.tableAttributeOrientation(emp,1))
  expect_match(result, "column")
  
  #data.tableSkipLinesHeader(emp,1)
  #data.tableMissingValueCodes(emp,1)
  
  suppressWarnings(result <- data.tableAttributeNames(emp,1)[1])
  expect_match(result, "year")
  
  suppressWarnings(result <- data.tableAttributeNames(emp,1)[2])
  expect_match(result, "site")
  
  suppressWarnings(result <- data.tableAttributeTypes(emp,1)[1])
  expect_match(result, "dateTime")
  
  suppressWarnings(result <- data.tableAttributeTypes(emp,1)[2])
  expect_match(result, "nominal")
  
  suppressWarnings(result <- data.tableAttributeStorageTypes(emp,1)[1])
  expect_match(result, "integer")
  
  suppressWarnings(result <- data.tableAttributeStorageTypes(emp,1)[2])
  expect_match(result,  "string")
  
})