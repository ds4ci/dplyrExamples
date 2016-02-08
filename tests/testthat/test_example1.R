library(dplyrExamples)
context("Example 1 - BLS Example")
## test file is just monthly employmnet for AL areas over 2015.
fn <- system.file("extdata", "BLS_NonFarmEmploymentInAreasAL_2015.tsv",
                  package = "dplyrExamples")
file_out <- "file_out.tsv"
BLS_CleanRawTextFile(fn, file_out)
fo <- read_tsv(file_out)
expect_equal(dim(fo), c(12, 13))
expect_equal(sum(fo[, -1]), 18846.3)
file.remove(file_out)
rm(fo)
