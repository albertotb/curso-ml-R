library(pagedown)
library(tools)

input_dir  <- "./Rmd"
output_dir <- "./slides"

# 1. Take all .Rmd files from `input_dir`
# 2. Convert them to PDF using `chrome_print()`
# 3. Move them to `output_dir`
for (file in list.files(path = "./Rmd", pattern = ".Rmd", recursive = TRUE)) {
  input <- file.path("./Rmd", file)
  output <- paste0(file_path_sans_ext(input), ".pdf")
  pagedown::chrome_print(input)
  file.copy(output, "./slides")
  file.remove(output)
}



