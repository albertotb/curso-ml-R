library(pagedown)
library(tools)

for (file in list.files(path = "./Rmd", pattern = ".Rmd", recursive = TRUE)) {
  input <- file.path("./Rmd", file)
  output <- paste0(file_path_sans_ext(input), ".pdf")
  pagedown::chrome_print(input)
  file.copy(output, "./slides")
  file.remove(output)
}



