options(max.print = 100)
options(sci.pen = 10)
options(editor = "emacs")
options(repos = c(CRAN = "https://cran.rstudio.com/"))
options(download.file.method = "libcurl")
options(menu.graphics = FALSE)

options(tigris_use_cache = TRUE)

options(
  usethis.full_name = "Cole Brokamp",
  usethis.description = list(
    `Authors@R` = 'person("Cole", "Brokamp", email = "cole@colebrokamp.com", role = c("aut", "cre"), 
    comment = c(ORCID = "0000-0002-0289-3151"))',
    Version = "0.1"
   )
 )

if (identical(
  find.package("rlang", quiet = TRUE),
  character(0)
)) {
  message("no rlang")
} else {
  rlang::global_entrace(enable = TRUE, class = c("error", "warning", "message"))
}

## if (interactive() && identical(find.package("httpgd", quiet = TRUE), character(0))) {
##   message("no httpgd")
## } else {
##   httpgd::hgd()
## }
