options(max.print = 100)
options(sci.pen = 10)
options(editor = "emacs")
options(repos = c(CRAN = "https://cran.rstudio.com/"))
options(download.file.method = "libcurl")
options(menu.graphics = FALSE)
options(prompt='R > ')

options(tigris_use_cache = TRUE)
## options(tigris_class = "sf")

options(
  usethis.full_name = "Cole Brokamp",
  usethis.description = list(
    `Authors@R` = 'person("Cole", "Brokamp", email = "cole.brokamp@gmail.com", role = c("aut", "cre"), 
    comment = c(ORCID = "0000-0002-0289-3151"))',
    Version = "0.1"
   )
 )

if ('rlang' %in% loadedNamespaces()) options(error = rlang::entrace)

# make sure that $COLUMNS is available (exported in bash_profile)
.set_width <- function(col_width = Sys.getenv("COLUMNS")) options(width=as.integer(col_width))

# .First <- function(){
#     if (Sys.getenv("RSTUDIO") == "") .set_width()
# }
