# some options
options(max.print=100)
options(sci.pen=10)
options(editor="vim")
options(repos=c(CRAN = "https://cran.rstudio.com/"))
options(download.file.method="libcurl")
options(menu.graphics=FALSE)
options(prompt='R > ')

# tab completion for library
utils::rc.settings(ipck=TRUE)

# set system variables
# if on CCHMC HPC, set proxy variables
if (Sys.info()['user'] == 'broeg1') { # better way to determine this?
    Sys.setenv(http_proxy='http://srv-sysproxy:ieQu3nei@bmiproxyp.chmcres.cchmc.org:80')
    Sys.setenv(https_proxy='https://srv-sysproxy:ieQu3nei@bmiproxyp.chmcres.cchmc.org:80')
}

# auto set R to max columns based on terminal size
# make sure that $COLUMNS is available (exported in bash_profile)
if (Sys.getenv("RSTUDIO") == "") {
    wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
      options(width=as.integer(howWide))
    }
    wideScreen()
}

# make sure that these packages are available

suppressPackageStartupMessages({

    library(stats)
    library(graphics)
    library(grDevices)
    library(datasets)
    library(utils)
    library(methods)
    library(base)

    if (suppressWarnings(!require(devtools, quietly=TRUE))) install.packages('devtools')
    if (suppressWarnings(!require(colorout, quietly=TRUE))) devtools::install_github('jalvesaq/colorout')
    if (interactive()) {
        if (suppressWarnings(!require(tidyverse, quietly=TRUE))) install.packages('tidyverse')
        if (suppressWarnings(!require(lookup, quietly=TRUE))) devtools::install_github('jimhester/lookup')
        if (suppressWarnings(!require(CB, quietly=TRUE))) devtools::install_github('cole-brokamp/CB')
    }

})

# use colorout if possible
if(Sys.getenv("TERM") == "xterm-256color") library(colorout)

# use lookup during interactive sessions
if(interactive()) suppressPackageStartupMessages(library(lookup))

# never save on quit
q <- function (save="no", ...) {
  quit(save=save, ...)
}

# print time and wd on startup
.First <- function(){
  if(interactive()){
    library(utils)
    timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))

  }
}
