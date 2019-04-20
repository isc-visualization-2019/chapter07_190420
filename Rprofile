### This is the system Rprofile file. It is always run on startup.
### Additional commands can be placed in site or user Rprofile files
### (see ?Rprofile).

### Copyright (C) 1995-2015 The R Core Team

### Notice that it is a bad idea to use this file as a template for
### personal startup files, since things will be executed twice and in
### the wrong environment (user profiles are run in .GlobalEnv).

.GlobalEnv <- globalenv()
attach(NULL, name = "Autoloads")
.AutoloadEnv <- as.environment(2)
assign(".Autoloaded", NULL, envir = .AutoloadEnv)
T <- TRUE
F <- FALSE
R.version <- structure(R.Version(), class = "simple.list")
version <- R.version            # for S compatibility

## for backwards compatibility only
R.version.string <- R.version$version.string

## NOTA BENE: options() for non-base package functionality are in places like
##            --------- ../utils/R/zzz.R

options(keep.source = interactive())
options(warn = 0)
# options(repos = c(CRAN="@CRAN@"))
# options(BIOC = "http://www.bioconductor.org")

options(timeout = 60)
options(encoding = "native.enc")
options(show.error.messages = TRUE)
## keep in sync with PrintDefaults() in  ../../main/print.c :
options(scipen = 0)
options(max.print = 99999)# max. #{entries} in internal printMatrix()
options(add.smooth = TRUE)# currently only used in 'plot.lm'
options(stringsAsFactors = TRUE)
if(!interactive() && is.null(getOption("showErrorCalls")))
    options(showErrorCalls = TRUE)

local({dp <- Sys.getenv("R_DEFAULT_PACKAGES")
       if(identical(dp, "")) ## it fact methods is done first
           dp <- c("datasets", "utils", "grDevices", "graphics",
                   "stats", "methods")
       else if(identical(dp, "NULL")) dp <- character(0)
       else dp <- strsplit(dp, ",")[[1]]
       dp <- sub("[[:blank:]]*([[:alnum:]]+)", "\\1", dp) # strip whitespace
       options(defaultPackages = dp)
    })

## Expand R_LIBS_* environment variables.
Sys.setenv(R_LIBS_SITE =
           .expand_R_libs_env_var(Sys.getenv("R_LIBS_SITE")))
Sys.setenv(R_LIBS_USER =
           .expand_R_libs_env_var(Sys.getenv("R_LIBS_USER")))

local({
    if(nzchar(tl <- Sys.getenv("R_SESSION_TIME_LIMIT_CPU")))
        setSessionTimeLimit(cpu = tl)
    if(nzchar(tl <- Sys.getenv("R_SESSION_TIME_LIMIT_ELAPSED")))
        setSessionTimeLimit(elapsed = tl)
})

.First.sys <- function()
{
    for(pkg in getOption("defaultPackages")) {
        res <- require(pkg, quietly = TRUE, warn.conflicts = FALSE,
                       character.only = TRUE)
        if(!res)
            warning(gettextf('package %s in options("defaultPackages") was not found', sQuote(pkg)),
                    call. = FALSE, domain = NA)
    }
}

## called at C level in the startup process prior to .First.sys
.OptRequireMethods <- function()
{
    pkg <- "methods" # done this way to avoid R CMD check warning
    if(pkg %in% getOption("defaultPackages"))
        if(!require(pkg, quietly = TRUE, warn.conflicts = FALSE,
                    character.only = TRUE))
            warning('package "methods" in options("defaultPackages") was not found',
                    call. = FALSE)
}

if(nzchar(Sys.getenv("R_BATCH"))) {
    .Last.sys <- function()
    {
        cat("> proc.time()\n")
        print(proc.time())
    }
    ## avoid passing on to spawned R processes
    ## A system has been reported without Sys.unsetenv, so try this
    try(Sys.setenv(R_BATCH=""))
}
###-*- R -*- Unix Specific ----

.Library <- file.path(R.home(), "library")
.Library.site <- Sys.getenv("R_LIBS_SITE")
.Library.site <- if(!nchar(.Library.site)) file.path(R.home(), "site-library") else unlist(strsplit(.Library.site, ":"))
.Library.site <- .Library.site[file.exists(.Library.site)]

invisible(.libPaths(c(unlist(strsplit(Sys.getenv("R_LIBS"), ":")),
                      unlist(strsplit(Sys.getenv("R_LIBS_USER"), ":")
                      ))))
local({
    popath <- Sys.getenv("R_TRANSLATIONS", "")
    if(!nzchar(popath)) {
        paths <- file.path(.libPaths(), "translations", "DESCRIPTION")
        popath <- dirname(paths[file.exists(paths)][1])
    }
    bindtextdomain("R", popath)
    bindtextdomain("R-base", popath)
    assign(".popath", popath, .BaseNamespaceEnv)
})
local({
## we distinguish between R_PAPERSIZE as set by the user and by configure
papersize <- Sys.getenv("R_PAPERSIZE_USER")
if(!nchar(papersize)) {
    lcpaper <- Sys.getlocale("LC_PAPER") # might be null: OK as nchar is 0
    papersize <- if(nchar(lcpaper))
        if(length(grep("(_US|_CA)", lcpaper))) "letter" else "a4"
    else Sys.getenv("R_PAPERSIZE")
}
options(papersize = papersize,
        printcmd = Sys.getenv("R_PRINTCMD"),
        dvipscmd = Sys.getenv("DVIPS", "dvips"),
        texi2dvi = Sys.getenv("R_TEXI2DVICMD"),
        browser = Sys.getenv("R_BROWSER"),
        pager = file.path(R.home(), "bin", "pager"),
        pdfviewer = Sys.getenv("R_PDFVIEWER"),
        useFancyQuotes = TRUE)
})

## non standard settings for the R.app GUI of the macOS port
if(.Platform$GUI == "AQUA") {
    ## this is set to let RAqua use both X11 device and X11/TclTk
    if (Sys.getenv("DISPLAY") == "")
	Sys.setenv("DISPLAY" = ":0")

    ## this is to allow gfortran compiler to work
    Sys.setenv("PATH" = paste(Sys.getenv("PATH"),":/usr/local/bin",sep = ""))
}## end "Aqua"

## de-dupe the environment on macOS (bug in Yosemite which affects things like PATH)
if (grepl("^darwin", R.version$os)) local({
    ## we have to de-dupe one at a time and re-check since the bug affects how
    ## environment modifications propagate
    while(length(dupes <- names(Sys.getenv())[table(names(Sys.getenv())) > 1])) {
        env <- dupes[1]
        value <- Sys.getenv(env)
        Sys.unsetenv(env)             ## removes the dupes, good
        .Internal(Sys.setenv(env, value)) ## wrapper requries named vector, a pain, hence internal
    }
})

local({
    tests_startup <- Sys.getenv("R_TESTS")
    if(nzchar(tests_startup)) source(tests_startup)
})




############################################
# R.home(component = "home") #해당 경로 확인하기
############################################

setHook(packageEvent("grDevices", "onLoad"),
        function(...){
        if(capabilities("aqua"))
            grDevices::quartzFonts(
              sans =grDevices::quartzFont(rep("AppleGothic",4)),
              serif=grDevices::quartzFont(rep("AppleMyungjp",4)))
        grDevices::pdf.options(family="Korea1")
        grDevices::ps.options(family="Korea1")
        }
)
attach(NULL, name = "KoreaEnv")
assign("familyset_hook",
       function() {
            macfontdevs=c("quartz","quartz_off_screen")
            devname=strsplit(names(dev.cur()),":")[[1L]][1]
            if (capabilities("aqua") &&
                devname %in% macfontdevs)
                    par(family="sans")
       },
       pos="KoreaEnv")
setHook("plot.new", get("familyset_hook", pos="KoreaEnv"))
setHook("persp", get("familyset_hook", pos="KoreaEnv"))



suppressPackageStartupMessages({
library(ggplot2)
library(ggthemes)
library(extrafont)
})


theme_woons<- function(base_size = 12, base_family = "NanumSquare"){
     (theme_foundation(base_size = base_size, base_family = base_family) +
        theme(line = element_line(colour = "black"), rect = element_rect(fill = "#F0F0F0",
            linetype = 0, colour = NA), text = element_text(colour = '#3C3C3C'),
            axis.title = element_text(), axis.text = element_text(),
            axis.ticks = element_blank(), axis.line = element_blank(),
            legend.background = element_rect(), legend.position = "bottom",
            legend.direction = "horizontal", legend.box = "vertical",
            panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = '#D2D2D2'),
            panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0,
                size = rel(1.5), face = "bold"), plot.margin = grid::unit(c(1,
                1, 0.5, 0.5), "lines"), strip.background = element_rect(), panel.margin.x=NULL, panel.margin.y=NULL))
}

theme_set(theme_woons())