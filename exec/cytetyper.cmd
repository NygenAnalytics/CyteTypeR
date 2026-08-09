@echo off
setlocal
"%~dp0Rscript.exe" --vanilla -e "status <- tryCatch({suppressPackageStartupMessages(library(CyteTypeR)); CyteTypeR:::.cytetyper_cli_main(commandArgs(trailingOnly = TRUE))}, interrupt = function(error) {cat('Cancelled.\n', file = stderr()); 130L}, error = function(error) {cat('Error: Unable to load CyteTypeR: ', conditionMessage(error), '\n', sep = '', file = stderr()); 1L}); quit(save = 'no', status = as.integer(status), runLast = FALSE)" %*
exit /b %ERRORLEVEL%
