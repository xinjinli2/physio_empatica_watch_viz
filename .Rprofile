# ------------------------------------------------------------------------------
# .Rprofile — for Empatica Watch App (Local + shinyapps.io compatible)
# ------------------------------------------------------------------------------

# Set your virtual environment name here (can be any name you like)
VIRTUALENV_NAME <- "watchapp_env"

if (Sys.info()[['user']] == 'shiny') {
  Sys.setenv(PYTHON_PATH = 'python3')
  Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME)
  # Sys.setenv(RETICULATE_PYTHON = paste0('/home/shiny/.virtualenvs/', VIRTUALENV_NAME, '/bin/python'))
} else if (Sys.info()[['user']] == 'rstudio-connect') {
  # For RStudio Connect servers (optional, if you ever move to them)
  Sys.setenv(PYTHON_PATH = '/opt/python/3.7.7/bin/python3')
  Sys.setenv(VIRTUALENV_NAME = paste0(VIRTUALENV_NAME, '/'))
  # Sys.setenv(RETICULATE_PYTHON = paste0(VIRTUALENV_NAME, '/bin/python'))
  
} else {
  # Running locally (your Mac or laptop)
  options(shiny.port = 7450)  # You can change the default port if needed
  Sys.setenv(PYTHON_PATH = 'python3')  # You may change this to a full path if needed
  Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME)
  # RETICULATE_PYTHON is not needed locally; reticulate finds ~/.virtualenvs/<env>/bin/python
}

# If running locally, use system Python directly instead of a virtualenv
if (Sys.info()[['user']] != 'shiny' && Sys.info()[['user']] != 'rstudio-connect') {
  # reticulate::use_python("/Users/xinjinli/Library/r-miniconda-arm64/bin/python3", required = TRUE)
  reticulate::use_condaenv("base", conda = "/Users/xinjinli/Library/r-miniconda-arm64/bin/conda", required = TRUE)
  
}

