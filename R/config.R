#' Get config file options
#'
#' @param config_file yml file containing the configs. Defaults to `slushy_config.yml` in current working directory. If set to `NULL`, then config file from slushy package will be used.

#' @export
#' @return list of config options and values
#' @importFrom config get
get_config <- function(config_file = "slushy_config.yml"){

  if (is.null(config_file) || !file.exists(config_file)){
    config_file <- system.file("slushy_config.yml", package = "slushy")
  }

  # get name of default config
  config <- get(value = "config_name", config = "default", file = config_file)

  config_list <- get(value = config, config = "default", file = config_file)
  
  # attach the file name as an attribute
  structure(config_list,
                 config_file_name = config_file)
  
  # return the configuration list with the attached attribute
}

#' Create a new slushy_config.yml from template in project directory
#'
#' @param config Name of config to default to
#' @param path Path to store slushy_config.yml. Defaults to current working directory
#'
#' @importFrom cli cli_alert_success cli_alert_danger
#' @importFrom yaml read_yaml write_yaml
#'
#' @export
new_config <- function(config = NULL, path = getwd()){

  all_configs <- read_yaml(system.file("slushy_config.yml", package = "slushy"),
                           eval.expr = FALSE)$default %>% names()

  template_txt <- readLines(system.file("slushy_config.yml", package = "slushy"))

  if (!is.null(config)){

    all_configs <- setdiff(all_configs, "config_name")

    if (! config %in% all_configs){
      cli_alert_danger("config = {config} is not a valid config option")
      return(invisible(NULL))
    }

    # define in the config yml
    which_config_line <- grep("config_name:", template_txt)
    template_txt[which_config_line] <- gsub("(config_name:).*", paste0("\\1 ",config), template_txt[which_config_line])

  }

  writeLines(template_txt, con = file.path(path, "slushy_config.yml"))

  if (file.exists(file.path(path, "slushy_config.yml"))){
    cli_alert_success("`slushy_config.yml` created at {path}.")
  } else {
    cli_alert_danger("Unable to create `slushy_config.yml` created at {path}.")
  }

}


#' Check that required config parameters are present
#' @return Error message if required parameters are absent
#' @noRd
check_config_params <- function(config) {
  # List of required parameters to check
  required_params <- c("rspm_url", "pkg_deps_ok")
  
  # Check for missing or empty parameters
  missing_params <- required_params[sapply(required_params, function(param) {
    is.null(config[[param]]) || length(config[[param]]) == 0
  })]
  
  # If there are any missing or empty parameters, stop the function
  if (length(missing_params) > 0) {
    stop(paste("The following parameters are missing or empty in config file:\n ", paste(missing_params, collapse = ", ")))
  }
}

