make_clean <- function(str) {
  str_glue("{str}--CLEAN.rds")
}

make_raw <- function(str) {
  str_glue("{str}--RAW.rds")
}

get_survey_data <- function(survey_id, should_source = SHOULD_SOURCE, file_name = CLEAN_FILE_NAME) {
  if(should_source) {
    api_key <- Sys.getenv("QUALTRICS_API_KEY_USC")
    base_url <- Sys.getenv("QUALTRICS_BASE_URL_USC")
    qualtrics_api_credentials(api_key = api_key,
                              base_url = base_url)
    
    df <- fetch_survey(surveyID = survey_id,
                       verbose = FALSE,
                       label = FALSE,
                       convert = FALSE) 
    df |> write_rds(here(DIR_DATA, DIR_RAW, make_raw(file_name)))
    
  } else {
    file_url <- here(DIR_DATA, DIR_RAW, make_raw(file_name))
    if(file.exists(file_url)) {
      df <- read_rds(file_url)
    } else {
      cat("File does not exist! Set Fetch to TRUE and download.")
    }
  }
  return(df)
}


tab_data <- function(data, var, ...) {
  if (rlang::dots_n(...) == 0) {
    # If no grouping variables are provided
    data |> 
      count({{ var }}, name = "n") |> 
      mutate(p = prop.table(n))
  } else {
    # If grouping variables are provided
    data %>%
      group_by(...) |> 
      count({{ var }}, name = "n") |> 
      mutate(p = n / sum(n)) |> 
      ungroup()
  }
}

# PLOT FUNCTIONS

theme_colors_variable <- function(color="#DDD9D4") {
  theme(
    panel.background = element_rect(fill = color, color = color),
    plot.background = element_rect(fill = color, color = color)
  )
}

strip_margins <- function() {
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"),
    plot.title.position = "plot"
  )
}

flush_left_y <- function() {
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = 0)
  )
}

strip_grid <- function() {
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
}


bar_chart <- function(tbl, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  
  tbl |> 
    ggplot(aes(x={{ x }}, y=fct_rev({{ y }}))) +
    geom_bar(stat = "identity", show.legend = FALSE, fill="#111111") +
    geom_text(aes(label=percent({{ x }}, 1)),
              color = "white",
              fontface = "bold",
              hjust = 1,
              size = 7) +
    strip_grid() +
    strip_margins() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 18)
    ) +
    labs(x="", y="")
}
