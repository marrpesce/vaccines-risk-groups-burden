
# this version of the function uses marginaleffects::avg_comparisons - which is slower, but is necessary if using interaction terms, for increased interpretability


adjusted_estimates <- function(data, subgroup, event_time, event_indicator) {
  # use age-splines unless age is the subgroup of interest

  poisson_formula <- as.formula(glue("event_indicator ~ {subgroup} + sex + ns(age, 3)"))
  if (subgroup == "ageband4") poisson_formula <- as.formula(glue("event_indicator ~ ageband4 + sex"))
  if (subgroup == "ageband13") poisson_formula <- as.formula(glue("event_indicator ~ ageband13 + sex"))

  # prepare dataset
  data_outcome <-
    data |>
    mutate(
      event_time = .data[[event_time]],
      event_indicator = .data[[event_indicator]]
    ) |>
    select(
      all_of(subgroup),
      sex, age,
      event_time,
      event_indicator
    )

  # how many possible values of the group are there
  n_values <- n_distinct(data_outcome[[subgroup]])

  # summarise total people, events, and person-time
  # and add contrast label for merging with model output later
  data_summary <-
    data_outcome |>
    mutate(label = .data[[subgroup]]) |>
    arrange(label) |>
    summarise(
      variable = subgroup,
      n_obs = roundmid_any(n(), sdc_threshold),
      n_event = roundmid_any(sum(event_indicator), sdc_threshold),
      exposure = roundmid_any(sum(event_time), sdc_threshold),

      .by = label
    ) |>
    mutate(
      reference_row = first(label) == label,
      contrast = glue("{label} - {first(label)}")
    )

  # IRR model

  if (n_values > 1) {

    parglm_control <- parglm.control(maxit = 40, nthreads = 4)


    # fit the model
    # if there is an error, just return an empty dataset rather than fail
    data_poisson0 <-
      tryCatch(
        expr = {
          data_outcome |>
            parglm(
              data = _,
              formula = poisson_formula,
              family = poisson,
              offset = log(event_time),
              control = parglm_control
            ) |>
            marginaleffects::avg_comparisons(model = _, type = "link", variables = subgroup, comparison = c("difference")) |>
            broom::tidy()
        },
        error = function(e) {
          cat("error for subgroup", subgroup, ":", conditionMessage(e), "\n")
          data_summary |>
            select(term = variable, contrast) |>
            mutate(estimate = NA_real_, std.error = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
        }
      )

    # combine summary and model outputs
    data_poisson <-
      full_join(
        data_summary,
        data_poisson0,
        by = c("variable" = "term", "contrast"),
      ) |>
      transmute(
        variable, label, reference_row,
        n_obs, n_event, exposure,
        irr = exp(estimate),
        irr.low = exp(conf.low),
        irr.high = exp(conf.high),
        irr.ln.std.error = std.error,
      )

  } else {
    data_poisson <- data_summary |> select(-contrast)
  }

  return(data_poisson)
}
