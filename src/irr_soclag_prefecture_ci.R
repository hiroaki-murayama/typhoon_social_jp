# irr_soclag_prefecture_ci.R
# Prefecture-level lag x social-indicator mortality-rate change tables with 95% CI.
# This reproduces the column layout used in the original TCdeath_social outputs.

.check_irr_soclag_dependencies <- function() {
  needed <- c("make_soclag_df", "get_english_pref_lookup", "run_inla_age")
  missing <- needed[!vapply(needed, exists, logical(1), mode = "function")]
  if (length(missing) > 0) {
    stop(
      "Missing required functions: ",
      paste(missing, collapse = ", "),
      ". Source ../src/utils.R and ../src/inla_pipeline.R first."
    )
  }
}

make_irr_soclag_prefecture_ci_df <- function(
  res,
  df,
  pref_lookup = NULL,
  lags = c("exp0", "exp1", "exp2"),
  soc_vars = c("income", "flood_per", "landslide_per", "med4_ratio")
) {
  .check_irr_soclag_dependencies()

  if (is.null(pref_lookup)) {
    pref_lookup <- get_english_pref_lookup()
  }

  needed_cols <- c("region_id", soc_vars)
  miss <- setdiff(needed_cols, names(df))
  if (length(miss) > 0) {
    stop("Missing columns in `df`: ", paste(miss, collapse = ", "))
  }

  needed_terms <- c(lags, as.vector(outer(lags, soc_vars, paste, sep = ":")))
  miss_terms <- setdiff(needed_terms, rownames(res$summary.fixed))
  if (length(miss_terms) > 0) {
    stop("Missing fixed effects in `res$summary.fixed`: ", paste(miss_terms, collapse = ", "))
  }

  soc <- df |>
    dplyr::select(region_id, dplyr::all_of(soc_vars)) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$region_id)

  heat_df <- make_soclag_df(res, soc, pref_lookup, lags, soc_vars)

  lag_back <- c(
    "lag 0 week" = "exp0",
    "lag 1 week" = "exp1",
    "lag 2 weeks" = "exp2"
  )
  soc_back <- c(
    "prefecture income" = "income",
    "potential flood risk" = "flood_per",
    "potential landslide risk" = "landslide_per",
    "distance to\nmedical facilities" = "med4_ratio"
  )

  heat_df |>
    dplyr::mutate(
      lag_raw = unname(lag_back[as.character(.data$lag)]),
      soc_raw = unname(soc_back[as.character(.data$soc)]),
      eta_hat = .data$beta + .data$gamma * .data$soc_val,
      eta_var = res$summary.fixed[.data$lag_raw, "sd"]^2 +
        (.data$soc_val^2) * res$summary.fixed[paste0(.data$lag_raw, ":", .data$soc_raw), "sd"]^2,
      mean = .data$irr,
      lower = (exp(.data$eta_hat - 1.96 * sqrt(.data$eta_var)) - 1) * 100,
      upper = (exp(.data$eta_hat + 1.96 * sqrt(.data$eta_var)) - 1) * 100,
      prefecture_en = as.character(.data$prefecture_en),
      lag = as.character(.data$lag),
      soc = as.character(.data$soc)
    ) |>
    dplyr::transmute(
      region_id,
      prefecture_en,
      lag,
      soc,
      mean,
      lower,
      upper
    )
}

write_irr_soclag_prefecture_ci <- function(
  run_obj,
  age_group = c("young", "eld"),
  out_dir = "../output",
  file = NULL
) {
  age_group <- match.arg(age_group)

  if (is.null(run_obj$res) || is.null(run_obj$df)) {
    stop("`run_obj` must contain at least `$res` and `$df`. Pass the return value of `run_inla_age()`.")
  }

  if (is.null(file)) {
    file <- file.path(out_dir, sprintf("irr_soclag_prefecture_ci_%s.csv", age_group))
  }

  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  pref_lookup <- if (!is.null(run_obj$summary_full) &&
    all(c("region_id", "prefecture_en") %in% names(run_obj$summary_full))) {
    run_obj$summary_full |>
      dplyr::select(.data$region_id, .data$prefecture_en) |>
      dplyr::distinct() |>
      dplyr::arrange(.data$region_id)
  } else {
    get_english_pref_lookup() |>
      dplyr::select(.data$region_id, .data$prefecture_en)
  }

  irr_soclag_ci <- make_irr_soclag_prefecture_ci_df(
    res = run_obj$res,
    df = run_obj$df,
    pref_lookup = pref_lookup
  )

  write.csv(irr_soclag_ci, file, row.names = FALSE)
  invisible(irr_soclag_ci)
}

run_and_write_irr_soclag_prefecture_ci <- function(
  age_group = c("young", "eld"),
  data_file = "../data/typhoon_mortality_dummy.csv",
  repo_root = NULL,
  out_dir = "../output",
  fig_dir = "../output/fig",
  threshold = 17.2,
  nSamp = 800L,
  seed = 1L,
  num_threads = INLA::inla.getOption("num.threads")
) {
  .check_irr_soclag_dependencies()
  age_group <- match.arg(age_group, several.ok = TRUE)

  results <- lapply(age_group, function(tag) {
    run_obj <- run_inla_age(
      age_group = tag,
      data_file = data_file,
      repo_root = repo_root,
      out_dir = out_dir,
      fig_dir = fig_dir,
      threshold = threshold,
      nSamp = nSamp,
      seed = seed,
      num_threads = num_threads
    )

    out_file <- file.path(out_dir, sprintf("irr_soclag_prefecture_ci_%s.csv", tag))
    irr_soclag_ci <- write_irr_soclag_prefecture_ci(
      run_obj = run_obj,
      age_group = tag,
      file = out_file
    )

    list(
      run = run_obj,
      irr_soclag_ci = irr_soclag_ci,
      file = out_file
    )
  })

  names(results) <- age_group
  if (length(results) == 1) {
    results[[1]]
  } else {
    results
  }
}
