# inla_pipeline.R (single source for young + elderly)
run_inla_age <- function(age_group=c("young","eld"),
                         data_file="../data/tc.mortality.week_final3.csv",
                         repo_root=NULL,
                         out_dir="../output",
                         fig_dir="../output/fig",
                         threshold=17.2,
                         nSamp=800L){
  age_group <- match.arg(age_group)
  if(!is.null(repo_root)) setwd(repo_root)
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)
  if(!dir.exists(fig_dir)) dir.create(fig_dir, recursive=TRUE)

  # ---- Load and standardise the CSV schema (fixed names) ----
  df0 <- read.csv(data_file)
  required <- c(
    "pref","week","year","hol","tmean","pwind","typhoon_no","typhoon_name",
    "totdeath_all","totdeath_age7079","totdeath_age8089","totdeath_age9099",
    "totpop_all","totpop_7079","totpop_8099",
    "income","flood_per","landslide_per","med4_ratio"
  )
  miss <- setdiff(required, names(df0))
  if(length(miss)>0) stop("Missing columns in CSV: ", paste(miss, collapse=", "))

  df <- df0 |>
    dplyr::mutate(
      region_id=as.integer(.data$pref),
      pref=factor(prefecture_names_jp[region_id], levels=prefecture_names_jp),
      week_id=as.integer(dplyr::dense_rank(.data$week)),
      year=as.integer(.data$year),
      holiday=as.integer(.data$hol),
      tmean=as.numeric(.data$tmean),
      pwind=as.numeric(.data$pwind),
      y_young=as.numeric(.data$totdeath_all) - (as.numeric(.data$totdeath_age7079) + as.numeric(.data$totdeath_age8089) + as.numeric(.data$totdeath_age9099)),
      y_eld=as.numeric(.data$totdeath_age7079) + as.numeric(.data$totdeath_age8089) + as.numeric(.data$totdeath_age9099),
      pop_young=(as.numeric(.data$totpop_all) - (as.numeric(.data$totpop_7079) + as.numeric(.data$totpop_8099))) * 1000,
      pop_eld=(as.numeric(.data$totpop_7079) + as.numeric(.data$totpop_8099)) * 1000
    )

  # ---- Wind exposure (thresholded) and lags within prefecture ----
  df <- df |>
    dplyr::mutate(pwind_exp=dplyr::if_else(.data$pwind >= threshold, .data$pwind, 0)) |>
    dplyr::arrange(.data$region_id, .data$week_id) |>
    dplyr::group_by(.data$region_id) |>
    dplyr::mutate(
      exp0=.data$pwind_exp,
      exp1=dplyr::lag(.data$pwind_exp, 1, default=0),
      exp2=dplyr::lag(.data$pwind_exp, 2, default=0)
    ) |>
    dplyr::ungroup()

  # ---- Match original scripts: factor(year) + prefecture-level standardisation of socio covariates ----
  df <- df |> dplyr::mutate(year=factor(.data$year))
  soc_vars <- c("income","flood_per","landslide_per","med4_ratio")
  soc_std <- df |>
    dplyr::group_by(.data$region_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(region_id, dplyr::all_of(soc_vars)) |>
    dplyr::mutate(dplyr::across(-region_id, ~ as.numeric(scale(.))))
  df <- df |>
    dplyr::select(-dplyr::all_of(soc_vars)) |>
    dplyr::left_join(soc_std, by="region_id")

  y_col <- if(age_group=="young") "y_young" else "y_eld"
  pop_col <- if(age_group=="young") "pop_young" else "pop_eld"
  tag <- age_group

  # ---- Spatial ICAR precision ----
  pref_list <- levels(df$pref)
  g <- build_prefecture_graph()
  nb_list <- build_nb_from_graph(g, pref_list)
  Q <- make_icar_precision(nb_list)

  # ---- Temperature splines ----
  tmp <- add_temp_splines(df, pref_list, pref_col="pref", tmean_col="tmean", probs=c(0.25,0.75), degree=3)
  df <- tmp$df
  K <- tmp$K
  for(k in seq_len(K)) df[[paste0("rid",k)]] <- df$region_id

  # ---- Model formula ----
  lags <- c("exp0","exp1","exp2")
  int_terms <- as.vector(outer(lags, soc_vars, paste, sep=":"))
  spline_terms <- vapply(seq_len(K), function(k) sprintf("f(rid%d, spline_temp_%d, model='iid')", k, k), character(1))

  formula_txt <- paste(
    sprintf("%s ~ factor(year) + holiday +", y_col),
    paste(lags, collapse=" + "), "+",
    paste(int_terms, collapse=" + "), "+",
    paste(spline_terms, collapse=" + "), "+",
    "f(region_id, model='generic0', Cmatrix=Q) +",
    "f(week_id, model='rw2', cyclic=TRUE)"
  )

  # ---- Fit INLA ----
  off <- log(df[[pop_col]])
  res <- INLA::inla(as.formula(formula_txt), family="poisson", data=df, offset=off,
    control.compute=list(config=TRUE, dic=TRUE, waic=TRUE, cpo=TRUE),
    control.predictor=list(compute=TRUE))

  # ---- Posterior excess deaths (prefecture-level) ----
  post <- compute_excess_by_prefecture(res, df, pop_col=pop_col, lags=lags, soc_vars=soc_vars, nSamp=nSamp)
  summary_by_region <- post$summary_by_region

  # Save prefecture-level posterior summary (matches your original output)
  write.csv(summary_by_region, file.path(out_dir, sprintf("summary_posterior_%s.csv", tag)), row.names=FALSE)

  # ---- Maps and names ----
  j <- get_jpn_pref_sf(pref_list, path=file.path(dirname(data_file), "gadm_cache"))
  {
    jpn <- j$jpn
    jpn_id <- j$jpn_id
    pref_lookup <- get_english_pref_lookup(jpn, pref_list)

    summary_full <- add_prefecture_names(summary_by_region, pref_lookup)
    write.csv(summary_full, file.path(out_dir, sprintf("summary_%s.csv", tag)), row.names=FALSE)

  # Posterior probability map (prefecture-level)
  pep_df <- summary_by_region |> dplyr::select(region_id, prob_exc_pos)
  map_pep <- jpn_id |> dplyr::left_join(pep_df, by="region_id")
  p_pep <- ggplot2::ggplot(map_pep) +
    ggplot2::geom_sf(ggplot2::aes(fill=.data$prob_exc_pos), colour="white", linewidth=0.2) +
    ggplot2::scale_fill_viridis_c(name="", limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::labs(title=if(tag=="young") "Posterior probability of excess deaths > 0 (young)"
                  else "Posterior probability of excess deaths > 0 (elderly)") +
    ggplot2::theme_minimal(base_size=14) +
    ggplot2::theme(legend.position.inside=c(0.9, 0.3))
  ggplot2::ggsave(filename=file.path(fig_dir, sprintf("posterior_%s.pdf", tag)), plot=p_pep, width=20, height=20)

  # ---- Typhoon-level outputs (prefecture x typhoon; prob + mean/CrI; heatmap) ----
  ty <- make_typhoon_map(df, threshold=threshold)
  ty_map <- ty$ty_map
  ty_levels <- ty$ty_levels

  # (1) posterior prob(excess>0) by prefecture x typhoon
  prob_tc_pos <- compute_prob_tc_pos(df, post$exc, ty_map, pref_lookup, threshold=threshold)
  write.csv(prob_tc_pos, file.path(out_dir, sprintf("prob_tc_pos_%s.csv", tag)), row.names=FALSE)

  # (2) mean (posterior mean per-week) by prefecture x typhoon (legacy CSV in your scripts)
  ty_mean <- compute_typhoon_mean_excess(df, post$exc, ty_map, threshold=threshold)
  write.csv(ty_mean, file.path(out_dir, sprintf("excessmortality_%s_tc.csv", tag)), row.names=FALSE)

  # (3) mean + 95% CrI by prefecture x typhoon
  ty_cri <- compute_typhoon_cri_excess(df, post$exc, ty_map, threshold=threshold)
  write.csv(ty_cri, file.path(out_dir, sprintf("excessdeath_by_cyclone_prefecture_%s.csv", tag)), row.names=FALSE)

  # (4) total excess by typhoon (across all prefectures)
  cyclone_total <- compute_cyclone_total_excess(df, post$exc, ty_map, threshold=threshold)
  write.csv(cyclone_total, file.path(out_dir, sprintf("excessdeath_by_cyclone_%s.csv", tag)), row.names=FALSE)

  # (5) heatmap (prefecture x typhoon) of mean excess deaths
  heat_file <- if(tag=="eld") file.path(fig_dir, "exdeath_tc_eld.pdf") else file.path(fig_dir, "exdeath_tc_young.pdf")
  p_tc_heat <- plot_typhoon_heatmap(ty_cri |> dplyr::select(region_id, .data$ty_label, .data$mean),
                                    pref_lookup, ty_levels, heat_file, width=20, height=10)

  # ---- Social-effect heatmap panel ----
  soc <- df |>
    dplyr::select(region_id, dplyr::all_of(soc_vars)) |>
    dplyr::distinct()
  heat_df <- make_soclag_df(res, soc, pref_lookup, lags, soc_vars)
  p_soclag <- plot_soclag_heatmap(heat_df)

  # Main panel figure (A–D) (same as before)
  p_exc <- plot_choropleth(jpn_id, summary_full, "mean_exc", "Cumulative excess deaths")
  p_pct <- plot_choropleth(jpn_id, summary_full, "mean_mort", "Cumulative excess mortality per 100,000 population", labels=scales::comma)
  # mean_irr is not in summary_full; keep panel C as change per 1 m/s using exp(beta0) is not stored here.
  # We approximate with exp(lag0 fixed mean) as a single map (legacy behaviour differs by script).
R <- max(df$region_id, na.rm=TRUE)

# prefecture-level soc values aligned to region_id=1..R
# ---- Panel C: Overall mortality rate change (%) per 1 m/s increase in windspeed ----
R <- max(df$region_id, na.rm=TRUE)

# 1) coefficient samples (15 x nSamp) in fixed order
lags <- c("exp0","exp1","exp2")
gamma_names <- as.vector(outer(lags, soc_vars, paste, sep=":"))
terms <- c(lags, gamma_names)

theta_samp <- extract_fixed_samples(res, terms=terms, nSamp=nSamp, seed=1)
# rows: terms, cols: samples

beta_samp  <- theta_samp[lags, , drop=FALSE]         # 3 x nSamp
gamma_samp <- theta_samp[gamma_names, , drop=FALSE]  # 12 x nSamp

# 2) sum over lags (sample-wise), exactly like original
beta_sum_samp <- colSums(beta_samp)  # length nSamp

gamma_sum_mat <- sapply(soc_vars, function(sv){
  idx <- match(paste0(lags, ":", sv), rownames(gamma_samp))
  colSums(gamma_samp[idx, , drop=FALSE])
})
gamma_sum_mat <- t(gamma_sum_mat)    # 4 x nSamp

# 3) prefecture-level soc matrix aligned by region_id (same intent as original distinct)
soc_mat <- df |>
  dplyr::distinct(region_id, .keep_all=TRUE) |>
  dplyr::arrange(.data$region_id) |>
  dplyr::select(dplyr::all_of(soc_vars)) |>
  as.matrix()
stopifnot(nrow(soc_mat) == R)

# 4) slope per prefecture per sample, then summarise exp(slope)
gamma_eff_mat <- soc_mat %*% gamma_sum_mat           # R x nSamp
slope_mat <- sweep(gamma_eff_mat, 2, beta_sum_samp, "+")  # R x nSamp

irr_summary <- t(apply(exp(slope_mat), 1, function(x)
  c(mean=mean(x),
    lower=stats::quantile(x, 0.025, names=FALSE),
    upper=stats::quantile(x, 0.975, names=FALSE))
))
colnames(irr_summary) <- c("mean","lower","upper")

irr_df <- tibble::tibble(
  region_id=seq_len(R),
  mean_irr=irr_summary[, "mean"],
  lower_irr=irr_summary[, "lower"],
  upper_irr=irr_summary[, "upper"]
) |>
  add_prefecture_names(pref_lookup)
p_irr <- plot_choropleth(jpn_id, irr_df, "mean_irr",
  "Overall mortality rate change (%)\nper 1 m/s increase in windspeed",
  transform=function(x) (x-1)*100, labels=scales::comma)

  fig_file <- file.path(fig_dir, sprintf("exdeath_%s.pdf", tag))
  export_panel_figure(p_exc, p_pct, p_irr, p_soclag, fig_file)

    }

  list(res=res, df=df, post=post,summary_full=summary_full,
       prob_tc_pos=prob_tc_pos,ty_mean=ty_mean, ty_cri=ty_cri, 
       cyclone_total=cyclone_total,
       figs=list(main=fig_file, posterior=file.path(fig_dir, sprintf("posterior_%s.pdf", tag)), typhoon_heat=heat_file))
}