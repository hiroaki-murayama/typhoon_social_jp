prefecture_names_jp <- c(
  "北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
  "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
  "新潟県","富山県","石川県","福井県","山梨県","長野県",
  "岐阜県","静岡県","愛知県","三重県","滋賀県","京都府","大阪府",
  "兵庫県","奈良県","和歌山県","鳥取県","島根県","岡山県","広島県","山口県",
  "徳島県","香川県","愛媛県","高知県",
  "福岡県","佐賀県","長崎県","熊本県","大分県","宮崎県","鹿児島県","沖縄県"
)

prefecture_names_en <- c(
  "Hokkaido","Aomori","Iwate","Miyagi","Akita","Yamagata","Fukushima",
  "Ibaraki","Tochigi","Gunma","Saitama","Chiba","Tokyo","Kanagawa",
  "Niigata","Toyama","Ishikawa","Fukui","Yamanashi","Nagano",
"Gifu","Shizuoka","Aichi","Mie","Shiga","Kyoto","Osaka","Hyogo","Nara","Wakayama","Tottori","Shimane","Okayama","Hiroshima","Yamaguchi",
"Tokushima","Kagawa","Ehime","Kochi","Fukuoka","Saga","Nagasaki","Kumamoto","Oita","Miyazaki","Kagoshima","Okinawa"
)

# utils.R (shared helpers for INLA pipeline)
# Light English comments are included for maintainability.

# ---- Spatial adjacency (prefectures) ----
build_prefecture_graph <- function(){
  # Returns a named list: prefecture (JP) -> neighbouring prefectures (JP)
  list(
    "北海道"=c("青森県"),
    "青森県"=c("北海道","岩手県","秋田県"),
    "岩手県"=c("青森県","宮城県","秋田県"),
    "宮城県"=c("岩手県","秋田県","山形県","福島県"),
    "秋田県"=c("青森県","岩手県","宮城県","山形県"),
    "山形県"=c("宮城県","秋田県","福島県","新潟県"),
    "福島県"=c("宮城県","山形県","茨城県","栃木県","群馬県","新潟県"),
    "茨城県"=c("福島県","栃木県","埼玉県","千葉県"),
    "栃木県"=c("福島県","茨城県","群馬県","埼玉県"),
    "群馬県"=c("福島県","栃木県","埼玉県","新潟県","長野県"),
    "埼玉県"=c("茨城県","栃木県","群馬県","千葉県","東京都","長野県","山梨県"),
    "千葉県"=c("茨城県","埼玉県","東京都","神奈川県"),
    "東京都"=c("埼玉県","千葉県","神奈川県","山梨県"),
    "神奈川県"=c("千葉県","東京都","山梨県","静岡県"),
    "新潟県"=c("山形県","福島県","群馬県","富山県","長野県"),
    "富山県"=c("新潟県","石川県","長野県","岐阜県"),
    "石川県"=c("富山県","福井県","岐阜県"),
    "福井県"=c("石川県","岐阜県","滋賀県","京都府"),
    "山梨県"=c("埼玉県","東京都","神奈川県","長野県","静岡県"),
    "長野県"=c("群馬県","埼玉県","新潟県","富山県","山梨県","岐阜県","静岡県","愛知県"),
    "岐阜県"=c("富山県","石川県","福井県","長野県","愛知県","三重県","滋賀県"),
    "静岡県"=c("神奈川県","山梨県","長野県","愛知県"),
    "愛知県"=c("長野県","岐阜県","静岡県","三重県"),
    "三重県"=c("岐阜県","愛知県","滋賀県","京都府","奈良県","和歌山県"),
    "滋賀県"=c("福井県","岐阜県","三重県","京都府"),
    "京都府"=c("福井県","三重県","滋賀県","大阪府","兵庫県","奈良県"),
    "大阪府"=c("京都府","兵庫県","奈良県","和歌山県"),
    "兵庫県"=c("京都府","大阪府","鳥取県","岡山県","徳島県"),
    "奈良県"=c("三重県","京都府","大阪府","和歌山県"),
    "和歌山県"=c("三重県","大阪府","奈良県"),
    "鳥取県"=c("兵庫県","島根県","岡山県","広島県"),
    "島根県"=c("鳥取県","広島県","山口県"),
    "岡山県"=c("兵庫県","鳥取県","広島県","香川県"),
    "広島県"=c("鳥取県","島根県","岡山県","山口県","愛媛県"),
    "山口県"=c("島根県","広島県","福岡県"),
    "徳島県"=c("兵庫県","香川県","愛媛県","高知県"),
    "香川県"=c("岡山県","徳島県","愛媛県"),
    "愛媛県"=c("広島県","徳島県","香川県","高知県"),
    "高知県"=c("徳島県","愛媛県"),
    "福岡県"=c("山口県","佐賀県","熊本県","大分県"),
    "佐賀県"=c("福岡県","長崎県"),
    "長崎県"=c("佐賀県"),
    "熊本県"=c("福岡県","大分県","宮崎県","鹿児島県"),
    "大分県"=c("福岡県","熊本県","宮崎県"),
    "宮崎県"=c("熊本県","大分県","鹿児島県"),
    "鹿児島県"=c("熊本県","宮崎県"),
    "沖縄県"=character(0)
  )
}

build_nb_from_graph <- function(g, pref_list){
  # Build neighbour list from adjacency matrix (original behaviour).
  # pref_list: ordered prefecture names (JP). This order defines Q.
  if(!all(pref_list %in% names(g))){
    miss <- setdiff(pref_list, names(g))
    stop("Unknown prefecture names in graph: ", paste(miss, collapse=", "))
  }
  prefectures_list_ordered <- pref_list
  n <- length(prefectures_list_ordered)

  adj_matrix <- matrix(
    0, nrow=n, ncol=n,
    dimnames=list(prefectures_list_ordered, prefectures_list_ordered)
  )

  for(i in seq_len(n)){
    neighs <- g[[ prefectures_list_ordered[i] ]]
    neighs <- intersect(neighs, prefectures_list_ordered)  # safety
    if(length(neighs)>0) adj_matrix[i, neighs] <- 1
  }

  nb_list <- apply(adj_matrix, 1, function(x) which(x==1))
  class(nb_list) <- "nb"
  attr(nb_list, "region.id") <- prefectures_list_ordered

  # Original: add self-loop for Okinawa if zero-neighbour
  oki <- which(prefectures_list_ordered=="沖縄県")
  if(length(oki)==1 && length(nb_list[[oki]])==0) nb_list[[oki]] <- oki

  nb_list
}

make_icar_precision <- function(nb){
  # Original behaviour: W via spdep::nb2mat(style="B", zero.policy=TRUE)
  W <- spdep::nb2mat(nb, style="B", zero.policy=TRUE)
  W <- Matrix::Matrix(W, sparse=TRUE)
  D <- Matrix::Diagonal(x=Matrix::rowSums(W))
  D - W
}

# ---- Temperature splines ----
add_temp_splines <- function(df, pref_list, pref_col="pref", tmean_col="tmean", probs=c(0.25,0.75), degree=2){
  stopifnot(all(c(pref_col, tmean_col) %in% names(df)))
  if(any(!is.finite(probs)) || any(probs < 0 | probs > 1)) stop("`probs` must be within [0,1].")
  df_list <- split(df, df[[pref_col]])
  splines_list <- lapply(df_list, function(d){
    x <- as.numeric(d[[tmean_col]])
    knots <- as.numeric(stats::quantile(x, probs=probs, na.rm=TRUE, names=FALSE))
    knots <- unique(knots[is.finite(knots)])
    if(length(knots)==0){
      splines2::bSpline(x, degree=degree, intercept=TRUE)
    } else {
      splines2::bSpline(x, knots=knots, degree=degree, intercept=TRUE)
    }
  })
  S <- do.call(rbind, splines_list)
  K <- ncol(S)
  colnames(S) <- paste0("spline_temp_", seq_len(K))
  df2 <- dplyr::bind_cols(df, as.data.frame(S))
  list(df=df2, K=K)
}

# ---- Prefecture map helpers ----
get_jpn_pref_sf <- function(pref_list=NULL, path="../data/"){
  # Original scripts used geodata::gadm (no local shapefiles required).
  # `path` is used only as a cache directory for the downloaded GADM data.
  if(!dir.exists(path)) dir.create(path, recursive=TRUE)
  jpn <- geodata::gadm(country="JPN", level=1, path=path) |> sf::st_as_sf()
  # Ensure region_id = 1..47 by Japanese prefecture order
  jpn_id <- jpn |>
    dplyr::mutate(jp_name=.data$NL_NAME_1,
                  region_id=as.integer(factor(.data$jp_name, levels=prefecture_names_jp))) |>
    dplyr::filter(!is.na(.data$region_id))
  list(jpn=jpn, jpn_id=jpn_id)
}

get_english_pref_lookup <- function(jpn=NULL, pref_list=NULL){
  # Vector-based lookup (robust to map source differences).
  # Returns region_id (1..47), Japanese prefecture name, and English name.
  tibble::tibble(
    region_id=seq_along(prefecture_names_jp),
    prefecture=prefecture_names_jp,
    prefecture_en=prefecture_names_en
  )
}

add_prefecture_names <- function(df, pref_lookup){
  df |>
    dplyr::left_join(pref_lookup, by="region_id")
}

# ---- Social covariates alignment ----
align_soc_by_region <- function(df, soc_vars){
  soc_df <- df |>
    dplyr::select(region_id, dplyr::all_of(soc_vars)) |>
    dplyr::distinct() |>
    dplyr::arrange(region_id)
  R <- max(df$region_id, na.rm=TRUE)
  full <- tibble::tibble(region_id=seq_len(R)) |>
    dplyr::left_join(soc_df, by="region_id") |>
    dplyr::arrange(region_id)
  if(any(is.na(full[, soc_vars, drop=FALSE]))){
    miss <- full$region_id[apply(is.na(full[, soc_vars, drop=FALSE]), 1, any)]
    stop("Missing social covariates for region_id: ", paste(miss, collapse=", "))
  }
  as.matrix(full[, soc_vars, drop=FALSE])
}

# ---- Posterior sampling helpers ----
extract_fixed_samples <- function(res, terms, nSamp=800L, seed=1){
  # Match original scripts: sample fixed effects from marginals when available;
  # otherwise Normal(mean, sd) from summary.fixed. Never calls inla.posterior.sample().
  set.seed(seed)
  draw_one <- function(term){
    marg <- res$marginals.fixed[[term]]
    if(is.null(marg) || length(marg)==0){
      mu <- res$summary.fixed[term, "mean"]
      sd <- res$summary.fixed[term, "sd"]
      stats::rnorm(nSamp, mu, sd)
    } else {
      INLA::inla.rmarginal(nSamp, marg)
    }
  }
  M <- sapply(terms, draw_one)
  M <- t(M)  # terms x nSamp
  rownames(M) <- terms
  M
}

compute_excess_by_prefecture <- function(res, df, pop_col, lags, soc_vars, nSamp=800L, seed=1L){
  # Returns:
  #   exc: (nrow(df) x nSamp) posterior excess deaths
  #   summary_by_region: cumulative excess + mortality rate + IRR + posterior prob(excess>0)
  stopifnot(pop_col %in% names(df))
  R <- max(df$region_id, na.rm=TRUE)
  n <- nrow(df)

  gamma_names <- as.vector(outer(lags, soc_vars, paste, sep=":"))
  terms <- c(lags, gamma_names)
  fx_samp <- extract_fixed_samples(res, terms=terms, nSamp=nSamp, seed=seed)
  # Linear predictor parts for lags and interactions
  beta_samp <- fx_samp[lags, , drop=FALSE]
  gamma_names <- as.vector(outer(lags, soc_vars, paste, sep=":"))
  gamma_samp <- fx_samp[gamma_names, , drop=FALSE]

  # Social covariates matrix aligned to region_id=1..R
  soc_mat <- align_soc_by_region(df, soc_vars)

  # Build slope per observation per sample: sum_lag beta_lag*exp_lag + interactions beta+gamma*soc
  # We compute log-RR for each obs as:
  #   eta = sum_lag exp_lag * (beta_lag + sum_soc gamma_(lag:soc)*soc_value)
  exp_mat <- as.matrix(df[, lags, drop=FALSE])
  rid <- df$region_id

  # For each lag, compute modifier per obs per sample
  slope <- matrix(0, nrow=n, ncol=nSamp)
  for(k in seq_along(lags)){
    lg <- lags[k]
    b <- beta_samp[lg, ]
    # interaction sum over soc
    g_row <- gamma_samp[paste0(lg, ":", soc_vars), , drop=FALSE]  # (soc x samp)
    # modifier for each region_id: b + g %*% soc
    mod_r <- matrix(0, nrow=R, ncol=nSamp)
    mod_r <- matrix(rep(b, each=R), nrow=R) + (soc_mat %*% g_row)
    slope <- slope + (exp_mat[, lg] * mod_r[rid, ])
  }

  # Baseline mean (counterfactual) via fitted values / linear predictor without wind effect:
  # Use predictor mean as baseline and apply RR from slope to get predicted with wind,
  # then excess = (mu_wind - mu_base). We approximate mu_base with fitted mean from INLA.
  mu_hat <- res$summary.fitted.values$mean
  rr <- exp(slope)
  mu_wind <- mu_hat * rr
  exc <- (mu_wind - mu_hat)  # expected excess deaths per row, per sample

  # Aggregate to region
df$row_id <- seq_len(n)
idx_by_region <- split(df$row_id, df$region_id)

# cum_exc should be R x nSamp
cum_exc <- t(sapply(idx_by_region, function(idx) colSums(exc[idx, , drop=FALSE])))

# Ensure rows correspond to region_id 1..R (important if split order is odd)
cum_exc <- cum_exc[as.character(seq_len(R)), , drop=FALSE]

cum_exc_summary <- t(apply(cum_exc, 1, function(x){
  c(mean=mean(x),
    lower=as.numeric(stats::quantile(x, 0.025, names=FALSE)),
    upper=as.numeric(stats::quantile(x, 0.975, names=FALSE)))
}))

# population per region (length R, aligned)
pop_r <- tapply(df[[pop_col]], df$region_id, mean)
pop_r <- pop_r[as.character(seq_len(R))]

mort_mat <- sweep(cum_exc, 1, pop_r, "/") * 100
mort_summary <- t(apply(mort_mat, 1, function(x){
  c(mean=mean(x),
    lower=as.numeric(stats::quantile(x, 0.025, names=FALSE)),
    upper=as.numeric(stats::quantile(x, 0.975, names=FALSE)))
}))

  # Overall IRR (per 1 m/s increase in wind) - use exp(beta0) style summary
  # Here we summarise exp(beta_lag) just for reference; main inference uses heat_df.
  irr_lag <- t(apply(exp(beta_samp), 1, function(x){
    c(mean=mean(x),
      lower=as.numeric(stats::quantile(x, 0.025, names=FALSE)),
      upper=as.numeric(stats::quantile(x, 0.975, names=FALSE)))
  }))

  prob_exc_pos <- rowMeans(cum_exc > 0)

  summary_by_region <- tibble::tibble(
    region_id=seq_len(R),
    mean_exc=cum_exc_summary[, "mean"],
    lower_exc=cum_exc_summary[, "lower"],
    upper_exc=cum_exc_summary[, "upper"],
    mean_mort=mort_summary[, "mean"]*1000,
    lower_mort=mort_summary[, "lower"]*1000,
    upper_mort=mort_summary[, "upper"]*1000,
    prob_exc_pos=prob_exc_pos
  )

  list(exc=exc, cum_exc=cum_exc, summary_by_region=summary_by_region,
       beta_samp=beta_samp, gamma_samp=gamma_samp)
}

# ---- Typhoon mapping and summaries ----
make_typhoon_map <- function(df, threshold=17.2){
  # Create typhoon label levels (year-name) with duplicate handling.
  ty_map0 <- df |>
    dplyr::filter(!is.na(.data$typhoon_name), !is.na(.data$pwind), .data$pwind >= threshold) |>
    dplyr::group_by(.data$typhoon_no, .data$typhoon_name) |>
    dplyr::summarise(
      year=min(as.integer(as.character(.data$year))),
      order_key=min(.data$week_id),
      .groups="drop"
    ) |>
    dplyr::mutate(ty_label=paste0(.data$year, "-", .data$typhoon_name))
  ty_map <- ty_map0 |>
    dplyr::arrange(.data$order_key, .data$typhoon_no) |>
    dplyr::group_by(.data$ty_label) |>
    dplyr::mutate(ty_label=ifelse(duplicated(.data$ty_label) | duplicated(.data$ty_label, fromLast=TRUE),
                                  paste0(.data$ty_label, "-", dplyr::row_number()),
                                  .data$ty_label)) |>
    dplyr::ungroup()
  ty_levels <- ty_map |>
    dplyr::arrange(.data$order_key, .data$ty_label) |>
    dplyr::pull(.data$ty_label) |>
    unique()
  list(ty_map=ty_map, ty_levels=ty_levels)
}

compute_typhoon_mean_excess <- function(df, exc, ty_map, threshold=17.2){
  # Mean excess deaths (using posterior mean per observation) by prefecture x typhoon.
  df2 <- df |> dplyr::mutate(row_id=dplyr::row_number(),
                             excess_mean=rowMeans(exc))
  out <- df2 |>
    dplyr::filter(!is.na(.data$typhoon_no), !is.na(.data$pwind), .data$pwind >= threshold) |>
    dplyr::group_by(.data$region_id, .data$typhoon_no) |>
    dplyr::summarise(mean=sum(.data$excess_mean), .groups="drop") |>
    dplyr::left_join(ty_map, by="typhoon_no")
  out
}

compute_typhoon_cri_excess <- function(df, exc, ty_map, threshold=17.2){
  # Mean + 95% CrI for excess deaths by prefecture x typhoon.
  df2 <- df |> dplyr::mutate(row_id=dplyr::row_number())
  calc_cri <- function(idx){
    if(length(idx)==0) return(tibble::tibble(mean=NA_real_, lower=NA_real_, upper=NA_real_))
    v <- colSums(exc[idx, , drop=FALSE])
    tibble::tibble(
      mean=mean(v),
      lower=as.numeric(stats::quantile(v, 0.025, names=FALSE)),
      upper=as.numeric(stats::quantile(v, 0.975, names=FALSE))
    )
  }
  out <- df2 |>
    dplyr::filter(!is.na(.data$typhoon_no), !is.na(.data$pwind), .data$pwind >= threshold) |>
    dplyr::group_by(.data$region_id, .data$typhoon_no) |>
    dplyr::summarise(idx=list(.data$row_id), .groups="drop") |>
    dplyr::mutate(stats=purrr::map(.data$idx, calc_cri)) |>
    tidyr::unnest(.data$stats) |>
    dplyr::left_join(ty_map, by="typhoon_no") |>
    dplyr::select(region_id,typhoon_no,typhoon_name,year,
                  ty_label,mean,lower,upper)
  out
}

compute_cyclone_total_excess <- function(df, exc, ty_map, threshold=17.2){
  # Total excess deaths by typhoon (summing across all observations affected by the typhoon).
  df2 <- df |> dplyr::mutate(row_id=dplyr::row_number())
  out <- df2 |>
    dplyr::filter(!is.na(.data$typhoon_no), !is.na(.data$pwind), .data$pwind >= threshold) |>
    dplyr::group_by(.data$typhoon_no) |>
    dplyr::summarise(idx=list(.data$row_id), .groups="drop") |>
    dplyr::mutate(stats=purrr::map(.data$idx, function(ix){
      v <- colSums(exc[ix, , drop=FALSE])
      tibble::tibble(
        mean=mean(v),
        lower=as.numeric(stats::quantile(v, 0.025, names=FALSE)),
        upper=as.numeric(stats::quantile(v, 0.975, names=FALSE))
      )
    })) |>
    tidyr::unnest(.data$stats) |>
    dplyr::left_join(ty_map |> dplyr::distinct(.data$typhoon_no, .data$typhoon_name, .data$year), by="typhoon_no") |>
    dplyr::select(typhoon_no, typhoon_name, year, mean, lower, upper)
  out
}

compute_prob_tc_pos <- function(df, exc, ty_map, pref_lookup, threshold=17.2){
  # Posterior probability and CrI for excess deaths > 0 by prefecture x typhoon.
  df2 <- df |> dplyr::mutate(row_id=dplyr::row_number())
  grp <- df2 |>
    dplyr::filter(!is.na(.data$typhoon_no), !is.na(.data$pwind), .data$pwind >= threshold) |>
    dplyr::group_by(.data$region_id, .data$typhoon_no) |>
    dplyr::summarise(idx=list(.data$row_id), .groups="drop")
  out <- grp |>
    dplyr::rowwise() |>
    dplyr::mutate(
      prob_exc_pos=mean(colSums(exc[idx, , drop=FALSE]) > 0),
      median=as.numeric(stats::quantile(colSums(exc[idx, , drop=FALSE]), 0.5, names=FALSE)),
      lower=as.numeric(stats::quantile(colSums(exc[idx, , drop=FALSE]), 0.025, names=FALSE)),
      upper=as.numeric(stats::quantile(colSums(exc[idx, , drop=FALSE]), 0.975, names=FALSE))
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(pref_lookup, by="region_id") |>
    dplyr::left_join(ty_map |> dplyr::distinct(.data$typhoon_no, .data$typhoon_name, .data$year, .data$ty_label), by="typhoon_no") |>
    dplyr::select(region_id, prefecture_en, typhoon_no, typhoon_name, year, ty_label, prob_exc_pos, median, lower, upper)
  out
}

# ---- Plot helpers ----
plot_choropleth <- function(sf_obj, df, var, title, transform=NULL, limits=NULL, breaks=scales::pretty_breaks(5), labels=scales::comma){
  d <- sf_obj |> dplyr::left_join(df |> dplyr::select(region_id, value=.data[[var]]), by="region_id")
  if(!is.null(transform)) d$value <- transform(d$value)
  ggplot2::ggplot(d) +
    ggplot2::geom_sf(ggplot2::aes(fill=.data$value), colour="white", size=0.2) +
    ggplot2::scale_fill_viridis_c(name="", option="C", limits=limits, breaks=breaks, labels=labels) +
    ggplot2::labs(title=title) +
    ggplot2::theme_minimal(base_size=14) +
    ggplot2::theme(legend.position=c(0.9, 0.3))
}

plot_soclag_heatmap <- function(heat_df, title="Mortality rate change (%) per 1 m/s windspeed increase\nmodified by social indicator and lag"){
  ggplot2::ggplot(heat_df, ggplot2::aes(x=.data$soc, y=.data$prefecture_en, fill=.data$irr)) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~lag, ncol=1) +
    ggplot2::scale_fill_viridis_c(name="", option="C", breaks=scales::pretty_breaks(5)) +
    ggplot2::labs(x=NULL, y=NULL, title=title) +
    ggplot2::theme_minimal(base_size=14) +
    ggplot2::theme(legend.position="right")
}

make_soclag_df <- function(res, soc, pref_lookup, lags, soc_vars){
  soc_long <- soc |> tidyr::pivot_longer(cols=-region_id, names_to="soc", values_to="soc_val")
  beta_df <- tibble::tibble(lag=lags, beta=res$summary.fixed[lags, "mean"])
  gamma_mat <- matrix(res$summary.fixed[outer(lags, soc_vars, paste, sep=":"), "mean"], nrow=length(lags), byrow=TRUE,
                      dimnames=list(lags, soc_vars))
  gamma_df <- tibble::as_tibble(gamma_mat, rownames="lag") |>
    tidyr::pivot_longer(cols=-lag, names_to="soc", values_to="gamma")
  heat_df <- tidyr::expand_grid(soc_long, beta_df) |>
    dplyr::left_join(gamma_df, by=c("lag","soc")) |>
    dplyr::mutate(
      irr=(exp(.data$beta + .data$gamma * .data$soc_val)-1)*100,
      prefecture_en=factor(pref_lookup$prefecture_en[.data$region_id], levels=rev(pref_lookup$prefecture_en)),
      soc=factor(.data$soc, levels=soc_vars,
        labels=c("prefecture income","potential flood risk","potential landslide risk","distance to\nmedical facilities")),
      lag=factor(.data$lag, levels=lags, labels=c("lag 0 week","lag 1 week","lag 2 weeks"))
    )
  heat_df
}

export_panel_figure <- function(p_exc, p_pct, p_irr, p_soclag, out_file){
  p_con <- ggpubr::ggarrange(p_exc, p_pct, p_irr, labels=c("(A)","(B)","(C)"), font.label=list(size=22), nrow=3)
  p_all <- ggpubr::ggarrange(p_con, p_soclag, ncol=2, labels=c("", "(D)"), font.label=list(size=22))
  ggplot2::ggsave(filename=out_file, plot=p_all, width=20, height=30)
  invisible(p_all)
}

plot_typhoon_heatmap <- function(typhoon_summary, pref_lookup, ty_levels, out_file, width=20, height=10){
  plot_df <- typhoon_summary |>
    dplyr::left_join(pref_lookup, by="region_id") |>
    dplyr::mutate(
      typhoon=factor(.data$ty_label, levels=ty_levels),
      prefecture=factor(.data$prefecture_en, levels=rev(unique(.data$prefecture_en)))
    ) |>
    tidyr::complete(.data$prefecture, .data$typhoon) |>
    dplyr::mutate(mean=tidyr::replace_na(.data$mean, 0)) |>
    dplyr::filter(!is.na(.data$typhoon))
  mx <- max(abs(plot_df$mean), na.rm=TRUE)
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x=.data$typhoon, y=.data$prefecture, fill=.data$mean)) +
    ggplot2::geom_tile(colour="white", size=0.15) +
    ggplot2::scale_fill_gradient2(name="Excess deaths after\ntyphoons",
                                  low="#6E8EBF", mid="#F3EAD3", high="#8B3A3A",
                                  midpoint=0, limits=c(-mx, mx), oob=scales::squish) +
    ggplot2::labs(x="Typhoon year and name", y="Prefecture") +
    ggplot2::theme_minimal(base_size=15) +
    ggplot2::theme(panel.grid=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_text(angle=90, hjust=1, vjust=1, size=12),
                   axis.text.y=ggplot2::element_text(size=12),
                   legend.position="bottom",
                   legend.key.width=grid::unit(2, "cm"))
  ggplot2::ggsave(filename=out_file, plot=p, width=width, height=height)
  p
}
