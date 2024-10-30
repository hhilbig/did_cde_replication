rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sandwich,
  lmtest,
  lfe,
  gt,
  stringr,
  RColorBrewer,
  mgcv,
  hdm,
  ggrepel
)

# Install and load DirectEffects from GitHub
if (!pacman::p_loaded(DirectEffects)) {
  pacman::p_load_gh("mattblackwell/DirectEffects@assembly-line")
}

# Function to add line breaks

add_linebreak_vector <- function(string, ...) {
  sapply(string, function(s) add_linebreak(s, ...))
}

# Get data ----------------------------------------------------------------

data <- read_rds("data/bk_clean.rds")

# Bman code

t0.covariate.names <- c(
  "miami_trans_law_t0", "miami_trans_law2_t0", "therm_trans_t0",
  "gender_norms_sexchange_t0", "gender_norms_moral_t0", "gender_norms_abnormal_t0",
  "ssm_t0", "therm_obama_t0", "therm_gay_t0", "vf_democrat", "ideology_t0",
  "religious_t0", "exposure_gay_t0", "exposure_trans_t0", "pid_t0", "sdo_scale",
  "gender_norm_daugher_t0", "gender_norm_looks_t0",
  "gender_norm_rights_t0", "therm_afams_t0", "vf_female", "vf_hispanic",
  "vf_black", "vf_age", "survey_language_es", "cluster_level_t0_scale_mean",
  "vf_racename"
)

# Variables
# M : vtherm_trans_t
# D : treat_ind
# Y : miami_trans_law_t*_avg
# X :

# Drop some variables

df <- data %>%
  dplyr::select(
    hh_id, treat_ind, matches("therm"),
    matches("gender"),
    matches("trans"),
    matches("miami_trans_law_t"),
    one_of(t0.covariate.names)
  ) %>%
  mutate(treated = treat_ind)

# Renaming

df <- df %>%
  dplyr::rename_all(list(~ str_replace(., "norms", "norm")))

# Differenced variables

df <- df %>%
  mutate(
    gender_nonconformity_t1_diff = gender_nonconformity_t1 - gender_nonconformity_t0,
    gender_nonconformity_t2_diff = gender_nonconformity_t2 - gender_nonconformity_t0,
    gender_nonconformity_t3_diff = gender_nonconformity_t3 - gender_nonconformity_t0,
    gender_nonconformity_t4_diff = gender_nonconformity_t4 - gender_nonconformity_t0,
    trans.tolerance.dv.t1_diff = trans.tolerance.dv.t1 - trans.tolerance.dv.t0,
    trans.tolerance.dv.t2_diff = trans.tolerance.dv.t2 - trans.tolerance.dv.t0,
    trans.tolerance.dv.t3_diff = trans.tolerance.dv.t3 - trans.tolerance.dv.t0,
    trans.tolerance.dv.t4_diff = trans.tolerance.dv.t4 - trans.tolerance.dv.t0,
    therm_trans_t1_diff = therm_trans_t1 - therm_trans_t0,
    therm_trans_t2_diff = therm_trans_t2 - therm_trans_t0,
    therm_trans_t3_diff = therm_trans_t3 - therm_trans_t0,
    therm_trans_t4_diff = therm_trans_t4 - therm_trans_t0,
    miami_trans_law_t1_avg_diff = miami_trans_law_t1_avg - miami_trans_law_t0_avg,
    miami_trans_law_t2_avg_diff = miami_trans_law_t2_avg - miami_trans_law_t0_avg,
    miami_trans_law_t3_avg_diff = miami_trans_law_t3_avg - miami_trans_law_t0_avg,
    miami_trans_law_t4_avg_diff = miami_trans_law_t4_avg - miami_trans_law_t0_avg,
    therm_obama_t1_diff = therm_obama_t1 - therm_obama_t0,
    therm_marijuana_t1_diff = therm_marijuana_t1 - therm_marijuana_t0,
    gender_norm_looks_t1_diff = gender_norm_looks_t1 - gender_norm_looks_t0,
    gender_norm_looks_t2_diff = gender_norm_looks_t2 - gender_norm_looks_t0,
    gender_norm_looks_t3_diff = gender_norm_looks_t3 - gender_norm_looks_t0,
    gender_norm_looks_t4_diff = gender_norm_looks_t4 - gender_norm_looks_t0,
    gender_norm_rights_t1_diff = gender_norm_rights_t1 - gender_norm_rights_t0,
    gender_norm_rights_t2_diff = gender_norm_rights_t2 - gender_norm_rights_t0,
    gender_norm_rights_t3_diff = gender_norm_rights_t3 - gender_norm_rights_t0,
    gender_norm_rights_t4_diff = gender_norm_rights_t4 - gender_norm_rights_t0,
    gender_norm_moral_t1_diff = gender_norm_moral_t1 - gender_norm_moral_t0,
    gender_norm_moral_t2_diff = gender_norm_moral_t2 - gender_norm_moral_t0,
    gender_norm_moral_t3_diff = gender_norm_moral_t3 - gender_norm_moral_t0,
    gender_norm_moral_t4_diff = gender_norm_moral_t4 - gender_norm_moral_t0,
    gender_norm_sexchange_t1_diff = gender_norm_sexchange_t1 - gender_norm_sexchange_t0,
    gender_norm_sexchange_t2_diff = gender_norm_sexchange_t2 - gender_norm_sexchange_t0,
    gender_norm_sexchange_t3_diff = gender_norm_sexchange_t3 - gender_norm_sexchange_t0,
    gender_norm_sexchange_t4_diff = gender_norm_sexchange_t4 - gender_norm_sexchange_t0,
    gender_norm_abnormal_t1_diff = gender_norm_abnormal_t1 - gender_norm_abnormal_t0,
    gender_norm_abnormal_t2_diff = gender_norm_abnormal_t2 - gender_norm_abnormal_t0,
    gender_norm_abnormal_t3_diff = gender_norm_abnormal_t3 - gender_norm_abnormal_t0,
    gender_norm_abnormal_t4_diff = gender_norm_abnormal_t4 - gender_norm_abnormal_t0
  )

# List of covariates --------------------------------------------------------

X_list <- c(
  "gender_norm_sexchange_t0", "gender_norm_moral_t0",
  "gender_norm_abnormal_t0", "ssm_t0", "therm_obama_t0",
  "therm_gay_t0", "vf_democrat", "ideology_t0",
  "religious_t0", "exposure_gay_t0", "exposure_trans_t0",
  "pid_t0", "sdo_scale", "gender_norm_daugher_t0",
  "gender_norm_looks_t0", "gender_norm_rights_t0",
  "therm_afams_t0", "vf_female", "vf_hispanic", "vf_black",
  "vf_age", "survey_language_es",
  "cluster_level_t0_scale_mean"
)


# Covariate labels

X_labs <- c(
  `gender_norm_sexchange_t0` = "Gender norms (sex change)",
  `gender_norm_moral_t0` = "Gender norms (morality)",
  `gender_norm_abnormal_t0` = "Gender norms (abnormal)",
  `ssm_t0` = "Same-sex marriage",
  `therm_obama_t0` = "Obama therm.",
  `therm_gay_t0` = "Gay men therm.",
  `vf_democrat` = "Democrat",
  `ideology_t0` = "Ideology",
  `religious_t0` = "Religiousity",
  `exposure_gay_t0` = "Knows gay people",
  `exposure_trans_t0` = "Knows trans\npeople",
  `pid_t0` = "Partisanship",
  `sdo_scale` = "Social dominance\norientation",
  `gender_norm_daugher_t0` = "Gender norms (daughter)",
  `gender_norm_looks_t0` = "",
  `gender_norm_rights_t0` = "",
  `therm_afams_t0` = "AfAm\ntherm.",
  `vf_female` = "Female",
  `vf_hispanic` = "Hispanic",
  `vf_black` = "Black",
  `vf_age` = "Age",
  `survey_language_es` = "Spanish Survey",
  `cluster_level_t0_scale_mean` = "Blocking scale"
)

# Mediator

M_list <- c(
  "therm_trans_t1", "therm_trans_t2",
  "therm_trans_t3", "therm_trans_t4"
)
M_list_proper <- c(
  "Trans feeling therm. (t_1)",
  "Trans feeling therm. (t_2)",
  "Trans feeling therm. (t_3)",
  "Trans feeling therm. (t_4)"
)

# Outcomes
Y_list <- c(
  "miami_trans_law_t1_avg_diff", "miami_trans_law_t2_avg_diff",
  "miami_trans_law_t3_avg_diff", "miami_trans_law_t4_avg_diff"
)
Y_list_proper <- c(
  "Trans law support (t_1 - t_0)",
  "Trans law support (t_2 - t_0)",
  "Trans law support (t_3 - t_0)",
  "Trans law support (t_4 - t_0)"
)

# Covariates

X_in <- list(
  c(X_list),
  X_list,
  c(
    "vf_democrat", "ideology_t0",
    "religious_t0", "exposure_trans_t0",
    "vf_female", "vf_hispanic",
    "vf_black", "vf_age", "survey_language_es",
    "trans.tolerance.dv.t0", "gender_nonconformity_t0",
    "therm_obama_t0"
  ),
  c(
    "vf_female", "vf_hispanic",
    "vf_black", "vf_age"
  )
)


# Covariate labels
X_in_labs <- c(
  "1: All covars + sq terms + ints",
  "2: All covars",
  "3: Indices + Demographics",
  "4: Only Demographics in X"
)

# Mediator

M_list <- c("therm_trans_t2", "therm_trans_t3")
M_list_proper <- c(
  "Trans feeling therm. (t_2)",
  "Trans feeling therm. (t_3)"
)

# Outcomes

Y_list <- c("miami_trans_law_t3_avg_diff", "miami_trans_law_t4_avg_diff")
Y_list_proper <- c(
  "Trans law support (t_3 - t_0)",
  "Trans law support (t_4 - t_0)"
)

# Get saved results ---------------------------------------------------------

out <- read_rds("results/main_output.rds")

## Save this for plotting

out_df_all <- out %>%
  left_join(data.frame(
    mediator = M_list,
    mediator_lab = M_list_proper
  )) %>%
  left_join(data.frame(
    outcome = Y_list,
    outcome_lab = Y_list_proper
  )) %>%
  mutate(
    p_med = as.numeric(str_extract(mediator, "[0-9]")),
    p_y = as.numeric(str_extract(outcome, "[0-9]"))
  ) %>%
  filter(!p_med > p_y) %>%
  mutate(
    outcome_lab = add_linebreak_vector(outcome_lab, min_length = 8),
    mediator_lab = add_linebreak_vector(mediator_lab, min_length = 8)
  ) %>%
  filter(p_y == 3 & p_med == 2) %>%
  filter(method != "DiD w/ X, Z; no mediator") %>%
  mutate(n_folds = "5") %>%
  replace_na(list(flexible = "No")) %>%
  mutate(
    method = str_replace(method, "DiD", "DID"),
    method = factor(method, levels = c(
      "DID w/ X, no mediator",
      "DID w/ X, Z + mediator",
      "ACDE-BC",
      "ACDE-PC"
    )),
    base_cond = factor(base_cond, levels = c(
      "Marginal",
      "m = Cool",
      "m = Neutral",
      "m = Warm"
    ))
  )

# Transform mediator -------------------------------------------------------

make_3cats <- function(v) {
  case_when(
    v < 50 ~ 0,
    v == 50 ~ 1,
    v > 50 ~ 2
  )
}

# Transform mediator -------------------------------------------------------

df <- df %>%
  mutate(mediator_original = therm_trans_t2) %>%
  mutate_at(vars(one_of(M_list)), make_3cats)
df <- df %>%
  mutate(base_med = make_3cats(therm_trans_t0))

# Prep for sensitivity analysis ----------------------------------------------

cov_table <- tibble()

for (k in seq_along(X_list)) {
  this_cov <- X_list[k]
  this_lab <- X_labs[this_cov]
  y_x_form <- reformulate(X_list, response = "miami_trans_law_t3_avg_diff")
  y_v_form <- reformulate(setdiff(X_list, this_cov), response = "miami_trans_law_t3_avg_diff")


  obs_mod <- lm(y_v_form, data = df, subset = base_med == 1 & therm_trans_t2 == 1 & treat_ind == 0)
  x_mod <- lm(y_x_form, data = df, subset = base_med == 1 & therm_trans_t2 == 1 & treat_ind == 0)
  v_2_mod <- lm(update(y_v_form, predict(x_mod, newdata = df) ~ .), data = df, subset = base_med == 1 & treat_ind == 0)

  o_preds <- predict(obs_mod, newdata = df |> filter(base_med == 1 & treat_ind == 0))
  v_preds <- predict(v_2_mod, newdata = df |> filter(base_med == 1 & treat_ind == 0))
  m_diff <- mean(abs(o_preds - v_preds))
  q_diff <- quantile(abs(o_preds - v_preds), probs = 0.95)

  this_df <- tibble(
    covariate = this_cov,
    label = this_lab,
    mean_abs_diff = m_diff,
    q95_abs_diff = q_diff
  )
  cov_table <- bind_rows(cov_table, this_df)
}

# CIs

imb.man.ci <- function(lo, hi, lo.se, hi.se, N, alpha = 0.95) {
  delta <- (hi - lo) / (max(lo.se, hi.se))
  Cn <- rep(NA, length(lo))
  Cs <- seq(0, 10, by = 0.001)
  for (k in seq_along(lo)) {
    Cseq <- abs(pnorm(Cs + delta[k]) - pnorm(-Cs) - alpha)
    Cn[k] <- Cs[Cseq == min(Cseq)]
  }

  ci.lo <- lo - Cn * lo.se
  ci.hi <- hi + Cn * hi.se
  return(cbind(ci.lo, ci.hi))
}

# Sensitivity analysis ------------------------------------------------------

sa_data <- out_df_all |>
  filter(
    method == "ACDE-PC",
    !str_detect(mediator, "tolerance"),
    cov_label == X_in_labs[2],
    flexible == "Yes",
    base_cond == "m = Neutral"
  ) |>
  cross_join(tibble(Gamma = seq(0, 1, by = 0.01))) |>
  mutate(
    lb = estimate - 2 * Gamma,
    ub = estimate + 2 * Gamma,
    lb_ci = estimate - 2 * Gamma - 1.64 * std.error,
    ub_ci = estimate + 2 * Gamma + 1.64 * std.error,
    lb_ci_im = imb.man.ci(lb, ub, std.error, std.error, n, alpha = 0.9)[, 1],
    ub_ci_im = imb.man.ci(lb, ub, std.error, std.error, n, alpha = 0.9)[, 2]
  )

# Figure SM.1: Sensitivity analysis -------------------------------------------

sa_data |>
  ggplot(aes(x = Gamma, y = lb)) +
  geom_ribbon(aes(ymin = lb_ci_im, ymax = ub_ci_im), fill = "grey") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  geom_line() +
  geom_line(aes(y = ub)) +
  theme_light(base_family = "Fira Sans") +
  geom_point(data = cov_table, aes(x = q95_abs_diff), y = 0, shape = 4, size = 2) +
  geom_label_repel(
    data = cov_table |> filter(q95_abs_diff > 0.17),
    aes(x = q95_abs_diff, y = 0, label = label), size = 2.5,
    nudge_y = -1.25, seed = 1234
  ) +
  labs(x = expression(Gamma), y = "ACDE-PC (m = Neutral)")

# Additional quantities -----------------------------------------------------

cov_table |>
  filter(q95_abs_diff > sa_data$Gamma[max(which(sa_data$lb_ci_im > 0))])

cov_table |>
  filter(q95_abs_diff <= sa_data$Gamma[max(which(sa_data$lb_ci_im > 0))])

cov_table |>
  filter(q95_abs_diff > sa_data$Gamma[max(which(sa_data$lb > 0))])

df |>
  filter(base_med == 1, therm_trans_t3 == 1) |>
  summarize(sd(miami_trans_law_t3_avg_diff))
