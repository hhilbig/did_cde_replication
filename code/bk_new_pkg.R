rm(list = ls())
do_save <- TRUE

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
  hdm
)

# Install and load DirectEffects from GitHub
if (!pacman::p_loaded(DirectEffects)) {
  pacman::p_load_gh("mattblackwell/DirectEffects@assembly-line")
}

# Function to add line breaks

add_linebreak_vector <- function(string, ...) {
  sapply(string, function(s) add_linebreak(s, ...))
}

add_linebreak <- function(string, min_length = 10, add_multiple_linebreaks = F) {
  if (nchar(string) > min_length) {
    if (!add_multiple_linebreaks) {
      l <- nchar(string)
      find_space <- str_locate_all(string, " |\\-") %>%
        .[[1]] %>%
        data.frame() %>%
        pull(start) %>%
        .[which.min(abs(. -
          (nchar(string) / 2)))]
      substr(string, find_space, find_space) <- "\n"
      string
    } else {
      find_space <- str_locate_all(string, " |\\-") %>%
        .[[1]] %>%
        data.frame() %>%
        slice(-1) %>%
        pull(1)
      for (i in find_space) {
        substr(string, i, i) <- "\n"
      }
      string
    }
  } else {
    string
  }
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
  "vf_black", "vf_age", "survey_language_es", "cluster_level_t0_scale_mean"
)

tX.indices <- colnames(data) %>%
  str_subset("nonconform|trans.tolerance.dv")

# Variables
# M : vtherm_trans_t
# D : treat_ind
# Y : miami_trans_law_t*_avg
# X :

# Drop some stuff

df <- data %>%
  dplyr::select(
    hh_id, treat_ind, matches("therm"),
    matches("gender_norm"),
    matches("trans"),
    matches("miami_trans_law_t"),
    one_of(t0.covariate.names),
    one_of(tX.indices)
  ) %>%
  mutate(treated = treat_ind)

# Renaming

df <- df %>%
  dplyr::rename_all(list(~ str_replace(., "norms", "norm")))

colnames(df)

# Need to change everything to be changes

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

#

# Start by estimating propensity score models

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
X_cont <- which(lapply(df[, X_list], function(x) length(unique(x))) > 5)
X_sq_terms <- paste0("I(", X_list[X_cont], " ^ 2)")
X_int_terms <- outer(X_list, X_list, paste, sep = " * ")
X_int_terms <- X_int_terms[upper.tri(X_int_terms)]
X_list_proper <- t0.covariate.names

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

# Function to tranform variable into categorical

make_3cats <- function(v) {
  case_when(
    v < 50 ~ 0,
    v == 50 ~ 1,
    v > 50 ~ 2
  )
}

# Make mediator binary

df <- df %>%
  mutate(mediator_original = therm_trans_t2) %>%
  mutate_at(vars(one_of(M_list)), make_3cats)

# Define Y_list

Y_list <- c(
  "miami_trans_law_t1_avg_diff", "miami_trans_law_t2_avg_diff",
  "miami_trans_law_t3_avg_diff", "miami_trans_law_t4_avg_diff"
)

# Define Y_list_proper (proper labels)

Y_list_proper <- c(
  "Trans law support (t_1 - t_0)",
  "Trans law support (t_2 - t_0)",
  "Trans law support (t_3 - t_0)",
  "Trans law support (t_4 - t_0)"
)

# Z_list
# Just keep this as it is

Z_list <- c(
  "gender_norm_sexchange_t1_diff", "gender_norm_moral_t1_diff",
  "gender_norm_abnormal_t1_diff", "therm_obama_t1_diff",
  "trans.tolerance.dv.t1_diff",
  "gender_nonconformity_t1_diff",
  "miami_trans_law_t1_avg_diff"
)
Z_list <- c(
  "gender_norm_sexchange_t1", "gender_norm_moral_t1",
  "gender_norm_abnormal_t1", "therm_obama_t1",
  "trans.tolerance.dv.t1",
  "gender_nonconformity_t1",
  "miami_trans_law_t1_avg_diff", "therm_marijuana_t1"
)

Z_list %in% colnames(df)

Z_sq_terms <- paste0("I(", Z_list, " ^ 2)")
Z_int_terms <- outer(Z_list, Z_list, paste, sep = " * ")
Z_int_terms <- Z_int_terms[upper.tri(Z_int_terms)]
XZ_int_terms <- c(outer(X_list, Z_list, paste, sep = " * "))

# Declare base mediator

df <- df %>%
  mutate(base_med = make_3cats(therm_trans_t0))

# Rename

est_df <- df

# Drop missings

sum(complete.cases(df[, c(
  "therm_trans_t2",
  "treated",
  "miami_trans_law_t3_avg_diff",
  "base_med",
  "gender_nonconformity_t0",
  "trans.tolerance.dv.t0"
)]))

# Drop missings

est_df <- est_df[complete.cases(df[, c(
  "therm_trans_t2",
  "treated",
  "miami_trans_law_t3_avg_diff",
  "base_med",
  "gender_nonconformity_t0",
  "trans.tolerance.dv.t0"
)]), ]

# Figure 1: Histogram of mediator, untransformed ----------------------------

figure_1 <- est_df %>%
  ggplot(aes(x = therm_trans_t0)) +
  geom_histogram(binwidth = 10, fill = "white", color = "black") +
  xlab("Feeling thermometer (mediator) at baseline, untransformed") +
  ylab("Frequency") +
  theme_light(base_family = "Fira Sans")
figure_1


if (do_save) {
  ggsave("output/hist_mediator_raw.pdf",
    width = 6, height = 3, device = cairo_pdf
  )
}

# Table 2 -----------------------------------------------------------------

table_2 <- est_df |>
  filter(!is.na(therm_trans_t2)) |>
  mutate(
    therm_trans_t2 = recode(therm_trans_t2,
      `0` = "Cool",
      `1` = "Neutral",
      `2` = "Warm"
    ),
    base_med = recode(base_med,
      `0` = "Cool",
      `1` = "Neutral",
      `2` = "Warm"
    )
  ) |>
  count(base_med, therm_trans_t2) |>
  group_by(base_med) |>
  mutate(
    perc = n / sum(n)
  ) |>
  pivot_wider(
    names_from = therm_trans_t2,
    values_from = c(n, perc)
  ) |>
  ungroup() |>
  rename(`Baseline` = "base_med") |>
  gt(id = "m_joint_dist") |>
  fmt_percent(starts_with("perc")) |>
  cols_merge_n_pct(
    col_n = n_Cool,
    col_pct = perc_Cool
  ) |>
  cols_merge_n_pct(
    col_n = n_Neutral,
    col_pct = perc_Neutral
  ) |>
  cols_merge_n_pct(
    col_n = n_Warm,
    col_pct = perc_Warm
  ) |>
  tab_spanner(label = "Wave 2", c(2:4)) |>
  cols_label_with(
    fn = ~ gsub("n_", "", .)
  )

# Optional save

do_save <- TRUE

if (do_save) {
  table_2 |>
    tab_options(latex.use_longtable = TRUE) |>
    gtsave("output/therm_joint_dist.tex")
}

# Function to get estimates ------------------------------------------------

get_estimates <- function(est_df,
                          X_list_use = X_list,
                          Z_list_use = Z_list,
                          n_folds = 5) {
  # Iterate over mediators and outcomes

  out_list <- lapply(M_list, function(m) {
    lapply(Y_list, function(y) {
      # Print outcome and mediator

      cat("Outcome: ", y, "\tMediator: ", m, "\n")

      # Formula

      big_form <- as.formula(paste0(
        y, " ~ treated + base_med + ", m, "+",
        paste0(X_list_use,
          collapse = "+"
        ), "+",
        paste0(Z_list_use, collapse = "+")
      ))

      # allows us to use squared terms/interactions
      est_df <- model.frame(big_form, data = df)

      # Rename outcome to "y"
      # Remove missings

      est_df[, "y"] <- est_df[, y]
      est_df_temp <- est_df %>%
        filter(!is.na(y))

      # New Doubly Robust estimator

      y_xz_mod <- as.formula(paste0(
        "~ treated + base_med + ", m, "+",
        paste0(X_list_use,
          collapse = "+"
        ), "+",
        paste0(Z_list_use, collapse = "+")
      ))
      y_x_mod <- as.formula(paste0(
        "~ treated + base_med +",
        paste0(X_list_use,
          collapse = "+"
        )
      ))

      # Pscore formula

      f_m_mod <- as.formula(paste0(
        m, "~ treated + base_med + ",
        paste0(X_list_use %>% setdiff("therm_trans_t0"),
          collapse = "+"
        ), "+",
        paste0(Z_list_use, collapse = "+")
      ))

      # Estimate

      out_dr_ml <- cde_did_aipw(base_mediator = base_med, trim = c(0.01, 0.99)) |>
        set_treatment(treated) |>
        treat_model(engine = "logit", formula = treated ~ base_med) |>
        outreg_model(engine = "rlasso", y_x_mod, separate = FALSE) |>
        set_treatment(!!m) |>
        treat_model(engine = "ranger_class", f_m_mod, separate = FALSE, include_past = FALSE) |>
        outreg_model(engine = "rlasso", y_xz_mod, separate = FALSE, include_past = FALSE) |>
        estimate(y ~ treated, data = est_df_temp, n_folds = n_folds, n_splits = 20)

      f_m_gamma_mod <- as.formula(paste0(
        m, "~ treated + base_med + ",
        paste0(X_list_use %>% setdiff("therm_trans_t0"),
          collapse = "+"
        )
      ))
      y_x_gamma_mod <- as.formula(paste0(
        "~ treated + base_med +", m, "+",
        paste0(X_list_use,
          collapse = "+"
        )
      ))


      out_dr_gamma_ml <- cde_did_aipw(base_mediator = base_med, trim = c(0.01, 0.99), on_treated = TRUE) |>
        set_treatment(treated) |>
        treat_model(engine = "logit", formula = treated ~ base_med) |>
        outreg_model(engine = "rlasso", y_x_mod, separate = FALSE) |>
        set_treatment(!!m) |>
        treat_model(engine = "ranger_class", f_m_gamma_mod, separate = FALSE, include_past = FALSE) |>
        outreg_model(engine = "rlasso", y_x_gamma_mod, separate = FALSE, include_past = FALSE) |>
        estimate(y ~ treated, data = est_df_temp, n_folds = n_folds, n_splits = 20)

      # Get estimates for tau


      est_dr_marginal_ml <- out_dr_ml$estimates["treated_1_*", "estimate"]
      se_dr_marginal_ml <- out_dr_ml$estimates["treated_1_*", "std.error"]

      est_dr_m0_ml <- out_dr_ml$estimates["treated_1_0", "estimate"]
      se_dr_m0_ml <- out_dr_ml$estimates["treated_1_0", "std.error"]

      est_dr_m1_ml <- out_dr_ml$estimates["treated_1_1", "estimate"]
      se_dr_m1_ml <- out_dr_ml$estimates["treated_1_1", "std.error"]

      est_dr_m2_ml <- out_dr_ml$estimates["treated_1_2", "estimate"]
      se_dr_m2_ml <- out_dr_ml$estimates["treated_1_2", "std.error"]


      # Gamma

      est_dr_marginal_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_*", "estimate"]
      se_dr_marginal_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_*", "std.error"]

      est_dr_m0_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_0", "estimate"]
      se_dr_m0_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_0", "std.error"]

      est_dr_m1_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_1", "estimate"]
      se_dr_m1_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_1", "std.error"]

      est_dr_m2_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_2", "estimate"]
      se_dr_m2_gamma_ml <- out_dr_gamma_ml$estimates["treated_1_2", "std.error"]


      df_dr_ml <- data.frame(
        estimate = c(
          est_dr_marginal_ml, est_dr_m0_ml, est_dr_m1_ml,
          est_dr_m2_ml
        ),
        std.error = c(
          se_dr_marginal_ml, se_dr_m0_ml, se_dr_m1_ml,
          se_dr_m2_ml
        ),
        mediator = m,
        covars = "Yes", intermed_covars = "Yes",
        flexible = "Yes",
        method = "ACDE-BC",
        base_cond = c(
          "Marginal",
          "m = Cool",
          "m = Neutral",
          "m = Warm"
        ),
        n = nrow(est_df),
        stringsAsFactors = F
      )

      df_dr_gamma_ml <- data.frame(
        estimate = c(
          est_dr_marginal_gamma_ml, est_dr_m0_gamma_ml, est_dr_m1_gamma_ml,
          est_dr_m2_gamma_ml
        ),
        std.error = c(
          se_dr_marginal_gamma_ml, se_dr_m0_gamma_ml, se_dr_m1_gamma_ml,
          se_dr_m2_gamma_ml
        ),
        mediator = m,
        covars = "Yes", intermed_covars = "Yes",
        flexible = "Yes",
        method = "ACDE-PC",
        base_cond = c(
          "Marginal",
          "m = Cool",
          "m = Neutral",
          "m = Warm"
        ),
        n = nrow(est_df),
        stringsAsFactors = F
      )

      # Add to table

      if (length(X_list_use) < 100) {
        # Effect #

        m2 <- felm(
          as.formula(paste0(
            "y ~ treated + base_med+",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp
        )
        m3 <- felm(
          as.formula(paste0(
            "y ~ treated + base_med+",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp
        )
        m4 <- felm(
          as.formula(paste0(
            "y ~ treated + base_med +", m, " + ",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df
        )


        m2_0 <- felm(
          as.formula(paste0(
            "y ~ treated+",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 0
        )
        m2_1 <- felm(
          as.formula(paste0(
            "y ~ treated+",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 1
        )
        m2_2 <- felm(
          as.formula(paste0(
            "y ~ treated+",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 2
        )

        m3_0 <- felm(
          as.formula(paste0(
            "y ~ treated +",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 0
        )
        m3_1 <- felm(
          as.formula(paste0(
            "y ~ treated +",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 1
        )
        m3_2 <- felm(
          as.formula(paste0(
            "y ~ treated +",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 1
        )
        m4_0 <- felm(
          as.formula(paste0(
            "y ~ treated +", m, " + ",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 0
        )
        m4_1 <- felm(
          as.formula(paste0(
            "y ~ treated +", m, " + ",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 1
        )
        m4_2 <- felm(
          as.formula(paste0(
            "y ~ treated +", m, " + ",
            paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            ),
            "+",
            paste0(c(Z_list_use %>% setdiff("therm_trans_t0")),
              collapse = "+"
            )
          )),
          data = est_df_temp,
          subset = base_med == 2
        )

        #

        mlist <- list(m2, m2_0, m2_1, m2_2)
        out_table_2 <- mlist %>%
          lapply(broom::tidy, conf.int = T) %>%
          reduce(rbind) %>%
          filter(str_detect(term, "treated")) %>%
          mutate(
            mediator = m,
            covars = "Yes",
            intermed_covars = "No",
            outcome = y,
            flexible = "No",
            method = "DiD w/ X, no mediator",
            base_cond = c(
              "Marginal",
              "m = Cool",
              "m = Neutral",
              "m = Warm"
            )
          ) %>%
          dplyr::select(
            estimate, std.error, mediator, covars,
            intermed_covars, method, flexible, base_cond
          ) %>%
          mutate(n = sapply(mlist, function(x) sum(!is.na(x$response))))

        mlist <- list(m3, m3_0, m3_1, m3_2)
        out_table_3 <- mlist %>%
          lapply(broom::tidy, conf.int = T) %>%
          reduce(rbind) %>%
          filter(str_detect(term, "treated")) %>%
          mutate(
            mediator = m,
            covars = "Yes",
            intermed_covars = "Yes",
            outcome = y,
            flexible = "No",
            method = "DiD w/ X, Z; no mediator",
            base_cond = c(
              "Marginal",
              "m = Cool",
              "m = Neutral",
              "m = Warm"
            )
          ) %>%
          dplyr::select(
            estimate, std.error, mediator, covars,
            intermed_covars, method, flexible, base_cond
          ) %>%
          mutate(n = sapply(mlist, function(x) sum(!is.na(x$response))))

        mlist <- list(m4, m4_0, m4_1, m4_2)
        out_table_4 <- mlist %>%
          lapply(broom::tidy, conf.int = T) %>%
          reduce(rbind) %>%
          filter(str_detect(term, "treated")) %>%
          mutate(
            mediator = m,
            covars = "Yes",
            intermed_covars = "Yes",
            outcome = y,
            flexible = "No",
            method = "DiD w/ X, Z + mediator",
            base_cond = c(
              "Marginal",
              "m = Cool",
              "m = Neutral",
              "m = Warm"
            )
          ) %>%
          dplyr::select(
            estimate, std.error, mediator, covars,
            intermed_covars, method, flexible, base_cond
          ) %>%
          mutate(n = sapply(mlist, function(x) sum(!is.na(x$response))))

        out_dr <- cde_did_aipw(base_mediator = base_med, trim = c(0.01, 0.99)) |>
          set_treatment(treated) |>
          treat_model(engine = "logit", formula = treated ~ base_med) |>
          outreg_model(engine = "lm", y_x_mod, separate = FALSE) |>
          set_treatment(!!m) |>
          treat_model(engine = "multinom", f_m_mod, separate = FALSE, include_past = FALSE) |>
          outreg_model(engine = "lm", y_xz_mod, separate = FALSE, include_past = FALSE) |>
          estimate(y ~ treated, data = est_df_temp, n_folds = n_folds, n_splits = 20)

        out_dr_gamma <- cde_did_aipw(base_mediator = base_med, trim = c(0.01, 0.99), on_treated = TRUE) |>
          set_treatment(treated) |>
          treat_model(engine = "logit", formula = treated ~ base_med) |>
          outreg_model(engine = "lm", y_x_mod, separate = FALSE) |>
          set_treatment(!!m) |>
          treat_model(engine = "multinom", f_m_gamma_mod, separate = FALSE, include_past = FALSE) |>
          outreg_model(engine = "lm", y_x_gamma_mod, separate = FALSE, include_past = FALSE) |>
          estimate(y ~ treated, data = est_df_temp, n_folds = n_folds, n_splits = 20)

        est_dr_marginal <- out_dr$estimates["treated_1_*", "estimate"]
        se_dr_marginal <- out_dr$estimates["treated_1_*", "std.error"]

        est_dr_m0 <- out_dr$estimates["treated_1_0", "estimate"]
        se_dr_m0 <- out_dr$estimates["treated_1_0", "std.error"]

        est_dr_m1 <- out_dr$estimates["treated_1_1", "estimate"]
        se_dr_m1 <- out_dr$estimates["treated_1_1", "std.error"]

        est_dr_m2 <- out_dr$estimates["treated_1_2", "estimate"]
        se_dr_m2 <- out_dr$estimates["treated_1_2", "std.error"]

        est_dr_marginal_gamma <- out_dr_gamma$estimates["treated_1_*", "estimate"]
        se_dr_marginal_gamma <- out_dr_gamma$estimates["treated_1_*", "std.error"]

        est_dr_m0_gamma <- out_dr_gamma$estimates["treated_1_0", "estimate"]
        se_dr_m0_gamma <- out_dr_gamma$estimates["treated_1_0", "std.error"]

        est_dr_m1_gamma <- out_dr_gamma$estimates["treated_1_1", "estimate"]
        se_dr_m1_gamma <- out_dr_gamma$estimates["treated_1_1", "std.error"]

        est_dr_m2_gamma <- out_dr_gamma$estimates["treated_1_2", "estimate"]
        se_dr_m2_gamma <- out_dr_gamma$estimates["treated_1_2", "std.error"]


        df_dr <- data.frame(
          estimate = c(
            est_dr_marginal, est_dr_m0, est_dr_m1,
            est_dr_m2
          ),
          std.error = c(
            se_dr_marginal, se_dr_m0, se_dr_m1,
            se_dr_m2
          ),
          mediator = m,
          covars = "Yes", intermed_covars = "Yes",
          flexible = "No",
          method = "ACDE-BC",
          base_cond = c(
            "Marginal",
            "m = Cool",
            "m = Neutral",
            "m = Warm"
          ),
          n = nrow(est_df),
          stringsAsFactors = F
        )

        df_dr_gamma <- data.frame(
          estimate = c(
            est_dr_marginal_gamma, est_dr_m0_gamma, est_dr_m1_gamma,
            est_dr_m2_gamma
          ),
          std.error = c(
            se_dr_marginal_gamma, se_dr_m0_gamma, se_dr_m1_gamma,
            se_dr_m2_gamma
          ),
          mediator = m,
          covars = "Yes", intermed_covars = "Yes",
          flexible = "No",
          method = "ACDE-PC",
          base_cond = c(
            "Marginal",
            "m = Cool",
            "m = Neutral",
            "m = Warm"
          ),
          n = nrow(est_df),
          stringsAsFactors = F
        )

        out_table <- out_table_2 %>%
          bind_rows(out_table_3) %>%
          bind_rows(out_table_4) %>%
          bind_rows(df_dr) %>%
          bind_rows(df_dr_ml) %>%
          bind_rows(df_dr_gamma) %>%
          bind_rows(df_dr_gamma_ml) %>%
          mutate_if(is.numeric, round, 3) %>%
          mutate(outcome = y) %>%
          dplyr::select(method, outcome, everything())
      } else {
        out_table <- df_dr_ml %>%
          bind_rows(df_dr_gamma_ml) %>%
          mutate_if(is.numeric, round, 3) %>%
          mutate(outcome = y) %>%
          dplyr::select(method, outcome, everything())
      }



      # Add
    }) %>% reduce(rbind)
  })

  # TO DF
  out_df <- out_list %>%
    reduce(rbind) %>%
    mutate(method = factor(method, levels = c(
      "DiD w/ X, no mediator",
      "DiD w/ X, Z; no mediator",
      "DiD w/ X, Z + mediator",
      "ACDE-BC",
      "ACDE-PC"
    )[5:1]))
}

# Def inputs

# Covariates

X_in <- list(
  c(X_list, X_sq_terms, X_int_terms),
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

# Z

Z_in <- list(
  c(Z_list, Z_sq_terms, Z_int_terms, XZ_int_terms),
  Z_list, Z_list, Z_list
)
Z_in_labs <- c("1: Z - All int. covars")

# Folds

folds_in <- 5

# Mediators

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

# Optionally, get saved results instead of running the code (faster)

do_estimate <- T

if (do_estimate) {
  out <- lapply(folds_in, function(folds) {
    lapply(1:4, function(X_id) {
      cat("Folds: ", folds, "\tCovariates:", X_in_labs[X_id], "\n")

      o <- get_estimates(
        est_df = est_df, X_list_use = X_in[[X_id]],
        Z_list_use = Z_in[[X_id]], n_folds = folds
      ) %>%
        mutate(
          cov_label = X_in_labs[X_id],
          n_folds = folds
        )

      o
    }) %>% reduce(rbind)
  }) %>% reduce(rbind)

  write_rds(out, file = "results/main_output.rds")
} else {
  out <- read_rds("results/main_output.rds")
}


# Format for plotting ------------------------------------------------------

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
  mutate(n_folds = as.character(n_folds)) %>%
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

# Figure 5: Covariate Comparison ---------------------------------------------

p1 <- ggplot(
  out_df_all %>% filter(
    !str_detect(mediator, "tolerance"),
    method == "ACDE-BC", base_cond == "m = Neutral"
  ),
  aes(x = str_wrap(cov_label, 20), y = estimate, group = flexible)
) +
  geom_hline(
    yintercept = 0, linetype = "dotted",
    color = "grey60"
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error,
      color = flexible
    ),
    width = 0, size = 0.25,
    position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.645 * std.error,
      ymax = estimate + 1.645 * std.error,
      color = flexible
    ),
    width = 0, size = 0.75,
    position = position_dodge(0.4)
  ) +
  geom_point(aes(shape = flexible, fill = flexible, color = flexible),
    size = 2,
    position = position_dodge(0.4)
  ) +
  theme_light(base_family = "Fira Sans") +
  xlab("") +
  ylab("") +
  theme(plot.caption.position = "plot") +
  # coord_flip() +
  scale_color_discrete(name = "Uses ML?") +
  scale_shape_discrete(name = "Uses ML?") +
  scale_fill_discrete(name = "Uses ML?") +
  ylab("Estimate") +
  labs(caption = "Thick bars = 90% CIs, Thin bars = 95% CIs\nML-based estimates use LASSO for outcome, random forests for propensity score")
p1

do_save <- TRUE

if (do_save) {
  ggsave("output/ml_comparison.pdf",
    width = 7, height = 3,
    device = cairo_pdf
  )
}

# Figure 3: CDE Estimates ----------------------------------------------------

p1 <- out_df_all %>%
  filter(
    !str_detect(mediator, "tolerance"), cov_label == X_in_labs[2],
    flexible == "Yes" | str_detect(method, "DID"),
    method != "DID w/ X, Z; no mediator"
  ) |>
  ggplot(aes(x = base_cond, y = estimate, group = method)) +
  geom_hline(
    yintercept = 0, linetype = "dotted",
    color = "grey60"
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error,
      color = method
    ),
    width = 0, size = 0.25,
    position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.645 * std.error,
      ymax = estimate + 1.645 * std.error,
      color = method
    ),
    width = 0, size = 0.75,
    position = position_dodge(0.4)
  ) +
  geom_point(aes(shape = method, fill = method, color = method),
    size = 2,
    position = position_dodge(0.4)
  ) +
  theme_light(base_family = "Fira Sans") +
  xlab("") +
  ylab("") +
  theme(plot.caption.position = "plot") +
  # coord_flip() +
  scale_color_discrete(name = "Estimand Type", type = RColorBrewer::brewer.pal(4, "Dark2")) +
  scale_shape_discrete(name = "Estimand Type") +
  scale_fill_discrete(name = "Estimand Type", type = RColorBrewer::brewer.pal(4, "Dark2")) +
  ylab("Estimate") +
  labs(caption = "Thick bars = 90% CIs, Thin bars = 95% CIs")
p1

do_save <- TRUE

if (do_save) {
  ggsave("output/main_results.pdf",
    width = 7, height = 3, device = cairo_pdf
  )
}

# Cross tabs ---------------------------------------------------------------

p_df <- est_df %>% dplyr::select(
  therm_trans_t2,
  therm_trans_t3,
  base_med
)

tab1 <- table(p_df$base_med, p_df$therm_trans_t2)
colnames(tab1) <- 1:3 %>% as.character()
rownames(tab1) <- 1:3 %>% as.character()
tab1
