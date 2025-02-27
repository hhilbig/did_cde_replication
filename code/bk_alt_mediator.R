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

# Function: 5 categories

make_5cats <- function(v) {
  case_when(
    v == 0 ~ 0,
    v > 0 & v < 50 ~ 1,
    v == 50 ~ 2,
    v > 50 & v < 100 ~ 3,
    v == 100 ~ 4
  )
}

# 5 categories

M_cat_labs <- c("m = 0", "0 < m < 50", "m = 50", "50 < m < 100", "m = 100")

# Make 5 categories

df <- df %>%
  mutate(mediator_original = therm_trans_t2) %>%
  mutate_at(vars(one_of(M_list)), make_5cats)

## Define Y_list

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
  mutate(base_med = make_5cats(therm_trans_t0))

## Rename

est_df <- df

## Drop missings

sum(complete.cases(df[, c(
  "therm_trans_t2",
  "treated",
  "miami_trans_law_t3_avg_diff",
  "base_med",
  "gender_nonconformity_t0",
  "trans.tolerance.dv.t0"
)]))


est_df <- est_df[complete.cases(df[, c(
  "therm_trans_t2",
  "treated",
  "miami_trans_law_t3_avg_diff",
  "base_med",
  "gender_nonconformity_t0",
  "trans.tolerance.dv.t0"
)]), ]

# Function to get estimates

get_estimates <- function(est_df,
                          X_list_use = X_list,
                          Z_list_use = Z_list,
                          n_folds = 5, subset = NULL) {
  out_list <- lapply(M_list, function(m) {
    lapply(Y_list, function(y) {
      cat("Outcome: ", y, "\tMediator: ", m, "\n")
      ## Drop missings


      big_form <- as.formula(paste0(
        y, " ~ treated + base_med + ", m, "+",
        paste0(X_list_use,
          collapse = "+"
        ), "+",
        paste0(Z_list_use, collapse = "+")
      ))

      ## allows us to use squared terms/interactions
      if (!is.null(subset)) {
        est_df <- model.frame(big_form, data = df, subset = subset)
      } else {
        est_df <- model.frame(big_form, data = df)
      }

      ## est_df <- est_df[complete.cases(est_df[, c(m,
      ##                                            'treated',
      ##                                            y,
      ##                                            'base_med',
      ##                                            X_list_use,
      ##                                            Z_list_use)]),]

      ## Def outcome

      est_df[, "y"] <- est_df[, y]
      est_df_temp <- est_df %>%
        filter(!is.na(y))


      #### Our method ####

      ## New Doubly Robust estimator

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

      ## Pscore formula

      f_m_mod <- as.formula(paste0(
        m, "~ treated + base_med + ",
        paste0(X_list_use %>% setdiff("therm_trans_t0"),
          collapse = "+"
        ), "+",
        paste0(Z_list_use, collapse = "+")
      ))


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



      df_dr_ml <- data.frame(
        estimate = out_dr_ml$estimates$estimate,
        std.error = out_dr_ml$estimates$std.error,
        mediator = m,
        covars = "Yes", intermed_covars = "Yes",
        flexible = "Yes",
        method = "ACDE-BC",
        base_cond = c(M_cat_labs, "Marginal"),
        n = out_dr_ml$estimate$DF,
        stringsAsFactors = FALSE
      )

      df_dr_gamma_ml <- data.frame(
        estimate = out_dr_gamma_ml$estimates$estimate,
        std.error = out_dr_gamma_ml$estimates$std.error,
        mediator = m,
        covars = "Yes", intermed_covars = "Yes",
        flexible = "Yes",
        method = "ACDE-PC",
        base_cond = c(M_cat_labs, "Marginal"),
        n = out_dr_gamma_ml$estimate$DF,
        stringsAsFactors = FALSE
      )

      ## Add to table

      did_x_form <- as.formula(
        paste0(
          "y ~ treated + base_med +",
          paste0(c(X_list_use %>% setdiff("therm_trans_t0")),
            collapse = "+"
          )
        )
      )
      m2 <- felm(did_x_form,
        data = est_df_temp
      )

      mlist <- lapply(
        0:4,
        function(num) {
          felm(did_x_form,
            data = est_df_temp[est_df_temp$base_med == num, ]
          )
        }
      )


      mlist <- c(list(m2), mlist)
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
          base_cond = c("Marginal", M_cat_labs)
        ) %>%
        dplyr::select(
          estimate, std.error, mediator, covars,
          intermed_covars, method, flexible, base_cond
        ) %>%
        mutate(n = sapply(mlist, function(x) sum(!is.na(x$response))))

      out_table <- out_table_2 %>%
        bind_rows(df_dr_ml) %>%
        bind_rows(df_dr_gamma_ml) %>%
        mutate_if(is.numeric, round, 3) %>%
        mutate(outcome = y) %>%
        dplyr::select(method, outcome, everything())



      ## Add
    }) %>% reduce(rbind)
  })

  ## TO DF
  out_df <- out_list %>%
    reduce(rbind) %>%
    mutate(method = factor(method, levels = c(
      "DiD w/ X, no mediator",
      "ACDE-BC",
      "ACDE-PC"
    )[5:1]))
}

## Def inputs

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
  Z_list,
  c(
    "therm_obama_t1_diff",
    "trans.tolerance.dv.t1_diff",
    "gender_nonconformity_t1_diff",
    "miami_trans_law_t1_avg_diff"
  )
)

# Z labels

Z_in_labs <- c(
  "1: Z - All int. covars",
  "2: Z - Indices + Demographics"
)


# Folds

folds <- 5

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

do_estimate <- FALSE

if (do_estimate) {
  out <- get_estimates(
    est_df = est_df, X_list_use = X_in[[3L]],
    Z_list_use = Z_in[[2L]], n_folds = folds
  ) %>%
    mutate(
      cov_label = X_in_labs[3L],
      n_folds = folds
    )

  write_rds(out, file = "results/alt_mediator_output.rds")
} else {
  out <- read_rds("results/alt_mediator_output.rds")
}

# Format for table ------------------------------------------------------

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
      "ACDE-BC",
      "ACDE-PC"
    )),
    base_cond = factor(base_cond, levels = c("Marginal", M_cat_labs))
  )

# Table SM.6: ACDE-BC estimates with alternative discretization of the mediator ----------------------------------------

cat5_table <- out_df_all |>
  filter(
    method %in% c("ACDE-BC", "DID w/ X, no mediator"),
    (flexible == "Yes" & method == "ACDE-BC") |
      (flexible == "No" & method == "DID w/ X, no mediator")
  ) |>
  mutate(
    method = if_else(method == "ACDE-BC", "ACDE_BC", "ATT")
  ) |>
  select(method, base_cond, estimate, std.error, n) |>
  pivot_wider(
    id_cols = c(base_cond),
    names_from = method,
    values_from = c(estimate, std.error, n)
  ) |>
  rename(
    `Baseline Mediator` = base_cond, `ACDE-BC (s.e.)` = estimate_ACDE_BC,
    `ATT (s.e.)` = estimate_ATT, `n (ACDE)` = n_ACDE_BC, `n (ATT)` = n_ATT
  ) |>
  select(
    `Baseline Mediator`, `ACDE-BC (s.e.)`, `n (ACDE)`, `ATT (s.e.)`,
    `n (ATT)`, everything()
  )
cat5_table

# Optional save

do_save <- TRUE

if (do_save) {
  cat5_table |>
    gt() |>
    cols_merge(
      columns = c(`ACDE-BC (s.e.)`, std.error_ACDE_BC),
      pattern = "{1} ({2})"
    ) |>
    cols_merge(
      columns = c(`ATT (s.e.)`, std.error_ATT),
      pattern = "{1} ({2})"
    ) |>
    cols_align(
      align = "left",
      columns = c(`Baseline Mediator`)
    ) |>
    tab_options(latex.use_longtable = TRUE) |>
    gtsave("output/cat5_table.tex")
}
