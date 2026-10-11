# Shared analysis pipeline for the report and Shiny dashboard.
# Run from the project root, or source this file from app.R / the R Markdown report.

project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "data", "student-mat.csv"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  stop("Could not locate data/student-mat.csv. Open the project folder or set the working directory to the project root.", call. = FALSE)
}

require_analysis_packages <- function() {
  needed <- c("ggplot2", "dplyr", "randomForest")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install required R packages before running the analysis: ",
         paste(missing, collapse = ", "),
         ". In RStudio, run install.packages(c(",
         paste(sprintf('"%s"', missing), collapse = ", "), ")).", call. = FALSE)
  }
}

load_student_data <- function(root = project_root()) {
  path <- file.path(root, "data", "student-mat.csv")
  if (!file.exists(path)) stop("Dataset not found: ", path, call. = FALSE)
  dat <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE,
                         na.strings = "NA", check.names = FALSE)
  required <- c("studytime", "failures", "absences", "G1", "G2", "G3")
  absent <- setdiff(required, names(dat))
  if (length(absent)) stop("Dataset is missing required columns: ", paste(absent, collapse = ", "), call. = FALSE)
  if (!is.numeric(dat$G3)) stop("G3 must be numeric for grade analysis.", call. = FALSE)
  dat
}

quality_checks <- function(raw) {
  empty_strings <- sum(vapply(raw[vapply(raw, is.character, logical(1))],
                              function(x) sum(!is.na(x) & trimws(x) == ""), numeric(1)))
  zero <- raw$G3 == 0
  zero[is.na(zero)] <- FALSE
  zero_absences <- raw$absences[zero]
  checks <- c(
    rows = nrow(raw), columns = ncol(raw), missing_values = sum(is.na(raw)),
    empty_strings = empty_strings, duplicate_rows = sum(duplicated(raw)),
    G3_out_of_range = sum(!is.na(raw$G3) & (raw$G3 < 0 | raw$G3 > 20)),
    negative_absences = sum(!is.na(raw$absences) & raw$absences < 0),
    G3_equal_zero = sum(zero),
    G3zero_absences_all_zero = length(zero_absences) > 0 && all(zero_absences == 0, na.rm = TRUE),
    G3zero_absences_count = sum(zero_absences == 0, na.rm = TRUE)
  )
  data.frame(check = names(checks), value = as.character(checks), row.names = NULL,
             stringsAsFactors = FALSE)
}

prepare_student_data <- function(raw) {
  dat <- raw
  character_columns <- vapply(dat, is.character, logical(1))
  dat[character_columns] <- lapply(dat[character_columns], factor)
  dat$studytime_f <- factor(dat$studytime, levels = 1:4,
                            labels = c("<2 h", "2-5 h", "5-10 h", ">10 h"), ordered = TRUE)
  dat$failures_f <- factor(dat$failures, levels = sort(unique(dat$failures[!is.na(dat$failures)])))
  dat
}

# Learn imputation and factor encoding on the training rows only. Test categories
# absent from training map to that predictor's training-set modal category.
encode_predictors <- function(train, test, predictors) {
  x_train <- list(); x_test <- list(); metadata <- list()
  for (nm in predictors) {
    tr <- train[[nm]]; te <- test[[nm]]
    if (is.factor(tr) || is.character(tr) || is.logical(tr)) {
      tr <- as.character(tr); te <- as.character(te)
      observed <- tr[!is.na(tr) & nzchar(tr)]
      if (!length(observed)) {
        tr[is.na(tr) | !nzchar(tr)] <- "(missing)"
        te[is.na(te) | !nzchar(te)] <- "(missing)"
        observed <- "(missing)"
      } else {
        counts <- sort(table(observed), decreasing = TRUE)
        modal <- names(counts)[1]
        tr[is.na(tr) | !nzchar(tr)] <- modal
        te[is.na(te) | !nzchar(te)] <- modal
        te[!te %in% unique(observed)] <- modal
      }
      levels_train <- sort(unique(tr))
      if (length(levels_train) > 1L) {
        for (lev in levels_train[-1L]) {
          safe_lev <- make.names(lev)
          col <- make.names(paste(nm, safe_lev, sep = "__"))
          x_train[[col]] <- as.numeric(tr == lev)
          x_test[[col]] <- as.numeric(te == lev)
          metadata[[col]] <- data.frame(feature = col, source = nm, encoding = paste0("indicator: ", lev))
        }
      }
    } else {
      tr <- suppressWarnings(as.numeric(tr)); te <- suppressWarnings(as.numeric(te))
      med <- if (all(is.na(tr))) 0 else stats::median(tr, na.rm = TRUE)
      tr[is.na(tr)] <- med; te[is.na(te)] <- med
      col <- make.names(nm)
      x_train[[col]] <- tr; x_test[[col]] <- te
      metadata[[col]] <- data.frame(feature = col, source = nm, encoding = "numeric")
    }
  }
  train_df <- as.data.frame(x_train, check.names = FALSE)
  test_df <- as.data.frame(x_test, check.names = FALSE)
  if (!ncol(train_df)) {
    train_df$.constant <- 0; test_df$.constant <- 0
  }
  keep <- vapply(train_df, function(x) length(unique(x)) > 1L, logical(1))
  train_df <- train_df[, keep, drop = FALSE]
  test_df <- test_df[, names(train_df), drop = FALSE]
  list(train = train_df, test = test_df,
       metadata = if (length(metadata)) do.call(rbind, metadata) else data.frame())
}

fit_linear_model <- function(x, y) {
  design <- cbind(`(Intercept)` = 1, as.matrix(x))
  fit <- stats::lm.fit(design, y)
  coefficients <- fit$coefficients
  coefficients[is.na(coefficients)] <- 0
  list(coefficients = coefficients, fitted = as.numeric(design %*% coefficients),
       predict = function(new_x) as.numeric(cbind(`(Intercept)` = 1, as.matrix(new_x)) %*% coefficients),
       rank = fit$rank)
}

regression_metrics <- function(actual, predicted) {
  valid <- is.finite(actual) & is.finite(predicted)
  actual <- actual[valid]; predicted <- predicted[valid]
  if (!length(actual)) return(c(MAE = NA_real_, RMSE = NA_real_, R2 = NA_real_, n = 0))
  sst <- sum((actual - mean(actual))^2)
  c(MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R2 = if (sst > 0) 1 - sum((actual - predicted)^2) / sst else NA_real_,
    n = length(actual))
}

fit_and_predict_models <- function(train, test, seed = 123, ntree = 500) {
  predictors <- setdiff(names(train), c("G3", "studytime_f", "failures_f"))
  x_a <- encode_predictors(train, test, setdiff(predictors, c("G1", "G2")))
  x_b <- encode_predictors(train, test, predictors)
  y_train <- train$G3; y_test <- test$G3
  set.seed(seed)
  lm_a <- fit_linear_model(x_a$train, y_train)
  lm_b <- fit_linear_model(x_b$train, y_train)
  rf_a <- randomForest::randomForest(x = x_a$train, y = y_train, ntree = ntree, importance = TRUE)
  rf_b <- randomForest::randomForest(x = x_b$train, y = y_train, ntree = ntree, importance = TRUE)
  predictions <- list(
    "Mean-grade baseline" = rep(mean(y_train, na.rm = TRUE), length(y_test)),
    "Linear regression - Model A" = lm_a$predict(x_a$test),
    "Linear regression - Model B" = lm_b$predict(x_b$test),
    "Random forest - Model A" = as.numeric(predict(rf_a, newdata = x_a$test)),
    "Random forest - Model B" = as.numeric(predict(rf_b, newdata = x_b$test))
  )
  list(predictions = predictions, actual = y_test, models = list(lm_a = lm_a, lm_b = lm_b,
       rf_a = rf_a, rf_b = rf_b), encoded = list(a = x_a, b = x_b))
}

cross_validated_rmse <- function(dat, folds = 5L, seed = 123, ntree = 300L) {
  set.seed(seed)
  fold_id <- sample(rep(seq_len(folds), length.out = nrow(dat)))
  names_models <- c("Mean-grade baseline", "Linear regression - Model A", "Linear regression - Model B",
                    "Random forest - Model A", "Random forest - Model B")
  squared_errors <- setNames(vector("list", length(names_models)), names_models)
  for (fold in seq_len(folds)) {
    train <- dat[fold_id != fold, , drop = FALSE]
    test <- dat[fold_id == fold, , drop = FALSE]
    fitted <- fit_and_predict_models(train, test, seed = seed + fold, ntree = ntree)
    for (nm in names_models) {
      ok <- is.finite(test$G3) & is.finite(fitted$predictions[[nm]])
      squared_errors[[nm]] <- c(squared_errors[[nm]], (test$G3[ok] - fitted$predictions[[nm]][ok])^2)
    }
  }
  data.frame(Model = names_models,
             CV_RMSE = vapply(squared_errors, function(x) sqrt(mean(x)), numeric(1)),
             CV_folds = folds, row.names = NULL)
}

analysis_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#0B1F3A"),
                   plot.subtitle = ggplot2::element_text(color = "#536579"),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(color = "#26384A"),
                   strip.text = ggplot2::element_text(face = "bold", color = "#0B1F3A"))
}

run_analysis <- function(root = project_root(), write_outputs = TRUE, run_cv = TRUE) {
  require_analysis_packages()
  raw <- load_student_data(root)
  df <- prepare_student_data(raw)
  quality <- quality_checks(raw)
  numeric_vars <- c("studytime", "absences", "failures", "G1", "G2", "G3")
  desc <- data.frame(Variable = numeric_vars,
                     Mean = vapply(df[numeric_vars], mean, numeric(1), na.rm = TRUE),
                     SD = vapply(df[numeric_vars], stats::sd, numeric(1), na.rm = TRUE),
                     Median = vapply(df[numeric_vars], stats::median, numeric(1), na.rm = TRUE),
                     Min = vapply(df[numeric_vars], min, numeric(1), na.rm = TRUE),
                     Max = vapply(df[numeric_vars], max, numeric(1), na.rm = TRUE), row.names = NULL)
  by_study <- dplyr::summarise(dplyr::group_by(df, studytime_f), n = dplyr::n(),
                               mean_G3 = mean(G3, na.rm = TRUE), median_G3 = median(G3, na.rm = TRUE),
                               .groups = "drop")
  corr_vars <- c("studytime", "absences", "failures", "G1", "G2")
  zero <- !is.na(df$G3) & df$G3 > 0
  cor_tab <- data.frame(Variable = corr_vars,
    Spearman_all = vapply(corr_vars, function(v) cor(df[[v]], df$G3, method = "spearman", use = "complete.obs"), numeric(1)),
    Spearman_G3_gt0 = vapply(corr_vars, function(v) cor(df[[v]][zero], df$G3[zero], method = "spearman", use = "complete.obs"), numeric(1)))
  numeric_matrix <- stats::cor(df[numeric_vars], use = "pairwise.complete.obs")
  n <- nrow(df)
  set.seed(123)
  train_idx <- sample(seq_len(n), floor(0.8 * n))
  train <- df[train_idx, , drop = FALSE]
  test <- df[-train_idx, , drop = FALSE]
  fitted <- fit_and_predict_models(train, test, seed = 123, ntree = 500)
  metric_matrix <- t(vapply(fitted$predictions, function(p) regression_metrics(fitted$actual, p), numeric(4)))
  model_results <- data.frame(Model = rownames(metric_matrix), metric_matrix,
                              Test_n = nrow(test), Train_n = nrow(train),
                              Evaluation = "80/20 held-out test", row.names = NULL, check.names = FALSE)
  if (run_cv) {
    cv <- cross_validated_rmse(df, folds = 5L, seed = 123, ntree = 300L)
    model_results$CV_RMSE <- cv$CV_RMSE[match(model_results$Model, cv$Model)]
    model_results$CV_method <- "5-fold CV; preprocessing refit per fold"
  } else {
    cv <- data.frame(Model = character(), CV_RMSE = numeric(), CV_folds = integer())
    model_results$CV_RMSE <- NA_real_; model_results$CV_method <- "Not run"
  }
  encoded <- fitted$encoded
  coefficient_table <- do.call(rbind, lapply(c("A", "B"), function(which_model) {
    fit <- fitted$models[[paste0("lm_", tolower(which_model))]]
    feature_names <- names(fit$coefficients)[-1]
    data.frame(Model = which_model, Feature = feature_names,
               Estimate = unname(fit$coefficients[-1]), stringsAsFactors = FALSE)
  }))
  importance_raw <- randomForest::importance(fitted$models$rf_b, type = 2)
  importance <- data.frame(Feature = rownames(importance_raw),
                           IncNodePurity = importance_raw[, 1], row.names = NULL)
  importance <- importance[order(importance$IncNodePurity, decreasing = TRUE), , drop = FALSE]
  importance$RelativeImportancePct <- 100 * importance$IncNodePurity / sum(importance$IncNodePurity)
  pred_plot_data <- do.call(rbind, lapply(c("Linear regression - Model A", "Linear regression - Model B"), function(nm) {
    data.frame(Model = sub("Linear regression - ", "", nm), Actual = fitted$actual,
               Predicted = fitted$predictions[[nm]])
  }))
  figs <- list(
    grade_distribution = ggplot2::ggplot(df, ggplot2::aes(G3)) +
      ggplot2::geom_histogram(binwidth = 1, boundary = -0.5, fill = "#0F8B8D", color = "white") +
      ggplot2::scale_x_continuous(breaks = 0:20, limits = c(-0.5, 20.5)) +
      ggplot2::labs(title = "Final Mathematics grade distribution", x = "G3 (0–20)", y = "Students") + analysis_theme(),
    studytime = ggplot2::ggplot(df, ggplot2::aes(studytime_f, G3)) +
      ggplot2::geom_boxplot(fill = "#CDE9E9", color = "#0B1F3A", outlier.alpha = .45) +
      ggplot2::labs(title = "Final grade by weekly study time", x = "Weekly study time", y = "G3 (0–20)") + analysis_theme(),
    absences = ggplot2::ggplot(df, ggplot2::aes(absences, G3)) +
      ggplot2::geom_point(color = "#0B1F3A", alpha = .48) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "#0F8B8D", fill = "#CDE9E9") +
      ggplot2::labs(title = "Absences and final grade", x = "Recorded absences", y = "G3 (0–20)") + analysis_theme(),
    failures = ggplot2::ggplot(df, ggplot2::aes(failures_f, G3)) +
      ggplot2::geom_boxplot(fill = "#CDE9E9", color = "#0B1F3A", outlier.alpha = .45) +
      ggplot2::labs(title = "Final grade by previous class failures", x = "Past failures", y = "G3 (0–20)") + analysis_theme(),
    heatmap = {
      corr_long <- as.data.frame(as.table(numeric_matrix)); names(corr_long) <- c("Variable1", "Variable2", "Correlation")
      ggplot2::ggplot(corr_long, ggplot2::aes(Variable1, Variable2, fill = Correlation)) +
        ggplot2::geom_tile(color = "white") + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), size = 3) +
        ggplot2::scale_fill_gradient2(low = "#B85C5C", mid = "white", high = "#0F8B8D", midpoint = 0, limits = c(-1, 1)) +
        ggplot2::coord_equal() + ggplot2::labs(title = "Numeric-variable correlation matrix", x = NULL, y = NULL, fill = "Pearson r") +
        analysis_theme() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
    },
    importance = {
      top_imp <- utils::head(importance, 15)
      top_imp$Feature <- factor(top_imp$Feature, levels = rev(top_imp$Feature))
      ggplot2::ggplot(top_imp, ggplot2::aes(Feature, RelativeImportancePct)) +
        ggplot2::geom_col(fill = "#0F8B8D") + ggplot2::coord_flip() +
        ggplot2::labs(title = "Random-forest Model B variable importance", x = NULL,
                      y = "Relative IncNodePurity (%)") + analysis_theme()
    },
    prediction = ggplot2::ggplot(pred_plot_data, ggplot2::aes(Actual, Predicted)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "#D99A32", linetype = "dashed") +
      ggplot2::geom_point(color = "#0B1F3A", alpha = .62) +
      ggplot2::facet_wrap(~Model) + ggplot2::coord_equal(xlim = c(0, 20), ylim = c(0, 20)) +
      ggplot2::labs(title = "Linear-regression predictions on held-out test records", x = "Actual G3", y = "Predicted G3") + analysis_theme()
  )
  if (write_outputs) {
    dir.create(file.path(root, "results"), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(root, "figures"), showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(quality, file.path(root, "results/quality.csv"), row.names = FALSE)
    utils::write.csv(desc, file.path(root, "results/desc.csv"), row.names = FALSE)
    utils::write.csv(by_study, file.path(root, "results/by_study.csv"), row.names = FALSE)
    utils::write.csv(cor_tab, file.path(root, "results/cor.csv"), row.names = FALSE)
    utils::write.csv(model_results, file.path(root, "results/model_results.csv"), row.names = FALSE)
    utils::write.csv(coefficient_table, file.path(root, "results/coef.csv"), row.names = FALSE)
    utils::write.csv(importance, file.path(root, "results/rf_imp.csv"), row.names = FALSE)
    top_a <- coefficient_table[coefficient_table$Model == "A", , drop = FALSE]
    top_a <- top_a[order(abs(top_a$Estimate), decreasing = TRUE), , drop = FALSE]
    utils::write.csv(utils::head(top_a, 6), file.path(root, "results/topA.csv"), row.names = FALSE)
    utils::write.csv(data.frame(type = names(table(vapply(raw, function(x) class(x)[1], character(1)))),
                                count = as.integer(table(vapply(raw, function(x) class(x)[1], character(1))))),
                     file.path(root, "results/types.csv"), row.names = FALSE)
    train_sst <- sum((train$G3 - mean(train$G3))^2)
    train_r2 <- c(A = 1 - sum((train$G3 - fitted$models$lm_a$fitted)^2) / train_sst,
            B = 1 - sum((train$G3 - fitted$models$lm_b$fitted)^2) / train_sst)
    utils::write.csv(data.frame(Model = names(train_r2), Train_R2 = unname(train_r2)),
             file.path(root, "results/train_r2.csv"), row.names = FALSE)
    utils::write.csv(cv, file.path(root, "results/cv_results.csv"), row.names = FALSE)
    utils::write.csv(pred_plot_data, file.path(root, "results/test_predictions.csv"), row.names = FALSE)
    plot_files <- c(grade_distribution = "fig0_grade_distribution.png", studytime = "fig1_studytime.png",
                    absences = "fig2_absences.png", failures = "fig3_failures.png",
                    prediction = "fig4_pred_actual.png", heatmap = "fig5_correlation_heatmap.png",
                    importance = "fig6_rf_importance.png")
    for (nm in names(plot_files)) ggplot2::ggsave(file.path(root, "figures", plot_files[[nm]]), figs[[nm]],
                                                   width = 7, height = 4.5, dpi = 160, bg = "white")
  }
  list(root = root, raw = raw, data = df, quality = quality, desc = desc, by_study = by_study,
       correlations = cor_tab, correlation_matrix = numeric_matrix, train = train, test = test,
       models = fitted$models, predictions = fitted$predictions, actual = fitted$actual,
       metrics = model_results, coefficients = coefficient_table, importance = importance,
       cv = cv, figures = figs, zero_grade_n = sum(df$G3 == 0, na.rm = TRUE),
       zero_grade_absences_all_zero = all(df$absences[df$G3 == 0] == 0, na.rm = TRUE))
}
