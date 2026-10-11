# Smart Study Pattern Analysis — interactive R Shiny dashboard
# Launch from this folder with: shiny::runApp()

required_app_packages <- c("shiny", "ggplot2", "dplyr", "randomForest")
missing_app_packages <- required_app_packages[!vapply(required_app_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_app_packages)) {
  stop("Missing R packages: ", paste(missing_app_packages, collapse = ", "),
       ". Install them with install.packages(c(",
       paste(sprintf('"%s"', missing_app_packages), collapse = ", "), ")).", call. = FALSE)
}

source("analysis.R", local = TRUE)
analysis <- run_analysis(write_outputs = TRUE, run_cv = TRUE)
dat <- analysis$data

navy <- "#0B1F3A"
teal <- "#0F8B8D"

ui <- shiny::navbarPage(
  title = shiny::div(class = "brand", shiny::span(class = "brand-mark", "IDS"),
                     shiny::span("Smart Study Analytics")),
  id = "main_tabs", collapsible = TRUE,
  header = shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML("\n      :root { --navy:#0B1F3A; --teal:#0F8B8D; --ink:#1D2B3A; --muted:#637386; --line:#E5EBF0; --paper:#F4F7FA; }\n      body { background:var(--paper); color:var(--ink); font-family:'Segoe UI',Arial,sans-serif; }\n      .navbar { background:var(--navy); border:0; border-radius:0; margin-bottom:0; box-shadow:0 3px 14px rgba(11,31,58,.16); }\n      .navbar-default .navbar-brand, .navbar-default .navbar-nav>li>a { color:#DCE7F1; }\n      .navbar-default .navbar-brand:hover, .navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus { color:white; background:rgba(255,255,255,.08); }\n      .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover { color:white; background:var(--teal); }\n      .brand { display:flex; align-items:center; gap:10px; font-weight:700; } .brand-mark { background:var(--teal); color:white; border-radius:8px; padding:4px 7px; font-size:12px; letter-spacing:.06em; }\n      .page-wrap { max-width:1440px; margin:0 auto; padding:25px 28px 40px; }\n      .hero { background:linear-gradient(115deg,var(--navy),#153C5B); color:white; border-radius:16px; padding:25px 30px; margin:6px 0 22px; box-shadow:0 8px 25px rgba(11,31,58,.12); }\n      .hero h1 { font-size:25px; font-weight:700; margin:0 0 8px; } .hero p { margin:0; color:#D9E7EF; line-height:1.55; }\n      .hero-meta { margin-top:13px; color:#B8D4DC; font-size:13px; }\n      .section-title { color:var(--navy); font-size:21px; font-weight:700; margin:5px 0 14px; }\n      .subtle { color:var(--muted); } .kpi-grid { display:grid; grid-template-columns:repeat(6,minmax(130px,1fr)); gap:13px; margin-bottom:18px; }\n      .kpi { background:#fff; border:1px solid var(--line); border-radius:13px; padding:16px 17px; min-height:104px; box-shadow:0 3px 12px rgba(11,31,58,.035); }\n      .kpi-label { font-size:12px; color:var(--muted); text-transform:uppercase; letter-spacing:.055em; font-weight:700; } .kpi-value { color:var(--navy); font-size:26px; font-weight:750; margin-top:8px; }\n      .card { background:#fff; border:1px solid var(--line); border-radius:14px; padding:18px 20px; margin-bottom:17px; box-shadow:0 3px 12px rgba(11,31,58,.035); }\n      .card h3 { margin:0 0 12px; font-size:16px; font-weight:700; color:var(--navy); }\n      .callout { background:#EAF5F5; border-left:4px solid var(--teal); border-radius:8px; padding:13px 16px; margin:12px 0; color:#24414A; }\n      .warning { background:#FFF7E8; border-left:4px solid #D99A32; border-radius:8px; padding:13px 16px; margin:12px 0; color:#604B26; }\n      .table { background:#fff; } .table>thead>tr>th { background:#EDF3F7; color:var(--navy); border-bottom:1px solid var(--line); }\n      .control-label { color:var(--navy); font-weight:650; } .well { background:white; border-color:var(--line); border-radius:13px; box-shadow:0 3px 12px rgba(11,31,58,.035); }\n      .btn-primary { background:var(--teal); border-color:var(--teal); } .btn-primary:hover { background:#0b7476; border-color:#0b7476; }\n      .footer-note { font-size:12px; color:var(--muted); border-top:1px solid var(--line); padding-top:12px; margin-top:22px; }\n      @media(max-width:1100px){.kpi-grid{grid-template-columns:repeat(3,minmax(130px,1fr));}}\n      @media(max-width:700px){.page-wrap{padding:16px 12px 28px}.kpi-grid{grid-template-columns:repeat(2,minmax(120px,1fr))}.hero{padding:20px}}\n    "))),
    shiny::div(class = "page-wrap",
      shiny::div(class = "hero",
        shiny::h1("Smart Study Pattern Analysis & Academic Productivity Prediction"),
        shiny::p("An evidence-led exploration of study patterns, attendance, prior achievement, and Mathematics outcomes."),
        shiny::div(class = "hero-meta", "Bhukya Pooja  ·  Roll No. 1601-24-771-003  ·  Introduction to Data Science  ·  Semester 5, B.Tech")
      )
    )
  ),
  shiny::tabPanel("Overview", value = "overview",
    shiny::div(class = "page-wrap",
      shiny::h2(class = "section-title", "Dataset at a glance"),
      shiny::uiOutput("kpis"),
      shiny::fluidRow(
        shiny::column(7, shiny::div(class = "card", shiny::h3("Final-grade distribution"), shiny::plotOutput("grade_plot", height = 310))),
        shiny::column(5, shiny::div(class = "card", shiny::h3("Research problem and workflow"),
          shiny::p("This case study asks how weekly study time, recorded absences, past failures, and prior-period grades are associated with a student's final Mathematics grade (G3)."),
          shiny::p("Workflow: verify and transform the source data; describe and visualize patterns; quantify rank correlations; compare baseline, linear-regression, and random-forest predictions on one reproducible 80/20 hold-out split; then examine sensitivity and limitations."),
          shiny::div(class = "callout", shiny::strong("Scope note. "), "These are Portuguese secondary-school Mathematics records—not CBIT student records. The dataset is observational and does not establish cause and effect."),
          shiny::h3("Data-quality checks"), shiny::tableOutput("quality_table")
        ))
      ),
      shiny::div(class = "footer-note", "Source: UCI Student Performance dataset (Mathematics file). G3 is the final grade on a 0–20 scale. Zero-grade records are retained.")
    )
  ),
  shiny::tabPanel("Study Pattern Analysis", value = "study",
    shiny::div(class = "page-wrap",
      shiny::h2(class = "section-title", "Weekly study time and final performance"),
      shiny::p(class = "subtle", "Study time is reported in ordered weekly categories. The boxplot shows distribution and overlap, not a causal effect."),
      shiny::fluidRow(shiny::column(7, shiny::div(class = "card", shiny::plotOutput("study_plot", height = 350))),
                      shiny::column(5, shiny::div(class = "card", shiny::h3("Group summary"), shiny::tableOutput("study_table"),
                                                  shiny::h3("Spearman association"), shiny::uiOutput("study_interpretation"))))
    )
  ),
  shiny::tabPanel("Attendance & Past Failures", value = "attendance",
    shiny::div(class = "page-wrap",
      shiny::h2(class = "section-title", "Absences, previous failures, and G3"),
      shiny::fluidRow(shiny::column(7, shiny::div(class = "card", shiny::plotOutput("absence_plot", height = 330))),
                      shiny::column(5, shiny::div(class = "card", shiny::h3("Rank correlations"), shiny::tableOutput("attendance_cor"), shiny::uiOutput("attendance_interpretation")))),
      shiny::fluidRow(shiny::column(7, shiny::div(class = "card", shiny::plotOutput("failure_plot", height = 320))),
                      shiny::column(5, shiny::div(class = "card", shiny::h3("Sensitivity: exclude G3 = 0"), shiny::tableOutput("sensitivity_table"),
                        shiny::div(class = "warning", "An overall small correlation is not evidence that attendance has no effect. Absences may be skewed, effects may vary across students, and this observational dataset omits important context."))))
    )
  ),
  shiny::tabPanel("Predictor & Correlation Analysis", value = "predictors",
    shiny::div(class = "page-wrap",
      shiny::h2(class = "section-title", "Associations and model-based importance"),
      shiny::fluidRow(shiny::column(6, shiny::div(class = "card", shiny::h3("Pearson correlation heatmap"), shiny::plotOutput("heatmap", height = 350))),
                      shiny::column(6, shiny::div(class = "card", shiny::h3("Spearman correlations with G3"), shiny::tableOutput("cor_table"),
                        shiny::p(class = "subtle", "The heatmap summarizes linear association among numeric variables; Spearman coefficients summarize rank association with G3.")))),
      shiny::fluidRow(shiny::column(6, shiny::div(class = "card", shiny::h3("Model B coefficient estimates"), shiny::tableOutput("coef_table"),
                        shiny::p(class = "subtle", "The table shows the largest absolute encoded coefficients. Numeric predictors retain their source units; categorical terms are indicator contrasts. Coefficients across differently scaled variables are not directly comparable."))),
                      shiny::column(6, shiny::div(class = "card", shiny::h3("Random-forest Model B importance"), shiny::plotOutput("importance_plot", height = 360),
                        shiny::p(class = "subtle", "Relative IncNodePurity is a within-model split-purity summary, not a causal effect or a probability of influence."))))
    )
  ),
  shiny::tabPanel("Academic Performance Prediction", value = "prediction",
    shiny::div(class = "page-wrap",
      shiny::h2(class = "section-title", "Held-out predictive performance"),
      shiny::div(class = "callout", shiny::strong("Evaluation. "), "One seeded 80/20 train-test split; all five models use the same held-out students. MAE and RMSE are in grade points; test R² is computed from held-out predictions. Five-fold CV refits preprocessing within each fold."),
      shiny::fluidRow(shiny::column(5, shiny::div(class = "card", shiny::h3("Model comparison"), shiny::tableOutput("model_table"))),
                      shiny::column(7, shiny::div(class = "card", shiny::h3("Test RMSE and R²"), shiny::plotOutput("metrics_plot", height = 350)))),
      shiny::div(class = "card", shiny::h3("Actual versus predicted: linear regression"), shiny::plotOutput("prediction_plot", height = 380)),
      shiny::div(class = "warning", shiny::strong("Model distinction. "), "Model A uses background and study-pattern predictors but excludes G1 and G2. Model B adds the earlier grade periods; this usually improves prediction because those grades are close measures of prior achievement. That makes Model B less suitable for support decisions made before those grades exist. A single split does not establish universal model superiority.")
    )
  ),
  shiny::tabPanel("Interactive Student Analysis", value = "interactive",
    shiny::div(class = "page-wrap",
      shiny::h2(class = "section-title", "Explore subsets of the existing dataset"),
      shiny::p(class = "subtle", "Filters explore records in this anonymized secondary-school dataset; they do not identify real students."),
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3,
          shiny::selectInput("study_filter", "Weekly study time", choices = c("All" = "All", levels(dat$studytime_f)), selected = "All"),
          shiny::sliderInput("failure_filter", "Past failures", min = min(dat$failures, na.rm = TRUE), max = max(dat$failures, na.rm = TRUE), value = range(dat$failures, na.rm = TRUE), step = 1),
          shiny::sliderInput("absence_filter", "Absences", min = min(dat$absences, na.rm = TRUE), max = max(dat$absences, na.rm = TRUE), value = range(dat$absences, na.rm = TRUE), step = 1),
          shiny::sliderInput("g1_filter", "Previous grade G1", min = min(dat$G1, na.rm = TRUE), max = max(dat$G1, na.rm = TRUE), value = range(dat$G1, na.rm = TRUE), step = 1),
          shiny::actionButton("reset_filters", "Reset filters", class = "btn-primary")
        ),
        shiny::mainPanel(width = 9,
          shiny::uiOutput("selection_kpi"),
          shiny::fluidRow(shiny::column(6, shiny::div(class = "card", shiny::h3("Selected G3 distribution"), shiny::plotOutput("filtered_grade_plot", height = 300))),
                          shiny::column(6, shiny::div(class = "card", shiny::h3("G3 by study category"), shiny::plotOutput("filtered_group_plot", height = 300)))),
          shiny::div(class = "card", shiny::h3("Selected-subset summaries"), shiny::tableOutput("filtered_summary"))
        )
      )
    )
  ),
  shiny::tabPanel("Findings & Recommendations", value = "findings",
    shiny::div(class = "page-wrap",
      shiny::h2(class = "section-title", "Evidence-led findings"),
      shiny::uiOutput("findings"),
      shiny::div(class = "card", shiny::h3("Limitations"),
        shiny::tags$ul(shiny::tags$li("Observational data from Portuguese secondary schools; associations cannot establish causal effects."),
          shiny::tags$li("The sample is modest and population-specific; test results from one random split may vary."),
          shiny::tags$li("G1 and G2 are prior grades and create a strong predictive signal, but they are unavailable for very early interventions."),
          shiny::tags$li("G3 = 0 records are retained. Recorded absence patterns among these rows need contextual interpretation."),
          shiny::tags$li("The dataset does not include all potentially relevant influences, and model predictions are not validated intervention decisions.")),
        shiny::h3("Recommendations"),
        shiny::p("Use results to motivate supportive study planning and mentoring—not to label students or infer individual causes. Any intervention should be voluntary, contextual, privacy-aware, and evaluated with appropriate local data before operational use."))
    )
  )
)

server <- function(input, output, session) {
  output$kpis <- shiny::renderUI({
    n0 <- sum(dat$G3 == 0, na.rm = TRUE)
    vals <- c(format(nrow(dat), big.mark = ","), ncol(analysis$raw), sprintf("%.2f", mean(dat$G3, na.rm = TRUE)),
              sprintf("%.1f", stats::median(dat$G3, na.rm = TRUE)), sprintf("%.2f", stats::sd(dat$G3, na.rm = TRUE)),
              sprintf("%d · %.1f%%", n0, 100 * n0 / nrow(dat)))
    labs <- c("Students", "Variables", "Mean G3", "Median G3", "SD of G3", "G3 = 0")
    shiny::div(class = "kpi-grid", lapply(seq_along(vals), function(i)
      shiny::div(class = "kpi", shiny::div(class = "kpi-label", labs[i]), shiny::div(class = "kpi-value", vals[i]))))
  })
  output$grade_plot <- shiny::renderPlot(analysis$figures$grade_distribution)
  output$quality_table <- shiny::renderTable(analysis$quality, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
  output$study_plot <- shiny::renderPlot(analysis$figures$studytime)
  output$study_table <- shiny::renderTable(analysis$by_study, digits = 2, striped = TRUE, bordered = TRUE, rownames = FALSE)
  output$study_interpretation <- shiny::renderUI({
    rho <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "studytime"]
    direction <- if (rho >= 0) "positive" else "negative"
    shiny::p(sprintf("In these %d records, study time has a %s Spearman rank association with G3 (rho = %.2f). The group distributions overlap; category means alone do not show that additional study time causes higher grades.", nrow(dat), direction, rho))
  })
  output$absence_plot <- shiny::renderPlot(analysis$figures$absences)
  output$failure_plot <- shiny::renderPlot(analysis$figures$failures)
  output$attendance_cor <- shiny::renderTable(analysis$correlations[analysis$correlations$Variable %in% c("absences", "failures"), ], digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
  output$attendance_interpretation <- shiny::renderUI({
    rho_a <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "absences"]
    rho_f <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "failures"]
    shiny::p(sprintf("Across all records, absences have Spearman rho = %.2f with G3 and past failures have rho = %.2f. These are population-level rank associations; neither coefficient isolates a causal effect.", rho_a, rho_f))
  })
  output$sensitivity_table <- shiny::renderTable({
    keep <- dat$G3 > 0 & !is.na(dat$G3)
    data.frame(Records_remaining = sum(keep), Records_excluded_G3_zero = sum(!keep),
      Absences_rho_all = cor(dat$absences, dat$G3, method = "spearman", use = "complete.obs"),
      Absences_rho_G3_gt0 = cor(dat$absences[keep], dat$G3[keep], method = "spearman", use = "complete.obs"),
      Zero_grade_records_all_zero_absences = analysis$zero_grade_absences_all_zero,
      Zero_grade_rows_with_zero_absences = sum(dat$absences[dat$G3 == 0] == 0, na.rm = TRUE))
  }, digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
  output$heatmap <- shiny::renderPlot(analysis$figures$heatmap)
  output$cor_table <- shiny::renderTable(analysis$correlations, digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
  output$coef_table <- shiny::renderTable({
    x <- analysis$coefficients[analysis$coefficients$Model == "B", , drop = FALSE]
    x <- x[order(abs(x$Estimate), decreasing = TRUE), , drop = FALSE]
    utils::head(x, 15)
  }, digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
  output$importance_plot <- shiny::renderPlot(analysis$figures$importance)
  output$model_table <- shiny::renderTable(analysis$metrics[, c("Model", "MAE", "RMSE", "R2", "CV_RMSE", "Train_n", "Test_n")], digits = 3, striped = TRUE, bordered = TRUE, rownames = FALSE)
  output$prediction_plot <- shiny::renderPlot(analysis$figures$prediction)
  output$metrics_plot <- shiny::renderPlot({
    m <- analysis$metrics
    plotdata <- rbind(data.frame(Model = m$Model, Metric = "Test RMSE (grade points)", Value = m$RMSE),
                      data.frame(Model = m$Model, Metric = "Test R-squared", Value = m$R2))
    plotdata$Model <- factor(plotdata$Model, levels = rev(m$Model))
    ggplot2::ggplot(plotdata, ggplot2::aes(Value, Model, fill = Metric)) +
      ggplot2::geom_col(show.legend = FALSE) + ggplot2::facet_wrap(~Metric, scales = "free_x", ncol = 2) +
      ggplot2::scale_fill_manual(values = c("Test RMSE (grade points)" = "#0F8B8D", "Test R-squared" = "#0B1F3A")) +
      ggplot2::labs(x = "Metric value (separate scales)", y = NULL) + analysis_theme()
  })
  shiny::observeEvent(input$reset_filters, {
    shiny::updateSelectInput(session, "study_filter", selected = "All")
    shiny::updateSliderInput(session, "failure_filter", value = range(dat$failures, na.rm = TRUE))
    shiny::updateSliderInput(session, "absence_filter", value = range(dat$absences, na.rm = TRUE))
    shiny::updateSliderInput(session, "g1_filter", value = range(dat$G1, na.rm = TRUE))
  })
  selected <- shiny::reactive({
    d <- dat
    if (!is.null(input$study_filter) && input$study_filter != "All") d <- d[d$studytime_f == input$study_filter, , drop = FALSE]
    d <- d[d$failures >= input$failure_filter[1] & d$failures <= input$failure_filter[2], , drop = FALSE]
    d <- d[d$absences >= input$absence_filter[1] & d$absences <= input$absence_filter[2], , drop = FALSE]
    d <- d[d$G1 >= input$g1_filter[1] & d$G1 <= input$g1_filter[2], , drop = FALSE]
    d
  })
  output$selection_kpi <- shiny::renderUI({
    d <- selected()
    shiny::div(class = "kpi-grid", style = "grid-template-columns:repeat(3,minmax(130px,1fr))",
      shiny::div(class = "kpi", shiny::div(class = "kpi-label", "Selected records"), shiny::div(class = "kpi-value", format(nrow(d), big.mark = ","))),
      shiny::div(class = "kpi", shiny::div(class = "kpi-label", "Mean G3"), shiny::div(class = "kpi-value", if (nrow(d)) sprintf("%.2f", mean(d$G3, na.rm = TRUE)) else "—")),
      shiny::div(class = "kpi", shiny::div(class = "kpi-label", "Median G3"), shiny::div(class = "kpi-value", if (nrow(d)) sprintf("%.1f", median(d$G3, na.rm = TRUE)) else "—"))
    )
  })
  output$filtered_grade_plot <- shiny::renderPlot({
    d <- selected(); shiny::validate(shiny::need(nrow(d) > 0, "No records match these filters."))
    ggplot2::ggplot(d, ggplot2::aes(G3)) + ggplot2::geom_histogram(binwidth = 1, boundary = -.5, fill = teal, color = "white") +
      ggplot2::scale_x_continuous(breaks = 0:20, limits = c(-.5, 20.5)) +
      ggplot2::labs(x = "G3 (0–20)", y = "Selected students") + analysis_theme()
  })
  output$filtered_group_plot <- shiny::renderPlot({
    d <- selected(); shiny::validate(shiny::need(nrow(d) > 0, "No records match these filters."))
    ggplot2::ggplot(d, ggplot2::aes(studytime_f, G3)) + ggplot2::geom_boxplot(fill = "#CDE9E9", color = navy) +
      ggplot2::labs(x = "Weekly study time", y = "G3 (0–20)") + analysis_theme()
  })
  output$filtered_summary <- shiny::renderTable({
    d <- selected(); shiny::validate(shiny::need(nrow(d) > 0, "No records match these filters."))
    dplyr::summarise(dplyr::group_by(d, studytime_f), n = dplyr::n(), mean_G3 = mean(G3),
                     median_G3 = median(G3), mean_absences = mean(absences), .groups = "drop")
  }, digits = 2, striped = TRUE, bordered = TRUE, rownames = FALSE)
  output$findings <- shiny::renderUI({
    rho_study <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "studytime"]
    rho_abs <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "absences"]
    rho_fail <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "failures"]
    rho_g1 <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "G1"]
    rho_g2 <- analysis$correlations$Spearman_all[analysis$correlations$Variable == "G2"]
    best <- analysis$metrics[which.min(analysis$metrics$RMSE), , drop = FALSE]
    shiny::tagList(
      shiny::div(class = "card", shiny::h3("1 · Study time and performance"),
        shiny::p(sprintf("Spearman rho is %.2f for study time versus G3. The category summaries and overlapping boxplots should be read as descriptive associations; this design cannot identify the effect of changing study time." , rho_study))),
      shiny::div(class = "card", shiny::h3("2 · Absences and performance"),
        shiny::p(sprintf("Spearman rho is %.2f across the full sample; among the %d records with G3 > 0, it is %.2f. All %d zero-grade records have zero recorded absences: this data feature materially affects interpretation and is not proof that zero grades mean nonattendance.", rho_abs, sum(dat$G3 > 0, na.rm = TRUE), analysis$correlations$Spearman_G3_gt0[analysis$correlations$Variable == "absences"], analysis$zero_grade_n))),
      shiny::div(class = "card", shiny::h3("3 · Strongest associated predictors"),
        shiny::p(sprintf("Prior grades show the largest rank associations with G3 (G1 rho = %.2f; G2 rho = %.2f). Past failures are negatively associated (rho = %.2f). Random-forest split importance is a separate, model-specific measure.", rho_g1, rho_g2, rho_fail))),
      shiny::div(class = "card", shiny::h3("4 · Grade prediction"),
        shiny::p(sprintf("The lowest RMSE on this 80/20 test split is %.2f points for %s (test R² = %.2f, MAE = %.2f). This estimates performance for one split of this dataset; it does not guarantee performance in another school or cohort.", best$RMSE, best$Model, best$R2, best$MAE))),
      shiny::div(class = "callout", shiny::strong("Practical use: "), "Use these patterns to inform supportive mentoring and study-planning conversations. Validate any future intervention locally; do not use predictions as labels or high-stakes decisions.")
    )
  })
}

shiny::shinyApp(ui = ui, server = server)
