library(shiny)
library(shinydashboard)
library(haven)
library(readxl)
library(mclust)
library(tidyLPA)   # yalnızca calc_lrt (LMR-LRT) için
library(dplyr)
library(ggplot2)
library(nnet)
library(DT)
library(tidyr)

# ---------------------------------------------------------------- yardımcılar
safe_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)))))
}
bt <- function(x) paste0("`", x, "`")

read_any <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  switch(ext,
    sav = as.data.frame(haven::zap_labels(haven::read_sav(path))),
    xlsx = , xls = as.data.frame(readxl::read_excel(path)),
    csv = {
      first <- readLines(path, n = 1, warn = FALSE)
      sep <- if (grepl(";", first, fixed = TRUE)) ";" else if (grepl("\t", first, fixed = TRUE)) "\t" else ","
      dec <- if (sep == ";") "," else "."
      read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE,
               check.names = FALSE, fileEncoding = "UTF-8-BOM")
    },
    stop("Unsupported file format.")
  )
}

# Model 1: esit varyans, kovaryans = 0  ->  mclust "EEI"
fit_eei <- function(X, k) {
  tryCatch(mclust::Mclust(X, G = k, modelNames = "EEI", verbose = FALSE),
           error = function(e) NULL, warning = function(w) {
             tryCatch(suppressWarnings(mclust::Mclust(X, G = k, modelNames = "EEI", verbose = FALSE)),
                      error = function(e) NULL)
           })
}

# Profilleri buyukluge gore (buyuk = 1) yeniden etiketle
classify <- function(m) {
  k <- m$G
  tab <- table(factor(m$classification, levels = seq_len(k)))
  ord <- order(tab, decreasing = TRUE)
  z <- m$z[, ord, drop = FALSE]
  cl <- match(m$classification, ord)
  list(z = z, cl = cl, k = k)
}

avg_pp <- function(z, cl, k) {
  M <- t(sapply(seq_len(k), function(j) {
    if (sum(cl == j) == 0) rep(NA_real_, k) else colMeans(z[cl == j, , drop = FALSE])
  }))
  if (k == 1) M <- matrix(M, 1, 1)
  dimnames(M) <- list(paste("Profile", seq_len(k)), paste("P(Profile", seq_len(k), ")"))
  M
}

entropy_rel <- function(z, k) {
  if (k < 2) return(NA_real_)
  zz <- pmax(z, 1e-12)
  1 - sum(-zz * log(zz)) / (nrow(z) * log(k))
}

lmr_p <- function(n, m0, m1) {
  tryCatch({
    r <- tidyLPA::calc_lrt(n, m0$loglik, m0$df, m0$G, m1$loglik, m1$df, m1$G)
    r <- unlist(r)
    idx <- grep("p", names(r), ignore.case = TRUE)
    if (length(idx) > 0) as.numeric(r[tail(idx, 1)]) else as.numeric(tail(r, 1))
  }, error = function(e) NA_real_)
}

build_fit_table <- function(models, X, run_blrt, nboot) {
  n <- nrow(X); K <- length(models)
  blrt <- rep(NA_real_, K)
  if (run_blrt && K >= 2) {
    bl <- tryCatch(
      mclust::mclustBootstrapLRT(X, modelName = "EEI", nboot = nboot, maxG = K - 1, verbose = FALSE),
      error = function(e) NULL)
    if (!is.null(bl)) {
      pv <- as.numeric(bl$p.value)            # pv[g]: g vs g+1
      for (k in 2:K) if (length(pv) >= k - 1) blrt[k] <- pv[k - 1]
    }
  }
  rows <- lapply(seq_len(K), function(k) {
    m <- models[[k]]
    if (is.null(m)) return(data.frame(Profiles = k))
    cc <- classify(m)
    props <- as.numeric(table(factor(cc$cl, levels = 1:k))) / n
    cnts  <- as.numeric(table(factor(cc$cl, levels = 1:k)))
    pp <- avg_pp(cc$z, cc$cl, k)
    npar <- m$df; ll <- m$loglik
    data.frame(
      Profiles = k, LogLik = ll, Npar = npar,
      AIC = -2 * ll + 2 * npar,
      BIC = -2 * ll + npar * log(n),
      SABIC = -2 * ll + npar * log((n + 2) / 24),
      Entropy = entropy_rel(cc$z, k),
      `LMR-LRT p` = if (k >= 2 && !is.null(models[[k - 1]])) lmr_p(n, models[[k - 1]], m) else NA_real_,
      `BLRT p` = blrt[k],
      `Min Class Prop` = min(props), `Max Class Prop` = max(props),
      `Min n` = min(cnts),
      `Min AvePP` = if (k >= 2) min(diag(pp), na.rm = TRUE) else NA_real_,
      check.names = FALSE
    )
  })
  ft <- dplyr::bind_rows(rows)
  ft$Flag <- ifelse(!is.na(ft$`Min Class Prop`) & (ft$`Min Class Prop` < 0.05 | ft$`Min n` < 20),
                    "Very small class", "")
  ft
}

# ---------------------------------------------------------------------- UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SPPLab - LPA Studio", titleWidth = 280),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("1. Data Import", tabName = "data_import", icon = icon("file-upload")),
      menuItem("2. LPA Analysis", tabName = "lpa_analysis", icon = icon("chart-line")),
      menuItem("3. Group Comparisons", tabName = "demographics", icon = icon("users")),
      menuItem("4. Logistic Regression", tabName = "regression", icon = icon("sliders-h")),
      menuItem("5. Longitudinal LPTA", tabName = "lpta_module", icon = icon("exchange-alt"))
    ),
    hr(),
    div(style = "padding: 15px; color: #b8c7ce; font-size: 12px; text-align: center;",
        p(strong("Sport and Performance Psychology Lab")),
        p("Open Source Analytics Tools"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_import",
        fluidRow(
          box(title = "Select Data File (.sav, .xlsx, .csv)", width = 6, status = "primary", solidHeader = TRUE,
              fileInput("file", "Upload Dataset", accept = c(".sav", ".xlsx", ".xls", ".csv")),
              helpText("Supported formats: SPSS (.sav), Excel (.xlsx/.xls), or CSV (.csv; comma or semicolon separated)."),
              verbatimTextOutput("data_info")),
          box(title = "Dataset Preview", width = 12, status = "info", DTOutput("raw_data_table"))
        )),

      tabItem(tabName = "lpa_analysis",
        fluidRow(
          box(title = "LPA Parameters", width = 4, status = "primary", solidHeader = TRUE,
              uiOutput("lpa_var_select"),
              numericInput("max_k", "Maximum Number of Profiles (K):", value = 4, min = 2, max = 8),
              checkboxInput("scale_vars", "Standardize indicators before estimation (Z-score)", value = TRUE),
              checkboxInput("run_blrt", "Compute BLRT (bootstrap; slower)", value = TRUE),
              numericInput("nboot", "BLRT bootstrap samples:", value = 100, min = 19, max = 1000, step = 10),
              numericInput("seed", "Random seed:", value = 123, min = 1, step = 1),
              actionButton("run_lpa", "Run LPA Analysis", class = "btn-success", icon = icon("play")),
              hr(),
              uiOutput("sel_k_ui"),
              p(tags$small(em("Specification: Model 1 (equal variances, covariances fixed to 0; mclust 'EEI').")))),
          box(title = "Profile Plot (Means ± SE)", width = 8, status = "info", plotOutput("lpa_plot", height = "420px"))
        ),
        fluidRow(
          box(title = "Model Fit Indices & Comparative Statistics", width = 12, status = "warning",
              DTOutput("lpa_compare_table"), br(),
              p(em("Lower AIC/BIC/SABIC indicate better fit. LMR-LRT and BLRT p-values test K profiles against K-1 (p < .05 favors K). Entropy and AvePP > .80 indicate good classification. Check class sizes (very small classes are flagged).")),
              plotOutput("ic_plot", height = "260px"),
              downloadButton("dl_fit", "Download fit table (CSV)"))
        ),
        fluidRow(
          box(title = "Class Counts & Proportions", width = 6, status = "info", DTOutput("lpa_counts_table")),
          box(title = "Average Latent Class Probabilities (Classification Matrix)", width = 6, status = "info",
              DTOutput("lpa_prob_matrix_table"),
              helpText("Rows: most likely assigned profile; columns: mean posterior probability. Diagonal values > 0.80 indicate strong separation."))
        ),
        fluidRow(
          box(title = "Profile Means (Raw Scale) and SDs", width = 12, status = "info",
              DTOutput("raw_means_table"),
              downloadButton("dl_data", "Download data with profile membership (CSV)"))
        ),
        fluidRow(
          box(title = "Suggested Methodological Citation for Manuscripts", width = 12, status = "success", solidHeader = TRUE,
              verbatimTextOutput("citation_text"),
              helpText("Copy this text into your Data Analysis section and References."))
        )),

      tabItem(tabName = "demographics",
        fluidRow(
          box(title = "Comparison Variable & Test Selection", width = 4, status = "primary", solidHeader = TRUE,
              uiOutput("demo_var_select"),
              selectInput("demo_type", "Variable Type / Test:",
                          choices = c("Continuous (t-Test / ANOVA)" = "continuous",
                                      "Categorical (Chi-Square Test)" = "categorical")),
              actionButton("run_demo", "Compare Groups", class = "btn-primary")),
          box(title = "Statistical Test Output", width = 8, status = "info", verbatimTextOutput("demo_results"))
        )),

      tabItem(tabName = "regression",
        fluidRow(
          box(title = "Predict Profile Membership", width = 4, status = "primary", solidHeader = TRUE,
              uiOutput("reg_pred_select"),
              uiOutput("reg_ref_ui"),
              checkboxInput("reg_scale", "Standardize numeric predictors", value = TRUE),
              actionButton("run_reg", "Run Multinomial Regression", class = "btn-success"),
              helpText("Note: this uses most-likely class membership (1-step-removed). Classification error is ignored; for publication consider BCH/R3STEP approaches.")),
          box(title = "Regression Coefficients & Odds Ratios (OR)", width = 8, status = "info",
              verbatimTextOutput("reg_results"), DTOutput("reg_table"))
        )),

      tabItem(tabName = "lpta_module",
        fluidRow(
          box(title = "LPTA Parameters (Time 1 vs. Time 2)", width = 4, status = "primary", solidHeader = TRUE,
              uiOutput("lpta_t1_select"),
              uiOutput("lpta_t2_select"),
              numericInput("lpta_k", "Number of Profiles per Timepoint (K):", value = 3, min = 2, max = 6),
              checkboxInput("lpta_scale", "Standardize indicators (Z-score) within each time point", value = TRUE),
              actionButton("run_lpta", "Run LPTA Transition Analysis", class = "btn-success", icon = icon("random")),
              helpText("Profiles are estimated separately at each time point and ordered by size (largest = Profile 1). Labels at T1 and T2 are not guaranteed to represent the same substantive profile: inspect the plots.")),
          box(title = "Profile Transition Matrix (Time 1 -> Time 2)", width = 8, status = "info",
              DTOutput("lpta_transition_table"),
              helpText("Rows: Time 1 profiles; columns: Time 2 profiles. Counts and row percentages."))
        ),
        fluidRow(
          box(title = "Time 1 Profiles", width = 6, status = "info", plotOutput("lpta_plot1")),
          box(title = "Time 2 Profiles", width = 6, status = "info", plotOutput("lpta_plot2"))
        ),
        fluidRow(
          box(title = "Cross-Time Transition Summary Output", width = 12, status = "warning",
              verbatimTextOutput("lpta_summary_text"))
        ))
    )
  )
)

# ------------------------------------------------------------------- SERVER
server <- function(input, output, session) {

  # ---------------- veri
  raw_data <- reactive({
    req(input$file)
    tryCatch(read_any(input$file$datapath, input$file$name),
             error = function(e) {
               showNotification(paste("File reading error:", e$message), type = "error")
               NULL
             })
  })

  output$raw_data_table <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$data_info <- renderPrint({
    req(raw_data())
    cat("Rows:", nrow(raw_data()), " Columns:", ncol(raw_data()), "\n")
  })

  num_cols <- reactive({
    req(raw_data())
    d <- raw_data()
    names(d)[sapply(d, function(x) {
      v <- safe_num(x); mean(!is.na(v[!is.na(x)])) > 0.9 && sum(!is.na(v)) > 0
    })]
  })

  output$lpa_var_select <- renderUI({
    req(num_cols())
    selectInput("lpa_vars", "Select LPA Indicator Variables (at least 2):",
                choices = num_cols(), multiple = TRUE)
  })

  # ---------------- LPA
  lpa_res <- eventReactive(input$run_lpa, {
    vars <- input$lpa_vars
    validate(need(length(vars) >= 2, "Please select at least 2 indicator variables."),
             need(!is.na(input$max_k) && input$max_k >= 2, "Maximum K must be at least 2."))
    df <- raw_data()
    X0 <- as.data.frame(lapply(df[vars], safe_num)); names(X0) <- vars
    ok <- complete.cases(X0)
    validate(need(sum(ok) >= 30, "Fewer than 30 complete cases; LPA is not reliable."))
    X0 <- X0[ok, , drop = FALSE]
    df_clean <- df[ok, , drop = FALSE]
    df_clean[vars] <- X0
    df_clean$Row_ID <- which(ok)
    validate(need(all(sapply(X0, sd) > 0), "At least one indicator has zero variance."))
    X <- if (input$scale_vars) as.data.frame(scale(X0)) else X0
    K <- as.integer(input$max_k)

    set.seed(input$seed)
    models <- vector("list", K)
    withProgress(message = "Fitting LPA models...", value = 0, {
      for (k in seq_len(K)) { models[[k]] <- fit_eei(X, k); incProgress(0.5 / K) }
      setProgress(message = if (input$run_blrt) "Bootstrap LRT (may take a while)..." else "Finishing...")
      ft <- build_fit_table(models, X, isTRUE(input$run_blrt), input$nboot)
      setProgress(value = 1)
    })
    list(models = models, X = X, X0 = X0, vars = vars, df_clean = df_clean, fit = ft,
         n_total = nrow(df), n_used = sum(ok), scaled = isTRUE(input$scale_vars))
  })

  output$sel_k_ui <- renderUI({
    req(lpa_res())
    ft <- lpa_res()$fit
    ft2 <- ft[ft$Profiles >= 2 & !is.na(ft$BIC), , drop = FALSE]
    req(nrow(ft2) > 0)
    selectInput("sel_k", "Profile solution used in plots, tables, and Tabs 3-4:",
                choices = ft2$Profiles, selected = ft2$Profiles[which.min(ft2$BIC)])
  })

  lpa_final <- reactive({
    r <- lpa_res(); req(input$sel_k)
    k <- as.integer(input$sel_k)
    m <- r$models[[k]]
    validate(need(!is.null(m), "Selected model failed to converge."))
    cc <- classify(m)
    df <- r$df_clean
    df$LPA_Class <- factor(cc$cl, levels = seq_len(k))
    list(k = k, model = m, cc = cc, data = df, X = r$X, X0 = r$X0, vars = r$vars, scaled = r$scaled)
  })

  output$lpa_plot <- renderPlot({
    f <- lpa_final()
    d <- f$X; d$LPA_Class <- f$data$LPA_Class
    n_by <- table(d$LPA_Class)
    pm <- d %>%
      pivot_longer(cols = all_of(f$vars), names_to = "Variable", values_to = "Value") %>%
      group_by(LPA_Class, Variable) %>%
      summarise(Mean = mean(Value), SE = sd(Value) / sqrt(n()), .groups = "drop") %>%
      mutate(Variable = factor(Variable, levels = f$vars),
             Profile = factor(paste0("Profile ", LPA_Class, " (n=", as.integer(n_by[as.character(LPA_Class)]), ")")))
    ggplot(pm, aes(x = Variable, y = Mean, group = Profile, color = Profile)) +
      geom_line(linewidth = 1.2) + geom_point(size = 3.5) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.1, linewidth = 0.6) +
      theme_minimal(base_size = 14) +
      labs(title = paste(f$k, "Profile Model"), x = "Indicator Variables",
           y = ifelse(f$scaled, "Z-Score Mean", "Raw Mean"), color = NULL) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 25, hjust = 1))
  })

  output$lpa_compare_table <- renderDT({
    ft <- lpa_res()$fit
    datatable(ft, options = list(dom = "t", ordering = FALSE, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = intersect(c("LogLik", "AIC", "BIC", "SABIC"), names(ft)), digits = 2) %>%
      formatRound(columns = intersect(c("Entropy", "LMR-LRT p", "BLRT p", "Min Class Prop", "Max Class Prop", "Min AvePP"), names(ft)), digits = 3)
  })

  output$ic_plot <- renderPlot({
    ft <- lpa_res()$fit
    d <- ft %>% select(Profiles, AIC, BIC, SABIC) %>%
      pivot_longer(-Profiles, names_to = "Index", values_to = "Value")
    ggplot(d, aes(Profiles, Value, color = Index)) +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_x_continuous(breaks = ft$Profiles) +
      theme_minimal(base_size = 13) + labs(y = "Information criterion", title = "Information criteria by number of profiles") +
      theme(legend.position = "bottom")
  })

  output$dl_fit <- downloadHandler(
    filename = function() "lpa_fit_table.csv",
    content = function(file) write.csv(lpa_res()$fit, file, row.names = FALSE))

  output$lpa_counts_table <- renderDT({
    f <- lpa_final()
    tb <- table(f$data$LPA_Class)
    d <- data.frame(Profile = names(tb), `Count (n)` = as.integer(tb),
                    `Proportion (%)` = round(100 * as.integer(tb) / sum(tb), 2), check.names = FALSE)
    datatable(d, options = list(dom = "t", ordering = FALSE), rownames = FALSE)
  })

  output$lpa_prob_matrix_table <- renderDT({
    f <- lpa_final()
    M <- avg_pp(f$cc$z, f$cc$cl, f$k)
    d <- data.frame(Profile = rownames(M), round(M, 3), check.names = FALSE)
    datatable(d, options = list(dom = "t", ordering = FALSE), rownames = FALSE)
  })

  output$raw_means_table <- renderDT({
    f <- lpa_final()
    d <- f$data
    res <- do.call(rbind, lapply(f$vars, function(v) {
      do.call(rbind, lapply(levels(d$LPA_Class), function(cl) {
        x <- d[[v]][d$LPA_Class == cl]
        data.frame(Variable = v, Profile = cl, n = length(x),
                   Mean = round(mean(x), 3), SD = round(sd(x), 3))
      }))
    }))
    datatable(res, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_data <- downloadHandler(
    filename = function() "lpa_data_with_profiles.csv",
    content = function(file) {
      f <- lpa_final()
      out <- f$data
      pp <- as.data.frame(round(f$cc$z, 4)); names(pp) <- paste0("Prob_Profile", seq_len(f$k))
      write.csv(cbind(out, pp), file, row.names = FALSE)
    })

  output$citation_text <- renderText({
    paste0(
      "--- IN-TEXT METHODOLOGY STATEMENT ---\n",
      "Latent Profile Analysis (LPA) was conducted using LPA Studio (v1.0; Senel, 2026), ",
      "an open-source web application developed by the Sport and Performance Psychology Lab (SPPLab). ",
      "Gaussian finite mixture models were estimated with the R package 'mclust' (Scrucca et al., 2016; v",
      as.character(packageVersion("mclust")), "), and the Lo-Mendell-Rubin test was computed via 'tidyLPA' (Rosenberg et al., 2018). ",
      "Models were estimated assuming equal indicator variances across profiles and covariances constrained to zero (Model 1 specification). ",
      "Solutions with 1 to K profiles were compared using AIC, BIC, SABIC, relative entropy, the Lo-Mendell-Rubin adjusted likelihood ratio test (LMR-LRT), ",
      "the bootstrap likelihood ratio test (BLRT), average posterior probabilities, and the size and interpretability of the profiles (Nylund et al., 2007).\n\n",
      "--- SUGGESTED REFERENCES (APA 7) ---\n",
      "Senel, E. (2026). LPA Studio: Open source web application for Latent Profile Analysis (v1.0) [Computer software]. Sport and Performance Psychology Lab (SPPLab). https://spplab.shinyapps.io/lpa-studio/\n",
      "Lo, Y., Mendell, N. R., & Rubin, D. B. (2001). Testing the number of components in a normal mixture. Biometrika, 88(3), 767-778.\n",
      "Nylund, K. L., Asparouhov, T., & Muthen, B. O. (2007). Deciding on the number of classes in latent class analysis and growth mixture modeling. Structural Equation Modeling, 14(4), 535-569.\n",
      "Rosenberg, J. M., Beymer, P. N., Anderson, D. J., Van Lissa, C. J., & Schmidt, J. A. (2018). tidyLPA: An R package to easily carry out Latent Profile Analysis (LPA) using open-source or commercial software. Journal of Open Source Software, 3(30), 978.\n",
      "Scrucca, L., Fop, M., Murphy, T. B., & Raftery, A. E. (2016). mclust 5: Clustering, classification and density estimation using Gaussian finite mixture models. The R Journal, 8(1), 289-317."
    )
  })

  # ---------------- Grup karsilastirmalari
  output$demo_var_select <- renderUI({
    f <- lpa_final()
    selectInput("demo_var", "Select Comparison Variable:",
                choices = setdiff(names(f$data), c("LPA_Class", "Row_ID")))
  })

  output$demo_results <- renderPrint({
    req(input$demo_var)
    f <- lpa_final(); df <- f$data; v <- input$demo_var
    ncl <- nlevels(df$LPA_Class)

    if (input$demo_type == "continuous") {
      df$.y <- safe_num(df[[v]])
      validate(need(sum(!is.na(df$.y)) > 5, "Selected variable is not numeric."))
      d <- df[!is.na(df$.y), ]
      cat("=== DESCRIPTIVES BY PROFILE ===\n")
      print(d %>% group_by(Profile = LPA_Class) %>%
              summarise(n = n(), Mean = round(mean(.y), 3), SD = round(sd(.y), 3), .groups = "drop") %>%
              as.data.frame())
      if (ncl == 2) {
        cat("\n=== WELCH t-TEST ===\n")
        tt <- t.test(.y ~ LPA_Class, data = d)
        print(tt)
        g <- split(d$.y, d$LPA_Class)
        sp <- sqrt(((length(g[[1]]) - 1) * var(g[[1]]) + (length(g[[2]]) - 1) * var(g[[2]])) /
                     (length(g[[1]]) + length(g[[2]]) - 2))
        cat("Cohen's d =", round((mean(g[[1]]) - mean(g[[2]])) / sp, 3), "\n")
      } else {
        aov_fit <- aov(.y ~ LPA_Class, data = d)
        ss <- summary(aov_fit)[[1]]
        cat("\n=== HOMOGENEITY OF VARIANCE (Bartlett) ===\n")
        print(bartlett.test(.y ~ LPA_Class, data = d))
        cat("\n=== ONE-WAY ANOVA ===\n")
        print(summary(aov_fit))
        cat("Eta squared =", round(ss[["Sum Sq"]][1] / sum(ss[["Sum Sq"]]), 3), "\n")
        cat("\n=== WELCH ANOVA (robust to unequal variances) ===\n")
        print(oneway.test(.y ~ LPA_Class, data = d, var.equal = FALSE))
        cat("\n=== POST-HOC COMPARISONS (TUKEY HSD) ===\n")
        print(TukeyHSD(aov_fit))
        cat("\n=== KRUSKAL-WALLIS (non-parametric check) ===\n")
        print(kruskal.test(.y ~ LPA_Class, data = d))
      }
    } else {
      tbl <- table(df[[v]], df$LPA_Class, dnn = c(v, "Profile"))
      cat("=== CROSS-TABULATION (counts) ===\n"); print(tbl)
      cat("\n=== COLUMN PERCENTAGES (within profile) ===\n")
      print(round(prop.table(tbl, 2) * 100, 1))
      suppressWarnings(ct <- chisq.test(tbl))
      cat("\n=== CHI-SQUARE TEST OF INDEPENDENCE ===\n")
      if (any(ct$expected < 5)) {
        cat("Warning: some expected counts < 5; Monte-Carlo p-value reported.\n")
        set.seed(1); ct <- chisq.test(tbl, simulate.p.value = TRUE, B = 5000)
      }
      print(ct)
      cat("Cramer's V =", round(sqrt(unname(ct$statistic) / (sum(tbl) * (min(dim(tbl)) - 1))), 3), "\n")
      cat("\nStandardized residuals (|value| > 1.96 notable):\n")
      suppressWarnings(print(round(chisq.test(tbl)$stdres, 2)))
    }
  }) |> bindEvent(input$run_demo)

  # ---------------- Regresyon
  output$reg_pred_select <- renderUI({
    f <- lpa_final()
    selectInput("reg_preds", "Select Predictor Variables:",
                choices = setdiff(names(f$data), c("LPA_Class", "Row_ID")), multiple = TRUE)
  })

  output$reg_ref_ui <- renderUI({
    f <- lpa_final()
    selectInput("reg_ref", "Reference Profile:", choices = levels(f$data$LPA_Class), selected = "1")
  })

  reg_fit <- eventReactive(input$run_reg, {
    req(input$reg_preds, input$reg_ref)
    f <- lpa_final()
    d <- f$data[, c("LPA_Class", input$reg_preds), drop = FALSE]
    for (p in input$reg_preds) {
      x <- d[[p]]
      v <- safe_num(x)
      if (!is.numeric(x) && mean(!is.na(v[!is.na(x)])) > 0.9) x <- v
      if (is.numeric(x)) { if (isTRUE(input$reg_scale)) x <- as.numeric(scale(x)) }
      else x <- factor(x)
      d[[p]] <- x
    }
    d <- d[complete.cases(d), , drop = FALSE]
    d$LPA_Class <- relevel(factor(d$LPA_Class), ref = input$reg_ref)
    validate(need(nrow(d) > 10 * length(input$reg_preds), "Too few cases for this many predictors."))
    fml <- as.formula(paste("LPA_Class ~", paste(bt(input$reg_preds), collapse = " + ")))
    m <- nnet::multinom(fml, data = d, trace = FALSE, Hess = TRUE, MaxNWts = 5000)
    m0 <- nnet::multinom(LPA_Class ~ 1, data = d, trace = FALSE)
    list(m = m, m0 = m0, d = d, ref = input$reg_ref)
  })

  reg_tbl <- reactive({
    r <- reg_fit(); m <- r$m
    cf <- coef(m); se <- summary(m)$standard.errors
    others <- setdiff(levels(r$d$LPA_Class), r$ref)
    if (is.null(dim(cf))) {
      cf <- matrix(cf, nrow = 1, dimnames = list(others, names(coef(m))))
      se <- matrix(se, nrow = 1, dimnames = dimnames(cf))
    }
    out <- do.call(rbind, lapply(rownames(cf), function(cl) {
      b <- cf[cl, ]; s <- se[cl, ]; z <- b / s; p <- 2 * pnorm(-abs(z))
      data.frame(Profile = paste0(cl, " vs ", r$ref), Term = colnames(cf), B = b, SE = s, z = z, p = p,
                 OR = exp(b), `OR 95% LCI` = exp(b - 1.96 * s), `OR 95% UCI` = exp(b + 1.96 * s),
                 check.names = FALSE, row.names = NULL)
    }))
    out
  })

  output$reg_results <- renderPrint({
    r <- reg_fit()
    cat("=== MULTINOMIAL LOGISTIC REGRESSION ===\n")
    cat("Reference profile:", r$ref, "| n =", nrow(r$d), "\n\n")
    ll1 <- as.numeric(logLik(r$m)); ll0 <- as.numeric(logLik(r$m0))
    cat("Model LR chi-square:", round(2 * (ll1 - ll0), 3),
        "| df =", attr(logLik(r$m), "df") - attr(logLik(r$m0), "df"),
        "| p =", signif(pchisq(2 * (ll1 - ll0), attr(logLik(r$m), "df") - attr(logLik(r$m0), "df"), lower.tail = FALSE), 3), "\n")
    cat("McFadden pseudo-R2:", round(1 - ll1 / ll0, 3), "\n")
    cat("AIC:", round(AIC(r$m), 2), "\n\n")
    cat("Coefficients (B), SE, z, p, Odds Ratios and 95% CI are in the table below.\n")
    cat("Intercept rows are included; interpret OR for predictors only.\n")
  })

  output$reg_table <- renderDT({
    d <- reg_tbl()
    datatable(d, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = c("B", "SE", "z", "OR", "OR 95% LCI", "OR 95% UCI"), digits = 3) %>%
      formatSignif(columns = "p", digits = 3)
  })

  # ---------------- LPTA
  output$lpta_t1_select <- renderUI({
    req(num_cols())
    selectInput("lpta_t1_vars", "Time 1 Indicator Variables (at least 2):", choices = num_cols(), multiple = TRUE)
  })
  output$lpta_t2_select <- renderUI({
    req(num_cols())
    selectInput("lpta_t2_vars", "Time 2 Indicator Variables (at least 2):", choices = num_cols(), multiple = TRUE)
  })

  lpta_res <- eventReactive(input$run_lpta, {
    v1 <- input$lpta_t1_vars; v2 <- input$lpta_t2_vars
    validate(need(length(v1) >= 2, "Please select at least 2 variables for Time 1."),
             need(length(v2) >= 2, "Please select at least 2 variables for Time 2."))
    df <- raw_data()
    allv <- unique(c(v1, v2))
    X0 <- as.data.frame(lapply(df[allv], safe_num)); names(X0) <- allv
    ok <- complete.cases(X0)
    validate(need(sum(ok) >= 30, "Fewer than 30 complete longitudinal cases."))
    X0 <- X0[ok, , drop = FALSE]
    df_clean <- df[ok, , drop = FALSE]; df_clean[allv] <- X0
    df_clean$Row_ID <- which(ok)
    k <- as.integer(input$lpta_k)
    prep <- function(v) { x <- X0[, v, drop = FALSE]; if (isTRUE(input$lpta_scale)) as.data.frame(scale(x)) else x }
    X1 <- prep(v1); X2 <- prep(v2)
    set.seed(123)
    m1 <- fit_eei(X1, k); m2 <- fit_eei(X2, k)
    validate(need(!is.null(m1) && !is.null(m2), "A model failed to converge; try a smaller K."))
    c1 <- classify(m1); c2 <- classify(m2)
    df_clean$Profile_T1 <- factor(paste("T1 - Profile", c1$cl), levels = paste("T1 - Profile", 1:k))
    df_clean$Profile_T2 <- factor(paste("T2 - Profile", c2$cl), levels = paste("T2 - Profile", 1:k))
    list(data = df_clean, X1 = X1, X2 = X2, v1 = v1, v2 = v2, k = k,
         ent1 = entropy_rel(c1$z, k), ent2 = entropy_rel(c2$z, k),
         scaled = isTRUE(input$lpta_scale))
  })

  lpta_plot_fn <- function(X, vars, cls, scaled, title) {
    d <- X; d$cls <- cls
    pm <- d %>% pivot_longer(all_of(vars), names_to = "Variable", values_to = "Value") %>%
      group_by(cls, Variable) %>%
      summarise(Mean = mean(Value), SE = sd(Value) / sqrt(n()), .groups = "drop") %>%
      mutate(Variable = factor(Variable, levels = vars))
    ggplot(pm, aes(Variable, Mean, group = cls, color = cls)) +
      geom_line(linewidth = 1.1) + geom_point(size = 3) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.1) +
      theme_minimal(base_size = 13) +
      labs(title = title, x = NULL, y = ifelse(scaled, "Z-Score Mean", "Raw Mean"), color = NULL) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 25, hjust = 1))
  }
  output$lpta_plot1 <- renderPlot({ r <- lpta_res(); lpta_plot_fn(r$X1, r$v1, r$data$Profile_T1, r$scaled, "Time 1") })
  output$lpta_plot2 <- renderPlot({ r <- lpta_res(); lpta_plot_fn(r$X2, r$v2, r$data$Profile_T2, r$scaled, "Time 2") })

  output$lpta_transition_table <- renderDT({
    df <- lpta_res()$data
    cnt <- table(df$Profile_T1, df$Profile_T2)
    prp <- prop.table(cnt, 1) * 100
    mat <- matrix("", nrow(cnt), ncol(cnt), dimnames = dimnames(cnt))
    for (i in seq_len(nrow(cnt))) for (j in seq_len(ncol(cnt)))
      mat[i, j] <- paste0(cnt[i, j], " (", ifelse(is.nan(prp[i, j]), 0, round(prp[i, j], 1)), "%)")
    d <- cbind(`Time 1 Profile` = rownames(mat), as.data.frame(mat, check.names = FALSE))
    datatable(d, options = list(dom = "t", ordering = FALSE), rownames = FALSE)
  })

  output$lpta_summary_text <- renderPrint({
    r <- lpta_res(); df <- r$data
    cnt <- table(Time_1 = df$Profile_T1, Time_2 = df$Profile_T2)
    cat("=== LATENT PROFILE TRANSITION ANALYSIS (LPTA) SUMMARY ===\n\n")
    cat("Longitudinal complete cases (n):", nrow(df), "\n")
    cat("Relative entropy T1:", round(r$ent1, 3), "| T2:", round(r$ent2, 3), "\n\n")
    cat("Cross-Tabulation (Counts):\n"); print(cnt)
    cat("\nRow Transition Probabilities (%):\n"); print(round(prop.table(cnt, 1) * 100, 2))
    suppressWarnings(ct <- chisq.test(cnt))
    if (any(ct$expected < 5)) { set.seed(1); ct <- chisq.test(cnt, simulate.p.value = TRUE, B = 5000) }
    cat("\nChi-square test of association between T1 and T2 profiles:\n"); print(ct)
    cat("Cramer's V =", round(sqrt(unname(ct$statistic) / (sum(cnt) * (min(dim(cnt)) - 1))), 3), "\n")
    # Ayni etiketli (T1-i -> T2-i) kalis orani ve Cohen kappa (etiketler karsilik geliyorsa anlamli)
    po <- sum(diag(cnt)) / sum(cnt)
    pe <- sum(rowSums(cnt) * colSums(cnt)) / sum(cnt)^2
    cat("\nStay rate if labels correspond (diagonal): ", round(100 * po, 1), "%\n", sep = "")
    cat("Cohen's kappa (diagonal agreement): ", round((po - pe) / (1 - pe), 3), "\n", sep = "")
    cat("Note: diagonal-based indices assume Profile i at T1 is substantively the same as Profile i at T2. Verify with the plots.\n")
  })
}

shinyApp(ui = ui, server = server)
