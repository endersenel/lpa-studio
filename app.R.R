library(shiny)
library(shinydashboard)
library(haven)
library(readxl)
library(tidyLPA)
library(dplyr)
library(ggplot2)
library(nnet)
library(DT)
library(tidyr)

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
    div(
      style = "padding: 15px; color: #b8c7ce; font-size: 12px; text-align: center;",
      p(strong("Sport and Performance Psychology Lab")),
      p("Open Source Analytics Tools")
    )
  ),
  dashboardBody(
    tabItems(
      # TAB 1: Data Import
      tabItem(tabName = "data_import",
        fluidRow(
          box(title = "Select Data File (.sav, .xlsx, .csv)", width = 6, status = "primary", solidHeader = TRUE,
            fileInput("file", "Upload Dataset", accept = c(".sav", ".xlsx", ".xls", ".csv")),
            helpText("Supported formats: SPSS (.sav), Excel (.xlsx/.xls), or CSV (.csv).")
          ),
          box(title = "Dataset Preview", width = 12, status = "info",
            DTOutput("raw_data_table")
          )
        )
      ),
      
      # TAB 2: LPA Analysis
      tabItem(tabName = "lpa_analysis",
        fluidRow(
          box(title = "LPA Parameters", width = 4, status = "primary", solidHeader = TRUE,
            uiOutput("lpa_var_select"),
            numericInput("max_k", "Maximum Number of Profiles (K):", value = 4, min = 2, max = 8),
            checkboxInput("scale_vars", "Standardize Variables (Z-Score)", value = TRUE),
            actionButton("run_lpa", "Run LPA Analysis", class = "btn-success", icon = icon("play")),
            hr(),
            p(tags$small(em("Methodological Specification: Model 1 (equal variances, covariances fixed to 0) via tidyLPA / mclust.")))
          ),
          box(title = "Profile Plot (Means)", width = 8, status = "info",
            plotOutput("lpa_plot")
          )
        ),
        fluidRow(
          box(title = "Model Fit Indices & Comparative Statistics", width = 12, status = "warning",
            DTOutput("lpa_compare_table"),
            br(),
            p(em("Note: LRT (LMR / BLRT) p-values test whether a K-profile model fits significantly better than a (K-1)-profile model (p < .05)."))
          )
        ),
        fluidRow(
          box(title = "Class Counts & Proportions", width = 6, status = "info",
            DTOutput("lpa_counts_table")
          ),
          box(title = "Average Latent Class Probabilities (Classification Accuracy Matrix)", width = 6, status = "info",
            DTOutput("lpa_prob_matrix_table"),
            helpText("Diagonal values represent classification accuracy for each profile (values > 0.80 indicate strong separation).")
          )
        ),
        fluidRow(
          box(title = "Suggested Methodological Citation for Manuscripts", width = 12, status = "success", solidHeader = TRUE,
            verbatimTextOutput("citation_text"),
            helpText("Directly copy and paste this text into your manuscript's Data Analysis section and References.")
          )
        )
      ),
      
      # TAB 3: Group Comparisons
      tabItem(tabName = "demographics",
        fluidRow(
          box(title = "Comparison Variable & Test Selection", width = 4, status = "primary", solidHeader = TRUE,
            uiOutput("demo_var_select"),
            selectInput("demo_type", "Variable Type / Test:", 
                        choices = c("Continuous (Independent t-Test / ANOVA)" = "continuous", 
                                    "Categorical (Chi-Square Test)" = "categorical")),
            actionButton("run_demo", "Compare Groups", class = "btn-primary")
          ),
          box(title = "Statistical Test Output", width = 8, status = "info",
            verbatimTextOutput("demo_results")
          )
        )
      ),
      
      # TAB 4: Logistic Regression
      tabItem(tabName = "regression",
        fluidRow(
          box(title = "Predict Profile Membership", width = 4, status = "primary", solidHeader = TRUE,
            uiOutput("reg_pred_select"),
            actionButton("run_reg", "Run Multinomial Regression", class = "btn-success")
          ),
          box(title = "Regression Coefficients & Odds Ratios (OR)", width = 8, status = "info",
            verbatimTextOutput("reg_results")
          )
        )
      ),

      # TAB 5: Longitudinal LPTA
      tabItem(tabName = "lpta_module",
        fluidRow(
          box(title = "LPTA Parameters (Time 1 vs. Time 2)", width = 4, status = "primary", solidHeader = TRUE,
            uiOutput("lpta_t1_select"),
            uiOutput("lpta_t2_select"),
            numericInput("lpta_k", "Number of Profiles per Timepoint (K):", value = 3, min = 2, max = 6),
            actionButton("run_lpta", "Run LPTA Transition Analysis", class = "btn-success", icon = icon("random"))
          ),
          box(title = "Profile Transition Matrix (Time 1 -> Time 2)", width = 8, status = "info",
            DTOutput("lpta_transition_table"),
            helpText("Rows represent Time 1 profiles; columns represent Time 2 profiles. Values display counts and row percentages.")
          )
        ),
        fluidRow(
          box(title = "Cross-Time Transition Summary Output", width = 12, status = "warning",
            verbatimTextOutput("lpta_summary_text")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    df <- tryCatch({
      switch(ext,
        sav = {
          data <- haven::read_sav(input$file$datapath)
          data <- haven::zap_labels(data)
          as.data.frame(data)
        },
        xlsx = as.data.frame(readxl::read_excel(input$file$datapath)),
        xls  = as.data.frame(readxl::read_excel(input$file$datapath)),
        csv  = read.csv(input$file$datapath, stringsAsFactors = FALSE),
        stop("Unsupported file format.")
      )
    }, error = function(e) {
      showNotification(paste("File reading error:", e$message), type = "error")
      return(NULL)
    })
    
    return(df)
  })
  
  output$raw_data_table <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$lpa_var_select <- renderUI({
    req(raw_data())
    selectInput("lpa_vars", "Select LPA Indicator Variables (at least 2):", 
                choices = names(raw_data()), multiple = TRUE)
  })
  
  lpa_res <- eventReactive(input$run_lpa, {
    req(input$lpa_vars)
    validate(
      need(length(input$lpa_vars) >= 2, "Please select at least 2 indicator variables for LPA.")
    )
    
    df_working <- raw_data()
    for(col in input$lpa_vars) {
      df_working[[col]] <- as.numeric(df_working[[col]])
    }
    
    complete_idx <- complete.cases(df_working[, input$lpa_vars, drop = FALSE])
    df_clean <- df_working[complete_idx, , drop = FALSE]
    
    lpa_input <- df_clean[, input$lpa_vars, drop = FALSE]
    
    models_enum <- lpa_input %>%
      single_imputation() %>%
      estimate_profiles(1:input$max_k)
    
    fit_table <- get_fit(models_enum)
    
    selected_model <- models_enum[[input$max_k]]
    class_preds <- get_data(selected_model)$Class
    df_clean$LPA_Class <- as.factor(class_preds)
    
    list(models = models_enum, selected_model = selected_model, data_with_class = df_clean, fit_table = fit_table)
  })
  
  output$lpa_plot <- renderPlot({
    req(lpa_res())
    df <- lpa_res()$data_with_class
    vars <- input$lpa_vars
    
    plot_df <- df
    if(input$scale_vars) {
      plot_df[vars] <- lapply(plot_df[vars], function(x) as.numeric(scale(x)))
    }
    
    profile_means <- plot_df %>%
      group_by(LPA_Class) %>%
      summarise(across(all_of(vars), mean, .names = "{.col}")) %>%
      pivot_longer(cols = -LPA_Class, names_to = "Variable", values_to = "Mean")
    
    ggplot(profile_means, aes(x = Variable, y = Mean, group = LPA_Class, color = LPA_Class)) +
      geom_line(size = 1.2) +
      geom_point(size = 3.5) +
      theme_minimal(base_size = 14) +
      labs(title = paste(input$max_k, "Profile Model Plot"),
           x = "Indicator Variables", 
           y = ifelse(input$scale_vars, "Z-Score Mean", "Raw Mean"),
           color = "Profile") +
      theme(legend.position = "bottom")
  })
  
  output$lpa_compare_table <- renderDT({
    req(lpa_res())
    fit_df <- lpa_res()$fit_table
    
    fit_df$Model <- paste(fit_df$classes_number, "Profile(s)")
    
    cols_to_keep <- c("Model", "AIC", "BIC", "SABIC", "Entropy", "lmrt_p", "blrt_p", "n_min", "n_max", "prob_min", "prob_max")
    cols_present <- intersect(cols_to_keep, names(fit_df))
    
    fit_df_sub <- fit_df[, cols_present, drop = FALSE]
    
    colnames(fit_df_sub) <- gsub("^lmrt_p$", "LMR-LRT p", colnames(fit_df_sub))
    colnames(fit_df_sub) <- gsub("^blrt_p$", "BLRT p", colnames(fit_df_sub))
    colnames(fit_df_sub) <- gsub("^n_min$", "Min Class Prop", colnames(fit_df_sub))
    colnames(fit_df_sub) <- gsub("^n_max$", "Max Class Prop", colnames(fit_df_sub))
    colnames(fit_df_sub) <- gsub("^prob_min$", "Min Prob", colnames(fit_df_sub))
    colnames(fit_df_sub) <- gsub("^prob_max$", "Max Prob", colnames(fit_df_sub))
    
    datatable(fit_df_sub, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatRound(columns = setdiff(names(fit_df_sub), "Model"), digits = 3)
  })
  
  output$lpa_counts_table <- renderDT({
    req(lpa_res())
    df <- lpa_res()$data_with_class
    
    counts_df <- df %>%
      group_by(LPA_Class) %>%
      summarise(
        Count = n(),
        Proportion = paste0(round((n() / nrow(df)) * 100, 2), " %")
      ) %>%
      rename(Profile = LPA_Class, `Count (n)` = Count, `Proportion (%)` = Proportion)
    
    datatable(counts_df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$lpa_prob_matrix_table <- renderDT({
    req(lpa_res())
    selected_m <- lpa_res()$selected_model
    data_preds <- get_data(selected_m)
    
    prob_cols <- grep("^Cprob", names(data_preds), value = TRUE)
    
    if(length(prob_cols) > 0) {
      prob_matrix <- data_preds %>%
        group_by(Class) %>%
        summarise(across(all_of(prob_cols), mean, .names = "{.col}"))
      
      colnames(prob_matrix) <- gsub("^Cprob", "Assigned Profile ", colnames(prob_matrix))
      colnames(prob_matrix)[1] <- "Profile"
      
      datatable(prob_matrix, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
        formatRound(columns = -1, digits = 3)
    } else {
      datatable(data.frame(Message = "Probability matrix not available for this model."), options = list(dom = 't'))
    }
  })
  
  output$citation_text <- renderText({
    paste0(
      "--- IN-TEXT METHODOLOGY STATEMENT ---\n",
      "Latent Profile Analysis (LPA) was conducted using LPA Studio (v1.0; Senel, 2026), ",
      "an open-source web application developed by the Sport and Performance Psychology Lab (SPPLab). ",
      "The tool utilizes the R package 'tidyLPA' (Rosenberg et al., 2018) powered by 'mclust' (Scrucca et al., 2016). ",
      "Models were estimated assuming equal indicator variances across profiles and covariances constrained to zero (Model 1 specification). ",
      "Model fit was evaluated using AIC, BIC, SABIC, Entropy, Likelihood Ratio Tests (LMR-LRT and BLRT), and average posterior probabilities.\n\n",
      "--- SUGGESTED REFERENCE (APA 7) ---\n",
      "Senel, E. (2026). LPA Studio: Open source web application for Latent Profile Analysis (v1.0) [Computer software]. Sport and Performance Psychology Lab (SPPLab). https://spplab.shinyapps.io/lpa-studio/"
    )
  })
  
  output$demo_var_select <- renderUI({
    req(lpa_res())
    selectInput("demo_var", "Select Comparison Variable:", 
                choices = setdiff(names(lpa_res()$data_with_class), "LPA_Class"))
  })
  
  output$reg_pred_select <- renderUI({
    req(lpa_res())
    selectInput("reg_preds", "Select Predictor Variables:", 
                choices = setdiff(names(lpa_res()$data_with_class), "LPA_Class"), multiple = TRUE)
  })
  
  observeEvent(input$run_demo, {
    output$demo_results <- renderPrint({
      req(lpa_res(), input$demo_var)
      df <- lpa_res()$data_with_class
      num_classes <- length(levels(df$LPA_Class))
      
      if (input$demo_type == "continuous") {
        formula <- as.formula(paste(input$demo_var, "~ LPA_Class"))
        if (num_classes == 2) {
          cat("=== INDEPENDENT SAMPLES t-TEST ===\n\n")
          print(t.test(formula, data = df, var.equal = TRUE))
        } else {
          cat("=== ONE-WAY ANOVA ===\n\n")
          aov_fit <- aov(formula, data = df)
          print(summary(aov_fit))
          cat("\n=== POST-HOC COMPARISONS (TUKEY HSD) ===\n")
          print(TukeyHSD(aov_fit))
        }
      } else {
        cat("=== CHI-SQUARE TEST OF INDEPENDENCE ===\n\n")
        tbl <- table(df[[input$demo_var]], df$LPA_Class)
        print(tbl)
        print(chisq.test(tbl))
      }
    })
  })
  
  observeEvent(input$run_reg, {
    output$reg_results <- renderPrint({
      req(lpa_res(), input$reg_preds)
      df <- lpa_res()$data_with_class
      formula_str <- paste("LPA_Class ~", paste(input$reg_preds, collapse = " + "))
      model <- multinom(as.formula(formula_str), data = df, trace = FALSE)
      
      cat("=== MULTINOMIAL LOGISTIC REGRESSION SUMMARY ===\n\n")
      print(summary(model))
      cat("\n=== ODDS RATIOS (OR / Exp(B)) ===\n\n")
      print(exp(coef(model)))
    })
  })

  # MODÜL 5: LONGITUDINAL LPTA SERVER MANTIĞI
  output$lpta_t1_select <- renderUI({
    req(raw_data())
    selectInput("lpta_t1_vars", "Time 1 Indicator Variables (at least 2):", 
                choices = names(raw_data()), multiple = TRUE)
  })

  output$lpta_t2_select <- renderUI({
    req(raw_data())
    selectInput("lpta_t2_vars", "Time 2 Indicator Variables (at least 2):", 
                choices = names(raw_data()), multiple = TRUE)
  })

  lpta_res <- eventReactive(input$run_lpta, {
    req(input$lpta_t1_vars, input$lpta_t2_vars)
    validate(
      need(length(input$lpta_t1_vars) >= 2, "Please select at least 2 variables for Time 1."),
      need(length(input$lpta_t2_vars) >= 2, "Please select at least 2 variables for Time 2.")
    )

    df_working <- raw_data()
    all_vars <- unique(c(input$lpta_t1_vars, input$lpta_t2_vars))

    for(col in all_vars) {
      df_working[[col]] <- as.numeric(df_working[[col]])
    }

    complete_idx <- complete.cases(df_working[, all_vars, drop = FALSE])
    df_clean <- df_working[complete_idx, , drop = FALSE]

    # Time 1 Model Estimation
    m1 <- df_clean[, input$lpta_t1_vars, drop = FALSE] %>%
      single_imputation() %>%
      estimate_profiles(input$lpta_k)
    class_t1 <- get_data(m1)$Class

    # Time 2 Model Estimation
    m2 <- df_clean[, input$lpta_t2_vars, drop = FALSE] %>%
      single_imputation() %>%
      estimate_profiles(input$lpta_k)
    class_t2 <- get_data(m2)$Class

    df_clean$Profile_T1 <- factor(paste("T1 - Profile", class_t1))
    df_clean$Profile_T2 <- factor(paste("T2 - Profile", class_t2))

    list(data = df_clean, model_t1 = m1, model_t2 = m2)
  })

  output$lpta_transition_table <- renderDT({
    req(lpta_res())
    df <- lpta_res()$data
    
    tbl_counts <- table(df$Profile_T1, df$Profile_T2)
    tbl_props <- prop.table(tbl_counts, margin = 1) * 100

    formatted_mat <- matrix("", nrow = nrow(tbl_counts), ncol = ncol(tbl_counts))
    rownames(formatted_mat) <- rownames(tbl_counts)
    colnames(formatted_mat) <- colnames(tbl_counts)

    for(r in 1:nrow(tbl_counts)) {
      for(c in 1:ncol(tbl_counts)) {
        cnt <- tbl_counts[r, c]
        prp <- round(tbl_props[r, c], 1)
        formatted_mat[r, c] <- paste0(cnt, " (", prp, "%)")
      }
    }

    df_trans <- as.data.frame(formatted_mat)
    df_trans <- cbind(`Time 1 Profile` = rownames(df_trans), df_trans)

    datatable(df_trans, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })

  output$lpta_summary_text <- renderPrint({
    req(lpta_res())
    df <- lpta_res()$data
    cat("=== LATENT PROFILE TRANSITION ANALYSIS (LPTA) SUMMARY ===\n\n")
    cat("Total Longitudinal Cases Analyzed (n):", nrow(df), "\n\n")
    cat("Cross-Tabulation Table (Counts):\n")
    print(table(Time_1 = df$Profile_T1, Time_2 = df$Profile_T2))
    cat("\nRow Transition Probabilities (%):\n")
    print(round(prop.table(table(Time_1 = df$Profile_T1, Time_2 = df$Profile_T2), margin = 1) * 100, 2))
  })
}

shinyApp(ui = ui, server = server)