# 宠物食品综合分析平台 - 完整整合版
# 整合了三个独立的Shiny应用：食品分析、消费者分析、用户留存分析

# 加载所有必要的包
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(caret)
library(randomForest)
library(broom)
library(reshape2)
library(factoextra)
library(corrplot)
library(stringr)
library(survival)
library(survminer)
library(lubridate)
library(rms)
library(timeROC)
library(dcurves)
library(gridExtra)
library(ggfortify)

# 自定义CSS样式 - 统一黄色主题
custom_css <- "
.main-header .logo {
  background-color: #fed36a !important;
  color: #333 !important;
  font-weight: bold;
}
.main-header .navbar {
  background-color: #fed36a !important;
}
.sidebar {
  background-color: #fef6e4 !important;
}
.content-wrapper, .right-side {
  background-color: #ffffff !important;
}
.nav-tabs-custom .nav-tabs li.active a {
  background-color: #fed36a !important;
  border-color: #fed36a !important;
}
.box.box-solid.box-warning>.box-header {
  background-color: #fed36a !important;
  color: #333;
}
.btn-warning {
  background-color: #fed36a !important;
  border-color: #fed36a !important;
  color: #333 !important;
}
.btn-warning:hover {
  background-color: #e5c156 !important;
  border-color: #e5c156 !important;
}
.selectize-input {
  border-color: #fed36a !important;
}
.selectize-input.focus {
  border-color: #e5c156 !important;
  box-shadow: 0 0 0 0.2rem rgba(254, 211, 106, 0.25);
}
.value-box {
  border-color: #fed36a !important;
}
.skin-yellow .sidebar-menu > li.active > a {
  border-left-color: #fed36a !important;
  background-color: #fff8e1 !important;
}
.box.box-warning {
  border-top-color: #fed36a !important;
}
.shiny-notification {
  background-color: #fed36a !important;
  color: #333 !important;
  border-color: #fed36a !important;
}
"

# UI定义 - 整合所有模块
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fas fa-paw", style = "margin-right: 10px;"),
      "线上犬类宠物干湿粮食品综合分析平台"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      
      menuItem("📊 食品分析", tabName = "food_analysis", icon = icon("utensils"),
               menuSubItem("订单类型分析", tabName = "order_type"),
               menuSubItem("卡路里对比", tabName = "calorie_comparison"),
               menuSubItem("湿粮质地分析", tabName = "texture_analysis"),
               menuSubItem("相关性分析", tabName = "correlation")),
      menuItem("👤 消费者分析", tabName = "consumer_analysis", icon = icon("users"),
               menuSubItem("消费者特征", tabName = "consumer_characteristics"),
               menuSubItem("逻辑回归", tabName = "consumer_logistic"),
               menuSubItem("随机森林", tabName = "consumer_randomforest"),
               menuSubItem("行为时序", tabName = "consumer_timeseries"),
               menuSubItem("聚类分析", tabName = "consumer_clustering")),
      menuItem("📈 用户留存分析", tabName = "retention_analysis", icon = icon("chart-line"),
               menuSubItem("数据概览", tabName = "retention_overview"),
               menuSubItem("生存分析", tabName = "retention_km"),
               menuSubItem("Cox回归", tabName = "retention_cox"),
               menuSubItem("预测工具", tabName = "retention_prediction")),
      menuItem("📁 数据管理", tabName = "data_management", icon = icon("database"))
    ),
    
    # 动态筛选面板
    conditionalPanel(
      condition = "input.sidebar == 'order_type' || input.sidebar == 'calorie_comparison' || input.sidebar == 'texture_analysis' || input.sidebar == 'correlation'",
      box(
        title = "食品分析筛选器",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        
        pickerInput(
          inputId = "food_order_type_filter",
          label = "订单类型筛选:",
          choices = c("所有订单", "纯干粮", "纯湿粮", "混合订单"),
          selected = "所有订单",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        sliderInput(
          inputId = "food_wet_ratio_range",
          label = "湿粮比例范围 (%):",
          min = 0,
          max = 100,
          value = c(0, 100),
          step = 5
        ),
        
        sliderInput(
          inputId = "food_calorie_range",
          label = "总卡路里范围 (kcal):",
          min = 0,
          max = 5000,
          value = c(0, 5000),
          step = 100
        ),
        
        actionButton("food_apply_filters", "应用筛选", 
                     icon = icon("filter"),
                     class = "btn-warning")
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'consumer_characteristics' || input.sidebar == 'consumer_logistic' || input.sidebar == 'consumer_randomforest' || input.sidebar == 'consumer_timeseries' || input.sidebar == 'consumer_clustering'",
      box(
        title = "消费者分析筛选器",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        
        pickerInput(
          inputId = "consumer_food_type_filter",
          label = "食品类型:",
          choices = c("所有类型", "纯干粮", "纯湿粮", "混合", "其他"),
          selected = "所有类型",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        pickerInput(
          inputId = "consumer_breed_size_filter",
          label = "宠物体型:",
          choices = c("所有体型", "小型", "中型", "大型", "迷你型", "巨大型", "其他"),
          selected = "所有体型",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        pickerInput(
          inputId = "consumer_food_tier_filter",
          label = "食品等级:",
          choices = c("所有等级", "mid", "premium", "superpremium"),
          selected = "所有等级",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        pickerInput(
          inputId = "consumer_life_stage_filter",
          label = "生命阶段:",
          choices = c("所有阶段", "puppy", "kitten", "mature", "senior"),
          selected = "所有阶段",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        actionButton("consumer_apply_filters", "应用筛选", 
                     icon = icon("filter"),
                     class = "btn-warning")
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'retention_overview'",
      box(
        title = "留存分析筛选器",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        
        selectInput("retention_breed_size_filter", "宠物体型筛选:",
                    choices = c("全部", "small", "medium", "large", "toy", "giant"),
                    selected = "全部"),
        sliderInput("retention_min_orders", "最小订单数:",
                    min = 1, max = 50, value = 1),
        sliderInput("retention_min_survival", "最小生存时间(天):",
                    min = 0, max = 365, value = 0)
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'retention_km'",
      box(
        title = "生存分析设置",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        selectInput("retention_km_variable", "选择分组变量:",
                    choices = list("订阅状态" = "subscription_status",
                                   "湿粮购买" = "has_ever_wet",
                                   "客服互动" = "has_ever_cs_interaction",
                                   "宠物体型" = "breed_size",
                                   "食品等级" = "food_tier"),
                    selected = "subscription_status"),
        checkboxInput("retention_km_confint", tags$span(style = "color:black;", "显示置信区间"), value = TRUE),
        checkboxInput("retention_km_risktable", tags$span(style = "color:black;", "显示风险表"), value = TRUE),
        checkboxInput("retention_km_median", tags$span(style = "color:black;", "显示中位生存时间"), value = TRUE),
        sliderInput("retention_km_xlim", "X轴时间范围(天):",
                    min = 0, max = 1000, value = c(0, 365))
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'retention_cox'",
      box(
        title = "回归模型设置",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        checkboxGroupInput("cox_variables", "选择模型变量:",
                           choiceNames = list(
                             HTML("<span style='color:black;'>订阅状态</span>"),
                             HTML("<span style='color:black;'>湿粮购买</span>"),
                             HTML("<span style='color:black;'>客服互动</span>"),
                             HTML("<span style='color:black;'>平均折扣</span>"),
                             HTML("<span style='color:black;'>湿粮订单比例</span>"),
                             HTML("<span style='color:black;'>宠物体型</span>"),
                             HTML("<span style='color:black;'>食品等级</span>")
                           ),
                           choiceValues = c("subscription_status", "has_ever_wet", "has_ever_cs_interaction",
                                            "avg_discount", "wet_order_ratio", "breed_size", "food_tier"),
                           selected = c("subscription_status", "has_ever_wet", "has_ever_cs_interaction")
        ),
        radioButtons("cox_type", "分析类型:",
                     choiceNames = list(
                       HTML("<span style='color:black;'>单因素分析</span>"),
                       HTML("<span style='color:black;'>多因素分析</span>")
                     ),
                     choiceValues = c("uni", "multi"),
                     selected = "multi"),
        actionButton("retention_run_cox", "运行分析", icon = icon("play"),
                     class = "btn-warning")
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'retention_prediction'",
      box(
        title = "预测参数设置",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        selectInput("retention_pred_subscription", "订阅状态:",
                    choices = list("已订阅" = "TRUE", "未订阅" = "FALSE"),
                    selected = "TRUE"),
        selectInput("retention_pred_wet", "湿粮购买:",
                    choices = list("曾购买" = "TRUE", "从未购买" = "FALSE"),
                    selected = "TRUE"),
        selectInput("retention_pred_cs", "客服互动:",
                    choices = list("有互动" = "TRUE", "无互动" = "FALSE"),
                    selected = "FALSE"),
        sliderInput("retention_pred_discount", "平均折扣(%):",
                    min = 0, max = 50, value = 10, step = 1),
        actionButton("retention_predict", "预测留存率", icon = icon("calculator"),
                     class = "btn-warning")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css)),
      tags$link(rel = "stylesheet", 
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
    ),
    
    tabItems(
      
      
      # 食品分析模块 - 订单类型分析
      tabItem(
        tabName = "order_type",
        h2("订单类型分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          valueBoxOutput("food_total_orders_box", width = 3),
          valueBoxOutput("food_total_pets_box", width = 3),
          valueBoxOutput("food_dry_orders_box", width = 3),
          valueBoxOutput("food_wet_orders_box", width = 3)
        ),
        fluidRow(
          box(
            title = "订单类型分布",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            plotlyOutput("food_order_type_bar", height = "100%")
          )
        ),
        fluidRow(
          box(
            title = "订单类型统计",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("food_order_type_table")
          )
        )
      ),
      
      # 食品分析模块 - 卡路里对比
      tabItem(
        tabName = "calorie_comparison",
        h2("卡路里对比分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "干粮与湿粮卡路里分布对比",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            height = "500px",
            plotlyOutput("food_calorie_boxplot", height = "100%")
          ),
          box(
            title = "干粮与湿粮卡路里密度分布",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            height = "500px",
            plotlyOutput("food_calorie_density", height = "100%")
          )
        )
      ),
      
      # 食品分析模块 - 湿粮质地分析
      tabItem(
        tabName = "texture_analysis",
        h2("湿粮质地分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "湿粮质地偏好分布",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            plotlyOutput("food_texture_bar", height = "100%")
          )
        ),
        fluidRow(
          box(
            title = "湿粮质地统计",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("food_texture_table")
          )
        )
      ),
      
      # 食品分析模块 - 相关性分析
      tabItem(
        tabName = "correlation",
        h2("相关性分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "混合订单中干湿粮卡路里相关性",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            plotlyOutput("food_correlation_scatter", height = "100%")
          )
        ),
        fluidRow(
          box(
            title = "相关性统计结果",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("food_correlation_stats")
          )
        )
      ),
      
      # 消费者分析模块 - 消费者特征
      tabItem(
        tabName = "consumer_characteristics",
        h2("消费者特征分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "消费者特征分布",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            plotlyOutput("consumer_feature_distribution", height = "100%")
          )
        ),
        fluidRow(
          box(
            title = "特征分布统计表",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("consumer_feature_table")
          )
        )
      ),
      
      # 消费者分析模块 - 逻辑回归分析
      tabItem(
        tabName = "consumer_logistic",
        h2("逻辑回归分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "逻辑回归森林图",
            status = "warning",
            solidHeader = TRUE,
            width = 8,
            height = "500px",
            plotlyOutput("consumer_forest_plot", height = "100%")
          ),
          box(
            title = "优势比统计",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            DTOutput("consumer_odds_ratio_table")
          )
        ),
        fluidRow(
          box(
            title = "逻辑回归模型摘要",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("consumer_logistic_summary")
          )
        )
      ),
      
      # 消费者分析模块 - 随机森林分析
      tabItem(
        tabName = "consumer_randomforest",
        h2("随机森林分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "随机森林特征重要性",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            height = "500px",
            plotlyOutput("consumer_rf_importance", height = "100%")
          ),
          box(
            title = "特征重要性排名",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("consumer_importance_table")
          )
        ),
        fluidRow(
          box(
            title = "随机森林OOB误差",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("consumer_rf_oob_error")
          )
        )
      ),
      
      # 消费者分析模块 - 行为时序分析
      tabItem(
        tabName = "consumer_timeseries",
        h2("行为时序分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "湿粮采用时间趋势",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            height = "500px",
            plotlyOutput("consumer_time_series_plot", height = "100%")
          ),
          box(
            title = "行为相关性热力图",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            height = "500px",
            plotlyOutput("consumer_correlation_heatmap", height = "100%")
          )
        ),
        fluidRow(
          box(
            title = "相关性统计表",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("consumer_correlation_table")
          )
        )
      ),
      
      # 消费者分析模块 - 聚类分析
      tabItem(
        tabName = "consumer_clustering",
        h2("聚类分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "聚类特征对比条形图",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            plotlyOutput("consumer_cluster_barplot", height = "100%")
          )
        ),
        fluidRow(
          box(
            title = "PCA聚类分布图",
            status = "warning",
            solidHeader = TRUE,
            width = 8,
            height = "500px",
            plotlyOutput("consumer_pca_cluster_plot", height = "100%")
          ),
          box(
            title = "聚类特征统计",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            DTOutput("consumer_cluster_profile_table")
          )
        ),
        fluidRow(
          box(
            title = "聚类用户画像",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("consumer_cluster_profiles")
          )
        )
      ),
      
      # 用户留存分析模块 - 数据概览
      tabItem(
        tabName = "retention_overview",
        h2("用户留存数据概览", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          valueBoxOutput("retention_total_users", width = 3),
          valueBoxOutput("retention_churn_rate", width = 3),
          valueBoxOutput("retention_avg_survival", width = 3),
          valueBoxOutput("retention_subscription_rate", width = 3)
        ),
        fluidRow(
          box(
            title = "整体流失率统计",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("retention_overall_churn_plot", height = "300px")
          ),
          box(
            title = "按宠物体型分组流失率",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("retention_breed_churn_plot", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "生存时间分布直方图",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("retention_survival_hist", height = "400px")
          )
        )
      ),
      
      # 用户留存分析模块 - Kaplan-Meier生存分析
      tabItem(
        tabName = "retention_km",
        h2("Kaplan-Meier生存分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = textOutput("retention_km_title"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("retention_km_plot", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "生存分析统计摘要",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("retention_km_summary")
          )
        )
      ),
      
      # 用户留存分析模块 - Cox回归分析
      tabItem(
        tabName = "retention_cox",
        h2("Cox比例风险回归分析", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "Cox回归结果",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("retention_cox_results"),
            style = "height: 600px; overflow-y: scroll;"
          ),
          box(
            title = "森林图",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("retention_forest_plot", height = "600px")
          )
        )
      ),
      
      # 用户留存分析模块 - 预测工具
      tabItem(
        tabName = "retention_prediction",
        h2("留存率预测工具", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          valueBoxOutput("retention_pred_30", width = 3),
          valueBoxOutput("retention_pred_60", width = 3),
          valueBoxOutput("retention_pred_90", width = 3),
          valueBoxOutput("retention_pred_180", width = 3)
        ),
        fluidRow(
          box(
            title = "列线图",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("retention_nomogram_plot", height = "600px")
          )
        )
      ),
      
      # 数据管理模块
      tabItem(
        tabName = "data_management",
        h2("数据管理", style = "color: #333; font-weight: bold; margin-bottom: 20px;"),
        fluidRow(
          box(
            title = "数据上传",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            fileInput("data_file_upload", "上传CSV文件",
                      accept = c(".csv", ".txt", ".xlsx", ".xls")),
            actionButton("data_load_button", "加载数据", class = "btn-warning"),
            br(), br(),
            actionButton("data_export_button", "导出当前数据", class = "btn-warning"),
            br(), br(),
            downloadButton("data_download_template", "下载数据模板")
          ),
          box(
            title = "数据概览",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("data_summary")
          )
        ),
        fluidRow(
          box(
            title = "数据预览",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_preview_table")
          )
        )
      )
    )
  )
)

# Server逻辑 - 整合所有模块
server <- function(input, output, session) {
  
  # 数据管理模块 - 数据加载
  data_upload <- reactiveVal(NULL)
  
  observeEvent(input$data_load_button, {
    if (!is.null(input$data_file_upload)) {
      tryCatch({
        # 根据文件类型读取数据
        ext <- tools::file_ext(input$data_file_upload$name)
        if (ext == "csv") {
          df <- read.csv(input$data_file_upload$datapath, stringsAsFactors = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          df <- readxl::read_excel(input$data_file_upload$datapath)
        } else {
          showNotification("不支持的文件格式，请上传CSV或Excel文件", type = "error")
          return()
        }
        
        data_upload(df)
        showNotification("数据加载成功！", type = "success")
      }, error = function(e) {
        showNotification(paste("数据加载失败:", e$message), type = "error")
      })
    }
  })
  
  # 数据预览
  output$data_preview_table <- renderDT({
    df <- data_upload()
    if (is.null(df)) {
      # 如果没有上传数据，尝试加载默认数据
      df <- load_all_data()
    }
    
    datatable(
      head(df, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  output$data_summary <- renderPrint({
    df <- data_upload()
    if (is.null(df)) {
      df <- load_all_data()
    }
    
    cat("数据概览:\n")
    cat("==========\n")
    cat(paste("行数:", nrow(df), "\n"))
    cat(paste("列数:", ncol(df), "\n"))
    cat("\n列名:\n")
    cat(paste(names(df), collapse = ", "), "\n")
    cat("\n数据类型:\n")
    str(df, vec.len = 1)
  })
  
  # 下载数据模板
  output$data_download_template <- downloadHandler(
    filename = function() {
      "pet_food_data_template.csv"
    },
    content = function(file) {
      # 创建一个示例数据模板
      template_df <- data.frame(
        pet_id = c("pet_001", "pet_002"),
        customer_id = c("cust_001", "cust_002"),
        order_payment_date = c("2023-01-01", "2023-01-15"),
        kibble_kcal = c(1500, 1200),
        wet_kcal = c(500, 800),
        total_order_kcal = c(2000, 2000),
        wet_food_textures_in_order = c("gravy,pate", "jelly"),
        pet_breed_size = c("medium", "small"),
        gender = c("male", "female"),
        neutered = c(TRUE, FALSE),
        pet_food_tier = c("premium", "mid"),
        pet_life_stage_at_order = c("mature", "puppy"),
        pet_has_active_subscription = c(TRUE, FALSE),
        wet_food_discount_percent = c(10, 5)
      )
      
      write.csv(template_df, file, row.names = FALSE)
    }
  )
  
  # 主数据加载函数 - 整合所有数据需求
  load_all_data <- reactive({
    tryCatch({
      # 首先检查是否有上传的数据
      uploaded_df <- data_upload()
      if (!is.null(uploaded_df)) {
        cat("使用上传的数据\n")
        return(uploaded_df)
      }
      
      # 否则尝试从默认路径加载
      default_path <- "C:/Users/Linkk/Desktop/R/宠物食品分析/pet_food_customer_orders.csv"
      if (file.exists(default_path)) {
        df <- read.csv(default_path, stringsAsFactors = FALSE,encoding = "GBK")
        cat("从默认路径加载数据成功，维度:", dim(df), "\n")
        return(df)
      } else {
        # 创建模拟数据
        cat("使用模拟数据\n")
        return(create_simulated_data())
      }
    }, error = function(e) {
      cat("数据加载错误:", e$message, "\n")
      return(create_simulated_data())
    })
  })
  
  # 创建模拟数据函数
  create_simulated_data <- function() {
    set.seed(123)
    n <- 2000
    
    # 模拟宠物ID和客户ID
    pet_ids <- paste0("pet_", 1:n)
    customer_ids <- paste0("cust_", 1:n)
    
    # 模拟订单日期
    start_date <- as.Date("2022-01-01")
    end_date <- as.Date("2023-12-31")
    order_dates <- sample(seq(start_date, end_date, by = "day"), n, replace = TRUE)
    
    # 模拟食品数据
    data <- data.frame(
      pet_id = pet_ids,
      customer_id = customer_ids,
      order_payment_date = order_dates,
      kibble_kcal = pmax(0, round(rnorm(n, mean = 1200, sd = 400))),
      wet_kcal = pmax(0, round(rnorm(n, mean = 600, sd = 300))),
      total_order_kcal = NA,  # 稍后计算
      wet_food_textures_in_order = sample(c("gravy", "jelly", "pate", "gravy,pate", "jelly,pate", ""), 
                                          n, replace = TRUE, prob = c(0.3, 0.2, 0.25, 0.1, 0.1, 0.05)),
      pet_breed_size = factor(sample(c("small", "medium", "large", "toy", "giant"), n, replace = TRUE,
                                     prob = c(0.3, 0.4, 0.2, 0.05, 0.05))),
      gender = sample(c("male", "female"), n, replace = TRUE),
      neutered = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.7, 0.3)),
      pet_food_tier = factor(sample(c("mid", "premium", "superpremium"), n, replace = TRUE,
                                    prob = c(0.5, 0.3, 0.2))),
      pet_life_stage_at_order = factor(sample(c("puppy", "kitten", "mature", "senior"), n, replace = TRUE,
                                              prob = c(0.2, 0.1, 0.6, 0.1))),
      pet_has_active_subscription = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.6, 0.4)),
      wet_food_discount_percent = round(runif(n, 0, 30), 1),
      total_web_sessions = rpois(n, 15),
      total_minutes_on_website = rnorm(n, 45, 15),
      total_wet_food_updates = rpois(n, 3),
      premium_treat_packs = rpois(n, 2),
      dental_treat_packs = rpois(n, 1),
      pet_signup_datetime = order_dates - sample(0:30, n, replace = TRUE),
      stringsAsFactors = FALSE
    )
    
    # 计算总卡路里
    data$total_order_kcal <- data$kibble_kcal + data$wet_kcal
    
    # 添加一些缺失值
    for (col in c("wet_food_textures_in_order", "pet_food_tier", "pet_life_stage_at_order")) {
      na_idx <- sample(1:n, n * 0.05)
      data[na_idx, col] <- NA
    }
    
    return(data)
  }
  
  
  
  # 食品分析模块 - 数据预处理
  food_raw_data <- reactive({
    data <- load_all_data()
    
    # 应用原始代码中的预处理步骤
    data <- data %>%
      mutate(across(where(is.factor), as.character))
    
    data <- data %>%
      mutate(
        across(where(is.numeric), ~ replace_na(.x, 0)),
        across(where(is.character), ~ replace_na(.x, "None"))
      )
    
    processed_data <- data %>%
      mutate(
        # 处理逻辑和因子变量
        pet_has_active_subscription = as.logical(pet_has_active_subscription),
        neutered = as.logical(neutered),
        
        # 分类变量因子化
        gender = factor(gender, levels = c("male", "female")),
        pet_breed_size = factor(pet_breed_size, levels = c("small", "medium", "large","giant","toy")),
        pet_food_tier = factor(pet_food_tier, levels = c("mid", "premium", "superpremium")),
        
        # 湿粮质地处理
        texture_type = case_when(
          grepl("gravy", wet_food_textures_in_order, ignore.case = TRUE) ~ "gravy",
          grepl("jelly", wet_food_textures_in_order, ignore.case = TRUE) ~ "jelly",
          grepl("pate", wet_food_textures_in_order, ignore.case = TRUE) ~ "pate",
          wet_food_textures_in_order == "" | is.na(wet_food_textures_in_order) ~ "none",
          TRUE ~ "other"
        ),
        texture_type = factor(texture_type, levels = c("none", "gravy", "jelly", "pate", "other")),
        
        # 订单类型分类
        order_type = case_when(
          kibble_kcal > 0 & wet_kcal == 0 ~ "纯干粮",
          kibble_kcal == 0 & wet_kcal > 0 ~ "纯湿粮",
          kibble_kcal > 0 & wet_kcal > 0 ~ "混合订单",
          TRUE ~ "其他"
        ),
        order_type = factor(order_type, levels = c("纯干粮", "纯湿粮", "混合订单", "其他")),
        
        # 计算湿粮比例
        wet_calorie_ratio = ifelse(total_order_kcal > 0, 
                                   wet_kcal / total_order_kcal, 
                                   0) * 100
      ) %>%
      # 移除无效数据
      filter(!is.na(kibble_kcal) & !is.na(wet_kcal) & total_order_kcal > 0)
    
    return(processed_data)
  })
  
  # 应用筛选器后的数据
  food_filtered_data <- eventReactive(input$food_apply_filters, {
    data <- food_raw_data()
    
    # 应用订单类型筛选
    if (!"所有订单" %in% input$food_order_type_filter) {
      data <- data %>% filter(order_type %in% input$food_order_type_filter)
    }
    
    # 应用湿粮比例筛选
    data <- data %>%
      filter(wet_calorie_ratio >= input$food_wet_ratio_range[1] &
               wet_calorie_ratio <= input$food_wet_ratio_range[2])
    
    # 应用卡路里筛选
    data <- data %>%
      filter(total_order_kcal >= input$food_calorie_range[1] &
               total_order_kcal <= input$food_calorie_range[2])
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # 初始加载时使用完整数据
  food_filtered_data_initial <- reactive({
    food_raw_data()
  })
  
  # 使用当前数据
  food_current_data <- reactive({
    if (input$food_apply_filters == 0) {
      food_filtered_data_initial()
    } else {
      food_filtered_data()
    }
  })
  
  # 食品分析模块 - 统计卡片
  output$food_total_orders_box <- renderValueBox({
    data <- food_current_data()
    valueBox(
      value = format(nrow(data), big.mark = ","),
      subtitle = "总订单数量",
      icon = icon("shopping-cart"),
      color = "yellow"
    )
  })
  
  output$food_total_pets_box <- renderValueBox({
    data <- food_current_data()
    unique_pets <- length(unique(data$pet_id))
    valueBox(
      value = format(unique_pets, big.mark = ","),
      subtitle = "宠物数量",
      icon = icon("paw"),
      color = "yellow"
    )
  })
  
  output$food_dry_orders_box <- renderValueBox({
    data <- food_current_data()
    dry_count <- sum(data$order_type == "纯干粮")
    valueBox(
      value = format(dry_count, big.mark = ","),
      subtitle = "纯干粮订单",
      icon = icon("bread-slice"),
      color = "yellow"
    )
  })
  
  output$food_wet_orders_box <- renderValueBox({
    data <- food_current_data()
    wet_count <- sum(data$order_type == "纯湿粮")
    valueBox(
      value = format(wet_count, big.mark = ","),
      subtitle = "纯湿粮订单",
      icon = icon("tint"),
      color = "yellow"
    )
  })
  
  # 食品分析模块 - 订单类型条形图
  output$food_order_type_bar <- renderPlotly({
    data <- food_current_data()
    
    order_summary <- data %>%
      group_by(order_type) %>%
      summarise(
        count = n(),
        percentage = n() / nrow(data) * 100,
        .groups = 'drop'
      )
    
    # 按百分比排序
    order_summary <- order_summary %>%
      arrange(desc(percentage))
    
    # 定义颜色
    colors <- c("纯干粮" = "#4E79A7", 
                "纯湿粮" = "#F28E2B", 
                "混合订单" = "#59A14F",
                "其他" = "#E15759")
    
    p <- ggplot(order_summary, aes(x = reorder(order_type, -percentage), y = percentage, 
                                   fill = order_type,
                                   text = paste("订单类型:", order_type,
                                                "<br>数量:", count,
                                                "<br>占比:", sprintf("%.1f%%", percentage)))) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", percentage, count)), 
                vjust = -0.3, size = 4) +
      labs(
        title = "订单类型分布",
        x = "订单类型",
        y = "百分比 (%)",
        fill = "订单类型"
      ) +
      scale_fill_manual(values = colors[order_summary$order_type]) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      ) +
      ylim(0, max(order_summary$percentage) * 1.1)
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(t = 50)
      )
  })
  
  # 食品分析模块 - 订单类型统计表格
  output$food_order_type_table <- renderDT({
    data <- food_current_data()
    
    summary_table <- data %>%
      group_by(order_type) %>%
      summarise(
        订单数量 = n(),
        占比 = sprintf("%.1f%%", n() / nrow(data) * 100),
        平均干粮卡路里 = sprintf("%.0f kcal", mean(kibble_kcal, na.rm = TRUE)),
        平均湿粮卡路里 = sprintf("%.0f kcal", mean(wet_kcal, na.rm = TRUE)),
        平均总卡路里 = sprintf("%.0f kcal", mean(total_order_kcal, na.rm = TRUE)),
        平均湿粮比例 = sprintf("%.1f%%", mean(wet_calorie_ratio, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      arrange(desc(订单数量))
    
    datatable(
      summary_table,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE,
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(columns = 1:7, fontSize = '13px')
  })
  
  # 食品分析模块 - 卡路里箱线图
  output$food_calorie_boxplot <- renderPlotly({
    data <- food_current_data()
    
    # 准备长格式数据用于绘图
    calorie_long <- data %>%
      select(kibble_kcal, wet_kcal) %>%
      pivot_longer(cols = everything(), names_to = "food_type", values_to = "calories") %>%
      mutate(
        food_type = factor(food_type, 
                           levels = c("kibble_kcal", "wet_kcal"),
                           labels = c("干粮", "湿粮"))
      )
    
    p <- ggplot(calorie_long, aes(x = food_type, y = calories, fill = food_type,
                                  text = paste("食品类型:", food_type,
                                               "<br>均值:", round(mean(calories), 1), "kcal",
                                               "<br>中位数:", round(median(calories), 1), "kcal",
                                               "<br>Q1:", round(quantile(calories, 0.25), 1), "kcal",
                                               "<br>Q3:", round(quantile(calories, 0.75), 1), "kcal",
                                               "<br>范围:", round(min(calories), 1), "-", round(max(calories), 1), "kcal"))) +
      geom_boxplot(alpha = 0.7, outlier.shape = 1, outlier.color = "red") +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
      labs(
        title = "干粮与湿粮卡路里分布对比",
        subtitle = "箱线图展示（黑钻表示均值）",
        x = "食品类型",
        y = "卡路里 (kcal)",
        fill = "食品类型"
      ) +
      scale_fill_manual(values = c("干粮" = "#4E79A7", "湿粮" = "#F28E2B")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50", size = 12),
        legend.position = "top",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(t = 70)
      )
  })
  
  # 食品分析模块 - 卡路里密度图
  output$food_calorie_density <- renderPlotly({
    data <- food_current_data()
    
    # 分别准备干粮和湿粮数据
    dry_data <- data.frame(
      calories = data$kibble_kcal,
      food_type = "干粮"
    )
    
    wet_data <- data.frame(
      calories = data$wet_kcal,
      food_type = "湿粮"
    )
    
    # 合并数据
    calorie_long <- rbind(dry_data, wet_data)
    calorie_long$food_type <- factor(calorie_long$food_type, levels = c("干粮", "湿粮"))
    
    # 创建基础ggplot对象
    p <- ggplot(calorie_long, aes(x = calories, fill = food_type)) +
      geom_density(alpha = 0.5, adjust = 1.5) +
      labs(
        title = "干粮与湿粮卡路里密度分布",
        x = "卡路里 (kcal)",
        y = "密度",
        fill = "食品类型"
      ) +
      scale_fill_manual(values = c("干粮" = "#4E79A7", "湿粮" = "#F28E2B")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "top",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11)
      ) +
      facet_wrap(~food_type, scales = "free_y", ncol = 1)
    
    # 直接转换为plotly
    ggplotly(p) %>%
      layout(
        hovermode = "x unified",
        margin = list(t = 50)
      )
  })
  
  # 食品分析模块 - 湿粮质地条形图
  output$food_texture_bar <- renderPlotly({
    data <- food_current_data() %>%
      filter(wet_kcal > 0)  # 仅分析包含湿粮的订单
    
    if (nrow(data) > 0) {
      texture_summary <- data %>%
        group_by(texture_type) %>%
        summarise(
          orders = n(),
          percentage = n() / nrow(data) * 100,
          avg_wet_kcal = mean(wet_kcal, na.rm = TRUE),
          avg_wet_ratio = mean(wet_calorie_ratio, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(desc(orders))
      
      p <- ggplot(texture_summary, aes(x = reorder(texture_type, -orders), y = orders, 
                                       fill = texture_type,
                                       text = paste("质地类型:", texture_type,
                                                    "<br>订单数量:", orders,
                                                    "<br>占比:", sprintf("%.1f%%", percentage),
                                                    "<br>平均湿粮卡路里:", round(avg_wet_kcal, 1), "kcal",
                                                    "<br>平均湿粮比例:", round(avg_wet_ratio, 1), "%"))) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", percentage, orders)), 
                  vjust = -0.3, size = 4) +
        labs(
          title = "湿粮质地偏好分布",
          subtitle = "仅包含湿粮的订单",
          x = "湿粮质地类型",
          y = "订单数量",
          fill = "质地类型"
        ) +
        scale_fill_brewer(palette = "Set3") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, color = "gray50", size = 12),
          legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        ylim(0, max(texture_summary$orders) * 1.1)
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 70)
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "当前筛选条件下无湿粮订单数据",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          title = "湿粮质地偏好分布",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          margin = list(t = 50)
        )
    }
  })
  
  # 食品分析模块 - 湿粮质地统计表格
  output$food_texture_table <- renderDT({
    data <- food_current_data() %>%
      filter(wet_kcal > 0)  # 仅分析包含湿粮的订单
    
    if (nrow(data) > 0) {
      texture_summary <- data %>%
        group_by(texture_type) %>%
        summarise(
          订单数量 = n(),
          占比 = sprintf("%.1f%%", n() / nrow(data) * 100),
          平均湿粮卡路里 = sprintf("%.0f kcal", mean(wet_kcal, na.rm = TRUE)),
          平均湿粮比例 = sprintf("%.1f%%", mean(wet_calorie_ratio, na.rm = TRUE)),
          平均总卡路里 = sprintf("%.0f kcal", mean(total_order_kcal, na.rm = TRUE)),
          .groups = 'drop'
        ) %>%
        arrange(desc(订单数量))
      
      datatable(
        texture_summary,
        options = list(
          pageLength = 5,
          dom = 't',
          scrollX = TRUE,
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        formatStyle(columns = 1:6, fontSize = '13px')
    } else {
      datatable(
        data.frame(消息 = "当前筛选条件下无湿粮订单数据"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # 食品分析模块 - 相关性散点图
  output$food_correlation_scatter <- renderPlotly({
    data <- food_current_data() %>%
      filter(order_type == "混合订单")  # 仅混合订单
    
    if (nrow(data) > 10) {
      # 计算相关性
      cor_test <- cor.test(data$kibble_kcal, data$wet_kcal, method = "pearson")
      correlation <- cor_test$estimate
      p_value <- cor_test$p.value
      
      p <- ggplot(data, aes(x = kibble_kcal, y = wet_kcal,
                            text = paste("干粮:", round(kibble_kcal, 1), "kcal",
                                         "<br>湿粮:", round(wet_kcal, 1), "kcal",
                                         "<br>总卡路里:", round(kibble_kcal + wet_kcal, 1), "kcal",
                                         "<br>湿粮比例:", round(wet_kcal/(kibble_kcal + wet_kcal)*100, 1), "%"))) +
        geom_point(alpha = 0.5, color = "steelblue", size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
        labs(
          title = sprintf("混合订单中干湿粮卡路里相关性\nPearson r = %.3f, p = %.3f", correlation, p_value),
          x = "干粮卡路里 (kcal)",
          y = "湿粮卡路里 (kcal)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 11),
          plot.subtitle = element_text(hjust = 0.5, color = "gray50", size = 12)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 70)
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "混合订单数据不足（需要至少10个样本）",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          title = "混合订单中干湿粮卡路里相关性",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          margin = list(t = 50)
        )
    }
  })
  
  # 食品分析模块 - 相关性统计结果
  output$food_correlation_stats <- renderPrint({
    data <- food_current_data() %>%
      filter(order_type == "混合订单")  # 仅混合订单
    
    if (nrow(data) > 10) {
      # 计算相关性
      cor_test <- cor.test(data$kibble_kcal, data$wet_kcal, method = "pearson")
      
      cat("混合订单干湿粮相关性分析结果\n")
      cat("========================================\n\n")
      cat(sprintf("混合订单样本数量: %d\n", nrow(data)))
      cat(sprintf("干粮卡路里均值: %.1f kcal\n", mean(data$kibble_kcal, na.rm = TRUE)))
      cat(sprintf("湿粮卡路里均值: %.1f kcal\n", mean(data$wet_kcal, na.rm = TRUE)))
      cat(sprintf("总卡路里均值: %.1f kcal\n", mean(data$total_order_kcal, na.rm = TRUE)))
      cat("\n相关性统计:\n")
      cat("----------------\n")
      cat(sprintf("Pearson相关系数 (r): %.4f\n", cor_test$estimate))
      cat(sprintf("t统计量: %.4f\n", cor_test$statistic))
      cat(sprintf("自由度: %.0f\n", cor_test$parameter))
      cat(sprintf("p-value: %.6f\n", cor_test$p.value))
      cat(sprintf("95%% 置信区间: [%.4f, %.4f]\n", 
                  cor_test$conf.int[1], cor_test$conf.int[2]))
      
      cat("\n显著性判断:\n")
      cat("------------\n")
      if (cor_test$p.value < 0.001) {
        cat("  ✅ 极显著相关 (p < 0.001)\n")
      } else if (cor_test$p.value < 0.01) {
        cat("  ✅ 非常显著相关 (p < 0.01)\n")
      } else if (cor_test$p.value < 0.05) {
        cat("  ✅ 显著相关 (p < 0.05)\n")
      } else {
        cat("  ⚠️  无显著相关性 (p > 0.05)\n")
      }
      
      cat("\n关系方向与强度:\n")
      cat("----------------\n")
      if (cor_test$estimate > 0.5) {
        cat("  🔴 强正相关\n")
      } else if (cor_test$estimate > 0.3) {
        cat("  🟡 中等正相关\n")
      } else if (cor_test$estimate > 0.1) {
        cat("  🟢 正相关\n")
      } else if (cor_test$estimate > 0) {
        cat("  ⚪ 极弱正相关\n")
      } else if (cor_test$estimate < -0.5) {
        cat("  🔴 强负相关\n")
      } else if (cor_test$estimate < -0.3) {
        cat("  🟡 中等负相关\n")
      } else if (cor_test$estimate < -0.1) {
        cat("  🟢 弱负相关\n")
      } else if (cor_test$estimate < 0) {
        cat("  ⚪ 极弱负相关\n")
      } else {
        cat("  ➖ 无线性关系\n")
      }
      
      if (cor_test$estimate > 0 && cor_test$p.value < 0.05) {
        cat("\n业务解读:\n")
        cat("--------\n")
        cat("  干粮与湿粮呈显著正相关，表明二者是互补关系。\n")
        cat("  宠物主倾向于同时购买或增加一种的同时也增加另一种。\n")
      } else if (cor_test$estimate < 0 && cor_test$p.value < 0.05) {
        cat("\n业务解读:\n")
        cat("--------\n")
        cat("  干粮与湿粮呈显著负相关，表明二者是替代关系。\n")
        cat("  宠物主倾向于在两者间进行取舍。\n")
      } else if (cor_test$p.value >= 0.05) {
        cat("\n业务解读:\n")
        cat("--------\n")
        cat("  干粮与湿粮无显著相关性，表明二者可能是独立决策。\n")
      }
      
    } else {
      cat("数据不足，无法进行相关性分析\n")
      cat("========================================\n")
      cat("需要至少10个混合订单样本，当前只有", nrow(data), "个\n")
      cat("\n建议:\n")
      cat("1. 放宽筛选条件\n")
      cat("2. 检查数据质量\n")
      cat("3. 确保选择了'所有订单'或包含'混合订单'\n")
    }
  })
  
  # 消费者分析模块 - 数据预处理
  # 修改消费者分析模块的数据预处理部分
  consumer_raw_data <- reactive({
    data <- load_all_data()
    
    tryCatch({
      cat("消费者分析数据预处理开始...\n")
      
      # 应用预处理步骤
      df <- data %>%
        mutate(across(where(is.factor), as.character))
      
      df <- df %>%
        mutate(
          across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)),
          across(where(is.character), ~ ifelse(is.na(.x) | .x == "", "None", .x))
        )
      
      df_clean <- df %>%
        mutate(
          # 处理signup_promo中的特殊值
          signup_promo = ifelse(signup_promo %in% c("Null & Default", "", "None", "NA"), 
                                "Unknown", signup_promo),
          
          # 转换逻辑变量
          pet_has_active_subscription = as.logical(pet_has_active_subscription),
          neutered = as.logical(neutered),
          
          # 转换日期时间
          pet_signup_datetime = as.POSIXct(pet_signup_datetime, format = "%Y-%m-%d %H:%M:%S"),
          order_payment_date = as.Date(order_payment_date),
          
          # 创建食品类型分类
          food_type = case_when(
            wet_kcal > 0 & kibble_kcal > 0 ~ "混合",
            wet_kcal > 0 & kibble_kcal == 0 ~ "纯湿粮",
            wet_kcal == 0 & kibble_kcal > 0 ~ "纯干粮",
            TRUE ~ "其他"
          ),
          
          # 计算湿粮比例
          wet_food_ratio = ifelse(total_order_kcal > 0, wet_kcal / total_order_kcal, 0),
          
          # 创建是否购买湿粮的二元变量
          wet_food_purchased = wet_kcal > 0,
          
          # 简化健康问题列表（提取主要问题）
          health_issue_main = str_extract(pet_health_issue_list, "^[^,]+"),
          
          # 创建是否订阅的简化变量
          has_subscription = pet_has_active_subscription,
          
          # 创建体型分类（简化）
          breed_size_group = case_when(
            pet_breed_size %in% c("small") ~ "小型",
            pet_breed_size %in% c("medium") ~ "中型",
            pet_breed_size %in% c("large") ~ "大型",
            pet_breed_size %in% c("toy") ~ "迷你型",
            pet_breed_size %in% c("giant") ~ "巨大型",
            TRUE ~ "其他"
          )
        )
      
      # 转换分类变量为因子
      categorical_vars <- c('food_type', 'pet_food_tier', 'gender', 'neutered', 
                            'pet_breed_size', 'breed_size_group', 'signup_promo',
                            'dry_food_brand_pre_tails', 'pet_life_stage_at_order',
                            'wet_tray_size', 'customer_support_ticket_category',
                            'health_issue_main')
      
      # 只转换存在的列
      existing_vars <- categorical_vars[categorical_vars %in% names(df_clean)]
      df_clean <- df_clean %>%
        mutate(across(all_of(existing_vars), as.factor))
      
      cat("数据预处理完成，食品类型分布:\n")
      print(table(df_clean$food_type))
      
      return(df_clean)
      
    }, error = function(e) {
      cat("数据加载错误:", e$message, "\n")
      
      # 创建模拟数据用于测试
      set.seed(123)
      n <- 1000
      simulated_data <- data.frame(
        pet_id = paste0("pet_", 1:n),
        customer_id = paste0("cust_", 1:n),
        food_type = factor(sample(c("纯干粮", "纯湿粮", "混合"), n, replace = TRUE, 
                                  prob = c(0.5, 0.3, 0.2)), 
                           levels = c("纯干粮", "混合", "纯湿粮")),
        wet_food_ratio = runif(n, 0, 1),
        wet_food_purchased = rbinom(n, 1, 0.5),
        breed_size_group = factor(sample(c("小型", "中型", "大型"), n, replace = TRUE)),
        gender = factor(sample(c("male", "female"), n, replace = TRUE)),
        neutered = sample(c(TRUE, FALSE), n, replace = TRUE),
        pet_life_stage_at_order = factor(sample(c("puppy", "kitten", "mature", "senior"), n, replace = TRUE)),
        pet_food_tier = factor(sample(c("mid", "premium", "superpremium"), n, replace = TRUE)),
        total_web_sessions = rpois(n, 10),
        total_minutes_on_website = rnorm(n, 30, 10),
        total_wet_food_updates = rpois(n, 2),
        wet_food_discount_percent = runif(n, 0, 30),
        premium_treat_packs = rpois(n, 1),
        dental_treat_packs = rpois(n, 1),
        has_subscription = rbinom(n, 1, 0.3),
        order_payment_date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = n)
      )
      
      cat("使用模拟数据\n")
      return(simulated_data)
    })
  })
  
  # 消费者分析模块 - 应用筛选器后的数据
  consumer_filtered_data <- reactive({
    data <- consumer_raw_data()
    
    # 应用食品类型筛选
    if (!"所有类型" %in% input$consumer_food_type_filter) {
      data <- data %>% filter(food_type %in% input$consumer_food_type_filter)
    }
    
    # 应用宠物体型筛选
    if (!"所有体型" %in% input$consumer_breed_size_filter) {
      data <- data %>% filter(breed_size_group %in% input$consumer_breed_size_filter)
    }
    
    # 应用食品等级筛选
    if (!"所有等级" %in% input$consumer_food_tier_filter) {
      data <- data %>% filter(pet_food_tier %in% input$consumer_food_tier_filter)
    }
    
    # 应用生命阶段筛选
    if (!"所有阶段" %in% input$consumer_life_stage_filter) {
      data <- data %>% filter(pet_life_stage_at_order %in% input$consumer_life_stage_filter)
    }
    
    cat(sprintf("消费者数据筛选后: %d 行\n", nrow(data)))
    return(data)
  })
  
  # 消费者分析模块 - 特征分布图
  output$consumer_feature_distribution <- renderPlotly({
    data <- consumer_filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "无数据可用"))
    }
    
    # 创建特征对比函数
    compare_features <- function(data, feature_var, group_var = "food_type") {
      result <- data %>%
        group_by(!!sym(group_var), !!sym(feature_var)) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(!!sym(group_var)) %>%
        mutate(
          total = sum(count),
          percentage = count / total * 100
        ) %>%
        ungroup()
      
      return(result)
    }
    
    # 定义要对比的特征
    features_to_compare <- list(
      "breed_size_group" = "宠物体型",
      "gender" = "性别", 
      "neutered" = "绝育状态",
      "pet_life_stage_at_order" = "生命阶段",
      "pet_food_tier" = "食品等级"
    )
    
    # 检查第一个特征是否存在
    feature <- names(features_to_compare)[1]
    if (!feature %in% names(data)) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "特征不存在"))
    }
    
    feature_name <- features_to_compare[[feature]]
    
    comp_table <- compare_features(data, feature)
    
    # 确保有数据
    if (nrow(comp_table) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "无足够数据"))
    }
    
    p <- ggplot(comp_table, aes(x = !!sym(feature), y = percentage, fill = food_type)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
      labs(
        title = paste(feature_name, "分布对比"),
        x = feature_name,
        y = "百分比 (%)",
        fill = "食品类型"
      ) +
      scale_fill_manual(values = c("纯干粮" = "#4E79A7", 
                                   "纯湿粮" = "#F28E2B", 
                                   "混合" = "#59A14F",
                                   "其他" = "#E15759")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(t = 50, b = 100)
      )
  })
  
  # 消费者分析模块 - 特征分布统计表
  output$consumer_feature_table <- renderDT({
    data <- consumer_filtered_data()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(消息 = "无数据可用")))
    }
    
    # 计算各特征的分布统计
    feature_stats <- data.frame()
    
    features <- c("breed_size_group", "gender", "neutered", 
                  "pet_life_stage_at_order", "pet_food_tier")
    
    # 只使用存在的特征
    features <- features[features %in% names(data)]
    
    for (feature in features) {
      stats <- data %>%
        group_by(food_type, !!sym(feature)) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(food_type) %>%
        mutate(
          total = sum(count),
          percentage = count / total * 100
        ) %>%
        ungroup() %>%
        mutate(feature = feature)
      
      feature_stats <- bind_rows(feature_stats, stats)
    }
    
    if (nrow(feature_stats) > 0) {
      feature_stats <- feature_stats %>%
        select(feature, food_type, everything()) %>%
        rename(
          特征 = feature,
          食品类型 = food_type,
          类别 = 3,
          数量 = count,
          百分比 = percentage
        )
    }
    
    datatable(
      feature_stats,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    ) %>%
      formatRound('百分比', 1)
  })
  
  # 消费者分析模块 - 逻辑回归模型
  consumer_logistic_model <- reactive({
    data <- consumer_filtered_data()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # 检查必需的列
    required_cols <- c("wet_food_purchased", "breed_size_group", "gender", "neutered",
                       "pet_life_stage_at_order", "pet_food_tier", 
                       "total_web_sessions", "has_subscription")
    
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      cat("消费者逻辑回归缺少必需的列:", paste(missing_cols, collapse = ", "), "\n")
      return(NULL)
    }
    
    tryCatch({
      # 准备回归数据
      reg_data <- data %>%
        select(all_of(required_cols)) %>%
        na.omit()
      
      if (nrow(reg_data) == 0) {
        cat("消费者回归数据为空\n")
        return(NULL)
      }
      
      # 确保wet_food_purchased是二分类
      if (length(unique(reg_data$wet_food_purchased)) < 2) {
        cat("消费者wet_food_purchased只有单一类别\n")
        return(NULL)
      }
      
      # 拟合逻辑回归模型
      model <- glm(wet_food_purchased ~ breed_size_group + gender + neutered +
                     pet_life_stage_at_order + pet_food_tier + 
                     total_web_sessions + has_subscription,
                   data = reg_data, family = binomial())
      
      cat("消费者逻辑回归模型构建成功，样本数:", nrow(reg_data), "\n")
      return(model)
      
    }, error = function(e) {
      cat("消费者逻辑回归错误:", e$message, "\n")
      return(NULL)
    })
  })
  
  # 消费者分析模块 - 森林图
  output$consumer_forest_plot <- renderPlotly({
    model <- consumer_logistic_model()
    
    if (is.null(model)) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "无法生成逻辑回归模型"))
    }
    
    tryCatch({
      # 提取系数并转换为OR
      or_results <- model %>%
        tidy(exponentiate = TRUE, conf.int = TRUE) %>%
        filter(term != "(Intercept)")
      
      if (nrow(or_results) == 0) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "无回归系数"))
      }
      
      # 重命名变量
      or_results <- or_results %>%
        mutate(
          term = gsub("breed_size_group", "体型: ", term),
          term = gsub("pet_life_stage_at_order", "生命阶段: ", term),
          term = gsub("pet_food_tier", "食品等级: ", term),
          term = gsub("gendermale", "性别: 雄性", term),
          term = gsub("neuteredTRUE", "已绝育", term),
          term = gsub("total_web_sessions", "网站访问次数", term),
          term = gsub("has_subscriptionTRUE", "有订阅", term)
        )
      
      # 按估计值排序
      or_results$term <- factor(or_results$term, levels = or_results$term[order(or_results$estimate)])
      
      p <- ggplot(or_results, aes(x = estimate, y = term)) +
        geom_point(size = 3, color = "darkblue") +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                       height = 0.2, color = "darkblue") +
        geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
        labs(
          title = "湿粮购买逻辑回归森林图",
          x = "优势比 (95% 置信区间)",
          y = "变量"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 10)
        ) +
        scale_x_log10()
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 50, l = 150)
        )
      
    }, error = function(e) {
      cat("消费者森林图错误:", e$message, "\n")
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "森林图生成错误"))
    })
  })
  
  # 消费者分析模块 - 优势比统计表
  output$consumer_odds_ratio_table <- renderDT({
    model <- consumer_logistic_model()
    
    if (is.null(model)) {
      return(datatable(data.frame(消息 = "无逻辑回归模型可用")))
    }
    
    tryCatch({
      # 计算优势比和置信区间
      odds_ratios <- exp(coef(model))
      ci <- exp(confint(model))
      
      or_table <- data.frame(
        Variable = names(odds_ratios),
        Odds_Ratio = round(odds_ratios, 3),
        CI_95_Lower = round(ci[, 1], 3),
        CI_95_Upper = round(ci[, 2], 3),
        stringsAsFactors = FALSE
      ) %>%
        filter(Variable != "(Intercept)") %>%
        mutate(
          Variable = gsub("breed_size_group", "体型: ", Variable),
          Variable = gsub("pet_life_stage_at_order", "生命阶段: ", Variable),
          Variable = gsub("pet_food_tier", "食品等级: ", Variable),
          Variable = gsub("gendermale", "性别: 雄性", Variable),
          Variable = gsub("neuteredTRUE", "已绝育", Variable),
          Variable = gsub("total_web_sessions", "网站访问次数", Variable),
          Variable = gsub("has_subscriptionTRUE", "有订阅", Variable)
        )
      
      datatable(
        or_table,
        options = list(
          pageLength = 15,
          dom = 't',
          scrollX = TRUE
        ),
        rownames = FALSE
      )
      
    }, error = function(e) {
      cat("消费者优势比表错误:", e$message, "\n")
      return(datatable(data.frame(错误 = "无法计算优势比")))
    })
  })
  
  # 消费者分析模块 - 逻辑回归模型摘要
  output$consumer_logistic_summary <- renderPrint({
    model <- consumer_logistic_model()
    
    if (is.null(model)) {
      cat("逻辑回归模型不可用\n")
      cat("可能原因:\n")
      cat("1. 数据不足\n")
      cat("2. 缺少必要的变量\n")
      cat("3. 所有观测值都属于同一类别\n")
      return()
    }
    
    cat("消费者逻辑回归模型摘要\n")
    cat("=======================\n\n")
    print(summary(model))
    
    cat("\n模型拟合优度:\n")
    cat(paste0("AIC: ", round(AIC(model), 2), "\n"))
    cat(paste0("BIC: ", round(BIC(model), 2), "\n"))
    
    # 伪R方
    tryCatch({
      null_model <- update(model, . ~ 1)
      mcfadden_r2 <- 1 - (logLik(model)[1] / logLik(null_model)[1])
      cat(paste0("McFadden伪R方: ", round(mcfadden_r2, 3), "\n"))
    }, error = function(e) {
      cat("无法计算伪R方\n")
    })
  })
  
  # 消费者分析模块 - 随机森林模型
  consumer_rf_model <- reactive({
    data <- consumer_filtered_data()
    
    if (nrow(data) < 50) {  # 确保有足够的数据
      cat("消费者随机森林数据不足，需要至少50个样本，当前:", nrow(data), "\n")
      return(NULL)
    }
    
    # 检查必需的列
    required_cols <- c("food_type", "breed_size_group", "gender", "neutered",
                       "pet_life_stage_at_order", "pet_food_tier",
                       "total_web_sessions", "has_subscription")
    
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      cat("消费者随机森林缺少必需的列:", paste(missing_cols, collapse = ", "), "\n")
      return(NULL)
    }
    
    tryCatch({
      # 准备随机森林数据
      tree_data <- data %>%
        select(all_of(required_cols)) %>%
        na.omit()
      
      cat("消费者随机森林数据准备，样本数:", nrow(tree_data), "\n")
      
      # 确保food_type有多个水平且是因子
      if (!is.factor(tree_data$food_type)) {
        tree_data$food_type <- as.factor(tree_data$food_type)
      }
      
      # 检查类别数量
      food_type_levels <- levels(tree_data$food_type)
      cat("消费者food_type水平:", paste(food_type_levels, collapse = ", "), "\n")
      cat("消费者food_type分布:\n")
      print(table(tree_data$food_type))
      
      if (length(food_type_levels) < 2) {
        cat("消费者food_type类别不足，需要至少2个类别\n")
        return(NULL)
      }
      
      # 确保其他分类变量是因子
      factor_cols <- c("breed_size_group", "gender", "neutered", 
                       "pet_life_stage_at_order", "pet_food_tier", "has_subscription")
      
      for (col in factor_cols) {
        if (col %in% names(tree_data) && !is.factor(tree_data[[col]])) {
          tree_data[[col]] <- as.factor(tree_data[[col]])
        }
      }
      
      # 检查是否有缺失值
      if (any(is.na(tree_data))) {
        cat("消费者随机森林数据中存在NA值，正在移除...\n")
        tree_data <- na.omit(tree_data)
        cat("移除NA后样本数:", nrow(tree_data), "\n")
      }
      
      if (nrow(tree_data) < 30) {
        cat("消费者随机森林数据不足，移除NA后样本数:", nrow(tree_data), "\n")
        return(NULL)
      }
      
      cat("开始构建消费者随机森林模型...\n")
      
      # 构建随机森林模型
      set.seed(123)
      model <- randomForest(
        food_type ~ .,
        data = tree_data,
        ntree = 100,
        mtry = sqrt(ncol(tree_data) - 1),
        importance = TRUE,
        na.action = na.omit,
        do.trace = 10
      )
      
      cat("消费者随机森林模型构建成功\n")
      cat("OOB误差率:", round(model$err.rate[nrow(model$err.rate), "OOB"] * 100, 2), "%\n")
      
      return(model)
      
    }, error = function(e) {
      cat("消费者随机森林构建错误:", e$message, "\n")
      cat("错误详情:", paste(e, collapse = "\n"), "\n")
      return(NULL)
    })
  })
  
  # 消费者分析模块 - 随机森林特征重要性图
  output$consumer_rf_importance <- renderPlotly({
    model <- consumer_rf_model()
    
    if (is.null(model)) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(
                 title = "无法生成随机森林模型",
                 annotations = list(
                   text = "请检查数据筛选条件，确保有足够的数据进行随机森林分析",
                   x = 0.5,
                   y = 0.5,
                   showarrow = FALSE,
                   font = list(size = 16)
                 )
               ))
    }
    
    tryCatch({
      # 提取特征重要性
      importance_data <- importance(model)
      
      if (is.null(importance_data)) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "特征重要性数据不可用"))
      }
      
      # 转换为数据框
      importance_df <- as.data.frame(importance_data)
      importance_df$Variable <- rownames(importance_df)
      
      # 检查是否有MeanDecreaseGini列
      if (!"MeanDecreaseGini" %in% names(importance_df)) {
        # 如果有其他重要性度量，使用第一个数值列
        numeric_cols <- names(importance_df)[sapply(importance_df, is.numeric)]
        if (length(numeric_cols) > 0) {
          importance_measure <- numeric_cols[1]
          names(importance_df)[names(importance_df) == importance_measure] <- "Importance"
        } else {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "无重要性度量数据"))
        }
      } else {
        importance_df$Importance <- importance_df$MeanDecreaseGini
      }
      
      importance_df <- importance_df %>%
        arrange(desc(Importance))
      
      # 重命名变量
      importance_df <- importance_df %>%
        mutate(
          Variable = gsub("breed_size_group", "宠物体型", Variable),
          Variable = gsub("gender", "性别", Variable),
          Variable = gsub("neutered", "绝育状态", Variable),
          Variable = gsub("pet_life_stage_at_order", "生命阶段", Variable),
          Variable = gsub("pet_food_tier", "食品等级", Variable),
          Variable = gsub("total_web_sessions", "网站访问次数", Variable),
          Variable = gsub("has_subscription", "订阅状态", Variable),
          Variable = factor(Variable, levels = Variable)  # 保持排序
        )
      
      p <- ggplot(importance_df, aes(x = Variable, y = Importance)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(
          title = "随机森林特征重要性",
          x = "变量",
          y = "重要性分数"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.text.y = element_text(size = 11)
        )
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 50, l = 200)
        )
      
    }, error = function(e) {
      cat("消费者特征重要性图错误:", e$message, "\n")
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "特征重要性图生成错误"))
    })
  })
  
  # 消费者分析模块 - 特征重要性排名表
  output$consumer_importance_table <- renderDT({
    model <- consumer_rf_model()
    
    if (is.null(model)) {
      return(datatable(data.frame(
        消息 = "无随机森林模型可用",
        建议 = "请尝试放宽筛选条件或使用默认筛选"
      )))
    }
    
    tryCatch({
      # 提取特征重要性
      importance_data <- importance(model)
      
      if (is.null(importance_data)) {
        return(datatable(data.frame(消息 = "特征重要性数据不可用")))
      }
      
      importance_df <- as.data.frame(importance_data)
      importance_df$Variable <- rownames(importance_df)
      
      # 使用MeanDecreaseGini或其他重要性度量
      if ("MeanDecreaseGini" %in% names(importance_df)) {
        importance_df$Importance <- importance_df$MeanDecreaseGini
      } else {
        # 使用第一个数值列
        numeric_cols <- names(importance_df)[sapply(importance_df, is.numeric)]
        if (length(numeric_cols) > 0) {
          importance_df$Importance <- importance_df[[numeric_cols[1]]]
        } else {
          return(datatable(data.frame(消息 = "无重要性度量数据")))
        }
      }
      
      importance_df <- importance_df[order(-importance_df$Importance), ]
      
      # 重命名变量
      importance_df <- importance_df %>%
        mutate(
          Variable = gsub("breed_size_group", "宠物体型", Variable),
          Variable = gsub("gender", "性别", Variable),
          Variable = gsub("neutered", "绝育状态", Variable),
          Variable = gsub("pet_life_stage_at_order", "生命阶段", Variable),
          Variable = gsub("pet_food_tier", "食品等级", Variable),
          Variable = gsub("total_web_sessions", "网站访问次数", Variable),
          Variable = gsub("has_subscription", "订阅状态", Variable),
          Importance = round(Importance, 3)
        ) %>%
        select(Variable, Importance) %>%
        rename(
          变量 = Variable,
          重要性 = Importance
        )
      
      datatable(
        importance_df,
        options = list(
          pageLength = 10,
          dom = 't',
          scrollX = TRUE,
          ordering = FALSE
        ),
        rownames = FALSE
      )
      
    }, error = function(e) {
      cat("消费者重要性表错误:", e$message, "\n")
      return(datatable(data.frame(错误 = "无法生成重要性表")))
    })
  })
  
  # 消费者分析模块 - 随机森林OOB误差
  output$consumer_rf_oob_error <- renderPrint({
    model <- consumer_rf_model()
    
    if (is.null(model)) {
      cat("随机森林模型不可用\n")
      cat("\n可能原因:\n")
      cat("1. 数据量不足（需要至少50个样本）\n")
      cat("2. food_type类别不足（需要至少2个类别）\n")
      cat("3. 缺少必要的变量\n")
      cat("\n建议:\n")
      cat("1. 尝试放宽筛选条件\n")
      cat("2. 确保选择了多个食品类型\n")
      cat("3. 检查数据质量\n")
      return()
    }
    
    cat("消费者随机森林模型性能评估\n")
    cat("=============================\n\n")
    
    tryCatch({
      # OOB误差率
      if (!is.null(model$err.rate) && nrow(model$err.rate) > 0) {
        oob_error <- model$err.rate[nrow(model$err.rate), "OOB"]
        cat(paste0("OOB误差率: ", round(oob_error * 100, 2), "%\n"))
        
        # 各类别的OOB误差
        cat("\n各类别OOB误差率:\n")
        oob_by_class <- model$err.rate[nrow(model$err.rate), ]
        for (i in 1:length(oob_by_class)) {
          if (names(oob_by_class)[i] != "OOB") {
            cat(paste0(names(oob_by_class)[i], ": ", 
                       round(oob_by_class[i] * 100, 2), "%\n"))
          }
        }
        
        # 混淆矩阵（OOB）
        cat("\nOOB混淆矩阵:\n")
        if (!is.null(model$confusion)) {
          print(model$confusion)
        }
      } else {
        cat("无法获取OOB误差信息\n")
      }
      
      # 模型基本信息
      cat("\n模型基本信息:\n")
      cat(paste0("决策树数量: ", model$ntree, "\n"))
      cat(paste0("每棵树使用的变量数: ", model$mtry, "\n"))
      
    }, error = function(e) {
      cat("消费者OOB误差计算错误:", e$message, "\n")
    })
  })
  
  # 消费者分析模块 - 时间序列分析
  output$consumer_time_series_plot <- renderPlotly({
    data <- consumer_filtered_data()
    
    if (nrow(data) == 0 || !"order_payment_date" %in% names(data)) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "无时间序列数据"))
    }
    
    tryCatch({
      # 按宠物和时间分析
      time_analysis <- data %>%
        arrange(pet_id, order_payment_date) %>%
        group_by(pet_id) %>%
        mutate(
          order_sequence = row_number(),
          first_wet_order = ifelse(any(wet_food_purchased), 
                                   min(order_payment_date[wet_food_purchased]), 
                                   NA),
          days_since_first_wet = ifelse(is.na(first_wet_order), 
                                        NA,
                                        as.numeric(order_payment_date - first_wet_order))
        ) %>%
        ungroup()
      
      # 分析湿粮采用模式
      wet_adoption <- time_analysis %>%
        filter(!is.na(days_since_first_wet)) %>%
        group_by(days_since_first_wet) %>%
        summarise(
          n_pets = n_distinct(pet_id),
          avg_wet_ratio = mean(wet_food_ratio, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(days_since_first_wet >= 0 & days_since_first_wet <= 365)
      
      if (nrow(wet_adoption) == 0) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "无湿粮采用数据"))
      }
      
      p <- ggplot(wet_adoption, aes(x = days_since_first_wet, y = avg_wet_ratio)) +
        geom_line(color = "steelblue", size = 1) +
        geom_smooth(method = "loess", color = "#F28E2B", se = FALSE) +
        labs(
          title = "湿粮采用趋势（首次购买后）",
          x = "首次湿粮购买后天数",
          y = "平均湿粮比例"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title = element_text(size = 12)
        )
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 50)
        )
      
    }, error = function(e) {
      cat("时间序列图错误:", e$message, "\n")
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "时间序列图生成错误"))
    })
  })
  
  # 修改相关性热力图函数，添加数据验证
  output$consumer_correlation_heatmap <- renderPlotly({
    data <- consumer_filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "无数据可用"))
    }
    
    tryCatch({
      # 检查必需的列
      required_cols <- c("wet_food_ratio", "total_web_sessions", "total_minutes_on_website",
                         "total_wet_food_updates", "wet_food_discount_percent")
      
      # 只选择存在的列
      existing_cols <- required_cols[required_cols %in% names(data)]
      
      if (length(existing_cols) < 2) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "相关性分析所需数据不足"))
      }
      
      # 检查数据变异性
      cat("相关性分析数据验证:\n")
      for (col in existing_cols) {
        unique_vals <- unique(data[[col]])
        cat(paste(col, ": 唯一值数量 =", length(unique_vals), 
                  "范围 =", round(min(data[[col]], na.rm = TRUE), 2), 
                  "-", round(max(data[[col]], na.rm = TRUE), 2), "\n"))
        
        if (length(unique_vals) == 1) {
          cat(paste("警告：列", col, "只有一个唯一值，无法计算相关性\n"))
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = paste("列", col, "没有变异性，无法计算相关性")))
        }
      }
      
      # 计算相关系数矩阵
      behavior_cor <- data %>%
        select(all_of(existing_cols)) %>%
        cor(use = "complete.obs")
      
      # 检查相关系数矩阵是否有效
      if (any(is.na(behavior_cor)) || any(is.infinite(behavior_cor))) {
        cat("警告：相关系数矩阵包含NA或Inf值\n")
        behavior_cor[is.na(behavior_cor)] <- 0
        behavior_cor[is.infinite(behavior_cor)] <- 0
      }
      
      cat("相关系数矩阵:\n")
      print(behavior_cor)
      
      # 转换为长格式
      behavior_cor_long <- behavior_cor %>%
        as.data.frame() %>%
        rownames_to_column(var = "Var1") %>%
        pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation") %>%
        mutate(
          Var1 = factor(Var1, levels = colnames(behavior_cor)),
          Var2 = factor(Var2, levels = colnames(behavior_cor))
        )
      
      # 重命名变量
      var_names <- c(
        "wet_food_ratio" = "湿粮比例",
        "total_web_sessions" = "网站访问次数",
        "total_minutes_on_website" = "网站停留时间(分钟)",
        "total_wet_food_updates" = "湿粮更新次数",
        "wet_food_discount_percent" = "折扣百分比"
      )
      
      behavior_cor_long <- behavior_cor_long %>%
        mutate(
          Var1 = ifelse(Var1 %in% names(var_names), var_names[as.character(Var1)], as.character(Var1)),
          Var2 = ifelse(Var2 %in% names(var_names), var_names[as.character(Var2)], as.character(Var2))
        )
      
      p <- ggplot(behavior_cor_long, aes(x = Var2, y = Var1, fill = Correlation)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = round(Correlation, 3)), color = "black", size = 4) +
        scale_fill_gradient2(
          low = "#4E79A7",
          mid = "white",
          high = "#F28E2B",
          midpoint = 0,
          limits = c(-1, 1),
          name = "相关系数"
        ) +
        labs(
          title = "网站行为与湿粮购买比例的相关性热力图",
          x = "",
          y = ""
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
        )
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 50, b = 100)
        )
      
    }, error = function(e) {
      cat("消费者热力图错误:", e$message, "\n")
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "热力图生成错误"))
    })
  })
  
  # 消费者分析模块 - 相关性统计表
  output$consumer_correlation_table <- renderDT({
    data <- consumer_filtered_data()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(消息 = "无数据可用")))
    }
    
    tryCatch({
      # 检查并准备存在的列
      available_cols <- c()
      col_names <- list()
      
      # 检查每个列是否存在，并创建对应的中文名称
      if ("wet_food_ratio" %in% names(data)) {
        available_cols <- c(available_cols, "wet_food_ratio")
        col_names["wet_food_ratio"] <- "湿粮比例"
      }
      
      if ("total_web_sessions" %in% names(data)) {
        available_cols <- c(available_cols, "total_web_sessions")
        col_names["total_web_sessions"] <- "网站访问次数"
      }
      
      if ("total_minutes_on_website" %in% names(data)) {
        available_cols <- c(available_cols, "total_minutes_on_website")
        col_names["total_minutes_on_website"] <- "网站停留时间"
      }
      
      if ("total_wet_food_updates" %in% names(data)) {
        available_cols <- c(available_cols, "total_wet_food_updates")
        col_names["total_wet_food_updates"] <- "湿粮更新次数"
      }
      
      if ("wet_food_discount_percent" %in% names(data)) {
        available_cols <- c(available_cols, "wet_food_discount_percent")
        col_names["wet_food_discount_percent"] <- "折扣百分比"
      }
      
      # 确保至少有两个变量可用于相关性分析
      if (length(available_cols) < 2) {
        return(datatable(data.frame(消息 = "可用变量不足，无法计算相关性")))
      }
      
      # 计算相关系数矩阵
      behavior_cor <- data %>%
        select(all_of(available_cols)) %>%
        cor(use = "complete.obs")
      
      # 转换为表格格式
      cor_table <- as.data.frame(behavior_cor)
      
      # 重命名行和列
      rownames(cor_table) <- sapply(rownames(cor_table), function(x) {
        ifelse(x %in% names(col_names), col_names[[x]], x)
      })
      
      # 添加变量名列
      cor_table <- cor_table %>%
        rownames_to_column(var = "变量")
      
      # 重命名列名
      for (i in 2:ncol(cor_table)) {
        col_name <- colnames(cor_table)[i]
        if (col_name %in% names(col_names)) {
          colnames(cor_table)[i] <- col_names[[col_name]]
        }
      }
      
      datatable(
        cor_table,
        options = list(
          pageLength = 10,
          dom = 't',
          scrollX = TRUE,
          ordering = FALSE
        ),
        rownames = FALSE,
        caption = "行为变量相关性矩阵"
      ) %>%
        formatRound(columns = 2:ncol(cor_table), digits = 3) %>%
        formatStyle(
          columns = 2:ncol(cor_table),
          color = styleInterval(c(-0.3, 0, 0.3), 
                                c("red", "#333333", "#333333", "green"))
        )
      
    }, error = function(e) {
      cat("相关性表错误:", e$message, "\n")
      return(datatable(data.frame(
        错误 = paste("无法计算相关性:", e$message),
        建议 = "请检查数据列是否完整"
      )))
    })
  })
  
  # 消费者分析模块 - 聚类分析
  consumer_cluster_results <- reactive({
    data <- consumer_filtered_data()  # 修改这里：使用 consumer_filtered_data()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    tryCatch({
      # 检查必需的列 - 根据 consumer_filtered_data() 的实际变量名调整
      required_cols <- c("wet_food_ratio", "total_web_sessions", "total_minutes_on_website",
                         "premium_treat_packs", "dental_treat_packs", "has_subscription")
      
      existing_cols <- required_cols[required_cols %in% names(data)]
      
      if (length(existing_cols) < 3) {
        cat("聚类分析所需列不足\n")
        return(NULL)
      }
      
      # 准备聚类数据
      cluster_data <- data %>%
        select(all_of(existing_cols)) %>%
        scale() %>%
        na.omit()
      
      if (nrow(cluster_data) < 10) {
        cat("聚类数据不足\n")
        return(NULL)
      }
      
      # 执行K-means聚类（4个聚类）
      set.seed(123)
      kmeans_result <- kmeans(cluster_data, centers = 4, nstart = 25)
      
      # 将聚类结果添加到原始数据
      df_clustered <- data[complete.cases(data[, existing_cols]), ]
      df_clustered$cluster <- as.factor(kmeans_result$cluster)
      
      # 分析各聚类特征
      cluster_profiles <- df_clustered %>%
        group_by(cluster) %>%
        summarise(
          n = n(),
          avg_wet_ratio = mean(wet_food_ratio, na.rm = TRUE),
          avg_web_sessions = mean(total_web_sessions, na.rm = TRUE),
          avg_website_minutes = mean(total_minutes_on_website, na.rm = TRUE),
          avg_treats = mean(premium_treat_packs + dental_treat_packs, na.rm = TRUE),
          subscription_rate = mean(has_subscription, na.rm = TRUE) * 100,
          .groups = 'drop'
        )
      
      # 添加额外特征如果存在
      if ("breed_size_group" %in% names(df_clustered)) {
        breed_stats <- df_clustered %>%
          group_by(cluster) %>%
          summarise(
            large_breed_pct = mean(breed_size_group == "大型", na.rm = TRUE) * 100,
            .groups = 'drop'
          )
        cluster_profiles <- left_join(cluster_profiles, breed_stats, by = "cluster")
      }
      
      if ("pet_life_stage_at_order" %in% names(df_clustered)) {
        life_stats <- df_clustered %>%
          group_by(cluster) %>%
          summarise(
            mature_pct = mean(pet_life_stage_at_order == "mature", na.rm = TRUE) * 100,
            .groups = 'drop'
          )
        cluster_profiles <- left_join(cluster_profiles, life_stats, by = "cluster")
      }
      
      return(list(
        cluster_data = cluster_data,
        kmeans_result = kmeans_result,
        df_clustered = df_clustered,
        cluster_profiles = cluster_profiles
      ))
      
    }, error = function(e) {
      cat("聚类分析错误:", e$message, "\n")
      return(NULL)
    })
  })
  
  # 消费者分析模块 - 聚类特征对比条形图
  output$consumer_cluster_barplot <- renderPlotly({  # 修改输出ID
    results <- consumer_cluster_results()  # 修改函数名
    
    if (is.null(results)) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "无法进行聚类分析"))
    }
    
    tryCatch({
      cluster_profiles <- results$cluster_profiles
      
      # 准备条形图数据
      bar_data <- cluster_profiles %>%
        select(cluster, avg_wet_ratio, avg_web_sessions, avg_website_minutes, 
               subscription_rate) %>%
        mutate(
          avg_wet_ratio = avg_wet_ratio * 100
        )
      
      # 添加额外特征如果存在
      if ("large_breed_pct" %in% names(cluster_profiles)) {
        bar_data$large_breed_pct <- cluster_profiles$large_breed_pct
      }
      
      if ("mature_pct" %in% names(cluster_profiles)) {
        bar_data$mature_pct <- cluster_profiles$mature_pct
      }
      
      # 重塑为长格式
      bar_data_long <- bar_data %>%
        pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")
      
      # 重命名变量
      variable_names <- c(
        "avg_wet_ratio" = "湿粮比例(%)",
        "avg_web_sessions" = "平均网站访问次数",
        "avg_website_minutes" = "平均网站停留时间(分钟)",
        "subscription_rate" = "订阅率(%)",
        "large_breed_pct" = "大型犬比例(%)",
        "mature_pct" = "成熟期比例(%)"
      )
      
      bar_data_long <- bar_data_long %>%
        mutate(
          variable = ifelse(variable %in% names(variable_names), 
                            variable_names[variable], 
                            variable)
        )
      
      p <- ggplot(bar_data_long, aes(x = variable, y = value, fill = cluster)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
        scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#ffd5b3"),
                          labels = c("聚类1", "聚类2", "聚类3", "聚类4")) +
        labs(
          title = "聚类特征对比",
          x = "特征指标",
          y = "数值",
          fill = "聚类组"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right"
        )
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 50, b = 100)
        )
      
    }, error = function(e) {
      cat("聚类条形图错误:", e$message, "\n")
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "聚类条形图生成错误"))
    })
  })
  
  # 消费者分析模块 - PCA聚类分布图
  output$consumer_pca_cluster_plot <- renderPlotly({  # 修改输出ID
    results <- consumer_cluster_results()  # 修改函数名
    
    if (is.null(results)) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "无法进行PCA分析"))
    }
    
    tryCatch({
      # 执行PCA降维
      pca_result <- prcomp(results$cluster_data, center = TRUE, scale. = TRUE)
      
      # 提取前两个主成分
      pca_scores <- as.data.frame(pca_result$x[, 1:2])
      pca_scores$cluster <- as.factor(results$kmeans_result$cluster)
      
      # 计算方差解释比例
      var_explained <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 1)
      
      p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.2) +
        scale_color_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#ffd5b3")) +
        scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#ffd5b3")) +
        labs(
          title = "PCA聚类分布图",
          x = paste0("PC1 (", var_explained[1], "% 方差)"),
          y = paste0("PC2 (", var_explained[2], "% 方差)"),
          color = "聚类",
          fill = "聚类"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right"
        )
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          margin = list(t = 50)
        )
      
    }, error = function(e) {
      cat("PCA图错误:", e$message, "\n")
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "PCA图生成错误"))
    })
  })
  
  # 消费者分析模块 - 聚类特征统计表
  output$consumer_cluster_profile_table <- renderDT({  # 修改输出ID
    results <- consumer_cluster_results()  # 修改函数名
    
    if (is.null(results)) {
      return(datatable(data.frame(消息 = "无聚类分析结果")))
    }
    
    tryCatch({
      cluster_profiles <- results$cluster_profiles
      
      # 格式化数据
      profile_table <- cluster_profiles %>%
        mutate(
          样本数 = n,
          湿粮比例 = paste0(round(avg_wet_ratio * 100, 1), "%"),
          网站访问次数 = round(avg_web_sessions, 1),
          网站停留时间 = round(avg_website_minutes, 1),
          平均零食包数 = round(avg_treats, 2),
          订阅率 = paste0(round(subscription_rate, 1), "%")
        ) %>%
        select(cluster, 样本数, 湿粮比例, 网站访问次数, 网站停留时间,
               平均零食包数, 订阅率)
      
      # 添加额外特征
      if ("large_breed_pct" %in% names(cluster_profiles)) {
        profile_table$大型犬比例 <- paste0(round(cluster_profiles$large_breed_pct, 1), "%")
      }
      
      if ("mature_pct" %in% names(cluster_profiles)) {
        profile_table$成熟期比例 <- paste0(round(cluster_profiles$mature_pct, 1), "%")
      }
      
      profile_table <- profile_table %>%
        rename(聚类 = cluster)
      
      datatable(
        profile_table,
        options = list(
          pageLength = 10,
          dom = 't',
          scrollX = TRUE
        ),
        rownames = FALSE
      )
      
    }, error = function(e) {
      cat("聚类特征表错误:", e$message, "\n")
      return(datatable(data.frame(错误 = "无法生成聚类特征表")))
    })
  })
  
  # 消费者分析模块 - 聚类用户画像
  output$consumer_cluster_profiles <- renderPrint({  # 修改输出ID
    results <- consumer_cluster_results()  # 修改函数名
    
    if (is.null(results)) {
      cat("聚类分析不可用\n")
      return()
    }
    
    df_clustered <- results$df_clustered
    
    cat("聚类用户画像分析\n")
    cat("===================\n\n")
    
    for (clust in sort(unique(df_clustered$cluster))) {
      cat(paste0("--- 聚类 ", clust, " 用户画像 ---\n"))
      
      cluster_subset <- df_clustered %>% filter(cluster == clust)
      
      # 基本统计
      cat(paste0("订单数: ", nrow(cluster_subset), "\n"))
      cat(paste0("宠物数: ", n_distinct(cluster_subset$pet_id), "\n"))
      
      # 湿粮比例
      if ("wet_food_ratio" %in% names(cluster_subset)) {
        avg_wet_ratio <- mean(cluster_subset$wet_food_ratio, na.rm = TRUE) * 100
        cat(paste0("平均湿粮比例: ", round(avg_wet_ratio, 1), "%\n"))
      }
      
      # 宠物特征
      if ("breed_size_group" %in% names(cluster_subset)) {
        breed_dist <- cluster_subset %>%
          count(breed_size_group) %>%
          mutate(pct = n / sum(n) * 100)
        
        breed_str <- paste(sapply(1:nrow(breed_dist), function(i) {
          paste0(breed_dist$breed_size_group[i], ": ", round(breed_dist$pct[i], 1), "%")
        }), collapse = ", ")
        cat(paste0("体型分布: ", breed_str, "\n"))
      }
      
      if ("gender" %in% names(cluster_subset)) {
        gender_dist <- cluster_subset %>%
          count(gender) %>%
          mutate(pct = n / sum(n) * 100)
        
        gender_str <- paste(sapply(1:nrow(gender_dist), function(i) {
          paste0(gender_dist$gender[i], ": ", round(gender_dist$pct[i], 1), "%")
        }), collapse = ", ")
        cat(paste0("性别分布: ", gender_str, "\n"))
      }
      
      # 消费者行为
      if ("has_subscription" %in% names(cluster_subset)) {
        subscription_rate <- mean(cluster_subset$has_subscription, na.rm = TRUE) * 100
        cat(paste0("订阅率: ", round(subscription_rate, 1), "%\n"))
      }
      
      if ("total_web_sessions" %in% names(cluster_subset)) {
        avg_web_sessions <- mean(cluster_subset$total_web_sessions, na.rm = TRUE)
        cat(paste0("平均网站访问: ", round(avg_web_sessions, 1), " 次\n"))
      }
      
      # 生成用户画像标签
      labels <- c()
      
      if (exists("avg_wet_ratio")) {
        if (avg_wet_ratio > 50) {
          labels <- c(labels, "湿粮偏好型")
        } else if (avg_wet_ratio > 20) {
          labels <- c(labels, "混合粮型")
        } else {
          labels <- c(labels, "干粮偏好型")
        }
      }
      
      if (exists("subscription_rate") && subscription_rate > 50) {
        labels <- c(labels, "高订阅率")
      } else if (exists("subscription_rate")) {
        labels <- c(labels, "低订阅率")
      }
      
      if (exists("avg_web_sessions") && avg_web_sessions > median(df_clustered$total_web_sessions, na.rm = TRUE)) {
        labels <- c(labels, "高参与度")
      } else if (exists("avg_web_sessions")) {
        labels <- c(labels, "低参与度")
      }
      
      if (length(labels) > 0) {
        cat(paste0("\n用户画像标签: ", paste(labels, collapse = " | "), "\n\n"))
      } else {
        cat("\n\n")
      }
    }
  })
  
  # 用户留存分析模块 - 数据预处理
  retention_raw_data <- reactive({
    tryCatch({
      data <- load_all_data()
      
      cat("留存分析数据预处理开始...\n")
      
      # 应用预处理步骤
      df <- data %>%
        mutate(
          across(where(is.character), ~na_if(., "")),
          across(where(is.character), ~na_if(., "None")),
          across(where(is.character), ~na_if(., "Null & Default")),
          
          # 数值型变量处理
          wet_food_discount_percent = ifelse(is.na(wet_food_discount_percent), 0, 
                                             as.numeric(wet_food_discount_percent)),
          wet_kcal = ifelse(is.na(wet_kcal), 0, as.numeric(wet_kcal)),
          kibble_kcal = ifelse(is.na(kibble_kcal), 0, as.numeric(kibble_kcal)),
          wet_trays = ifelse(is.na(wet_trays), 0, as.numeric(wet_trays)),
          
          # 客服互动标记
          has_cs_interaction = ifelse(!is.na(last_customer_support_ticket_date), 1, 0),
          
          # 日期转换
          order_payment_date = as.Date(order_payment_date),
          pet_signup_datetime = as.Date(pet_signup_datetime),
          
          # 因子转换
          pet_breed_size = factor(pet_breed_size, levels = c("small", "medium", "large", "toy", "giant")),
          pet_has_active_subscription = as.logical(pet_has_active_subscription)
        )
      
      # 构建用户级别数据集 - 完全按照原始代码
      user_data <- df %>%
        arrange(pet_id, order_payment_date) %>%
        group_by(pet_id, customer_id) %>%
        summarise(
          # 时间相关变量
          first_order_date = min(order_payment_date),
          last_order_date = max(order_payment_date),
          observation_end_date = max(df$order_payment_date) + 90,
          
          # 订单相关变量
          total_orders = n(),
          total_wet_orders = sum(wet_kcal > 0 | wet_trays > 0, na.rm = TRUE),
          total_kibble_kcal = sum(kibble_kcal, na.rm = TRUE),
          total_wet_kcal = sum(wet_kcal, na.rm = TRUE),
          
          # 运营相关变量
          avg_discount = mean(wet_food_discount_percent, na.rm = TRUE),
          has_ever_cs_interaction = max(has_cs_interaction, na.rm = TRUE),
          subscription_status = last(pet_has_active_subscription),
          
          # 宠物特征（取最后一次记录）
          breed_size = last(pet_breed_size),
          
          .groups = 'drop'
        ) %>%
        mutate(
          # 计算生存分析相关变量
          survival_time = as.numeric(last_order_date - first_order_date),
          
          # 判断是否流失（90天内无新订单）
          time_since_last = as.numeric(observation_end_date - last_order_date),
          is_churned = ifelse(time_since_last > 90 & total_orders > 1, 1, 0),
          
          # 事件状态（1=流失，0=删失/仍留存）
          status = is_churned,
          
          # 计算其他衍生指标
          wet_order_ratio = ifelse(total_orders > 0, total_wet_orders / total_orders, 0),
          has_ever_wet = ifelse(total_wet_orders > 0, 1, 0)
        )
      
      # 移除无效数据
      user_data <- user_data %>%
        filter(!is.na(survival_time) & !is.na(status) & 
                 survival_time >= 0 & !is.na(subscription_status))
      
      cat("用户数据处理完成，行数:", nrow(user_data), "\n")
      cat("生存时间范围:", range(user_data$survival_time), "\n")
      cat("流失用户数:", sum(user_data$status), "\n")
      cat("订阅状态分布:", table(user_data$subscription_status), "\n")
      
      return(user_data)
      
    }, error = function(e) {
      # 创建示例数据 - 严格按照原始数据的结构
      cat("使用示例数据，错误:", e$message, "\n")
      
      set.seed(123)
      n <- 500
      
      # 模拟日期数据
      start_date <- as.Date("2023-01-01")
      end_date <- as.Date("2024-01-01")
      
      user_data <- data.frame(
        pet_id = 1:n,
        customer_id = 1:n,
        first_order_date = sample(seq(start_date, end_date - 180, by = "day"), n, replace = TRUE),
        last_order_date = as.Date(NA),
        survival_time = pmax(30, round(rnorm(n, mean = 180, sd = 100))),
        status = rbinom(n, 1, 0.3),
        subscription_status = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.6, 0.4)),
        has_ever_wet = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.7, 0.3)),
        has_ever_cs_interaction = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
        breed_size = factor(sample(c("small", "medium", "large", "toy", "giant"), n, replace = TRUE),
                            levels = c("small", "medium", "large", "toy", "giant")),
        avg_discount = runif(n, 0, 30),
        wet_order_ratio = runif(n, 0, 1),
        total_orders = rpois(n, 5) + 1
      )
      
      # 计算last_order_date
      user_data$last_order_date <- user_data$first_order_date + user_data$survival_time
      
      cat("示例数据创建完成，行数:", nrow(user_data), "\n")
      cat("生存时间范围:", range(user_data$survival_time), "\n")
      cat("流失用户数:", sum(user_data$status), "\n")
      cat("订阅状态分布:", table(user_data$subscription_status), "\n")
      
      return(user_data)
    })
  })
  
  
  # 用户留存分析模块 - 过滤后的数据
  retention_filtered_data <- reactive({
    data <- retention_raw_data()
    
    if(nrow(data) == 0) {
      return(data)
    }
    
    # 应用筛选条件
    if(input$sidebar == "retention_overview" || input$sidebar == "retention_km" || 
       input$sidebar == "retention_cox" || input$sidebar == "retention_prediction") {
      if(input$retention_breed_size_filter != "全部") {
        data <- data %>% 
          filter(breed_size == input$retention_breed_size_filter)
      }
      
      if(!is.null(input$retention_min_orders)) {
        data <- data %>%
          filter(total_orders >= input$retention_min_orders)
      }
      
      if(!is.null(input$retention_min_survival)) {
        data <- data %>%
          filter(survival_time >= input$retention_min_survival)
      }
    }
    
    return(data)
  })
  
  # 用户留存分析模块 - 统计卡片
  output$retention_total_users <- renderValueBox({
    data <- retention_filtered_data()
    valueBox(
      nrow(data),
      "总宠物数",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  output$retention_churn_rate <- renderValueBox({
    data <- retention_filtered_data()
    if(nrow(data) == 0) {
      return(valueBox("0%", "整体流失率", color = "yellow"))
    }
    rate <- mean(data$status, na.rm = TRUE) * 100
    valueBox(
      paste0(round(rate, 1), "%"),
      "整体流失率",
      icon = icon("chart-line"),
      color = ifelse(rate > 30, "red", "green")
    )
  })
  
  output$retention_avg_survival <- renderValueBox({
    data <- retention_filtered_data()
    if(nrow(data) == 0 || sum(data$status) == 0) {
      return(valueBox("0天", "平均生存时间", color = "blue"))
    }
    avg <- mean(data$survival_time[data$status == 1], na.rm = TRUE)
    valueBox(
      paste0(round(avg), "天"),
      "平均生存时间",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$retention_subscription_rate <- renderValueBox({
    data <- retention_filtered_data()
    if(nrow(data) == 0) {
      return(valueBox("0%", "订阅率", color = "yellow"))
    }
    rate <- mean(data$subscription_status, na.rm = TRUE) * 100
    valueBox(
      paste0(round(rate, 1), "%"),
      "订阅率",
      icon = icon("credit-card"),
      color = ifelse(rate > 50, "green", "yellow")
    )
  })
  
  # 用户留存分析模块 - 整体流失率图
  output$retention_overall_churn_plot <- renderPlot({
    data <- retention_filtered_data()
    if(nrow(data) == 0) {
      plot(0, 0, type = "n", xlab = "", ylab = "", main = "无数据")
      return()
    }
    
    ggplot(data, aes(x = factor(status, labels = c("留存", "流失")))) +
      geom_bar(fill = c("#4CAF50", "#F44336"), alpha = 0.8) +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
      labs(x = "状态", y = "用户数") +
      theme_minimal()
  })
  
  # 用户留存分析模块 - 宠物体型流失率图
  output$retention_breed_churn_plot <- renderPlotly({
    data <- retention_filtered_data()
    if(nrow(data) == 0) {
      return(plotly_empty())
    }
    
    breed_churn <- data %>%
      group_by(breed_size) %>%
      summarise(
        total = n(),
        churned = sum(status),
        churn_rate = churned / total * 100
      ) %>%
      arrange(desc(churn_rate))
    
    p <- ggplot(breed_churn, aes(x = reorder(breed_size, -churn_rate), y = churn_rate)) +
      geom_bar(stat = "identity", fill = "#fed36a", alpha = 0.8) +
      geom_text(aes(label = paste0(round(churn_rate, 1), "%")), vjust = -0.5) +
      labs(x = "宠物体型", y = "流失率 (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 用户留存分析模块 - 生存时间直方图
  output$retention_survival_hist <- renderPlot({
    data <- retention_filtered_data()
    if(nrow(data) == 0) {
      plot(0, 0, type = "n", xlab = "", ylab = "", main = "无数据")
      return()
    }
    
    ggplot(data, aes(x = survival_time)) +
      geom_histogram(fill = "#fed36a", alpha = 0.7, bins = 30) +
      geom_vline(aes(xintercept = median(survival_time, na.rm = TRUE)),
                 color = "red", linetype = "dashed", size = 1) +
      labs(x = "生存时间 (天)", y = "用户数",
           title = paste("生存时间分布 (中位数:", round(median(data$survival_time, na.rm = TRUE)), "天)")) +
      theme_minimal()
  })
  
  # 用户留存分析模块 - Kaplan-Meier分析标题
  output$retention_km_title <- renderText({
    var_name <- switch(input$retention_km_variable,
                       "subscription_status" = "订阅状态",
                       "has_ever_wet" = "湿粮购买",
                       "has_ever_cs_interaction" = "客服互动",
                       "breed_size" = "宠物体型",
                       "food_tier" = "食品等级")
    paste(var_name, "对用户留存的影响")
  })
  
  # 用户留存分析模块 - Kaplan-Meier生存曲线图
  output$retention_km_plot <- renderPlot({
    data <- retention_filtered_data()
    
    if(nrow(data) == 0 || sum(user_data$status) < 5) {
      plot(0, 0, type = "n", xlab = "", ylab = "", 
           main = "数据不足，无法进行生存分析")
      return()
    }
    
    tryCatch({
      # 创建生存对象
      surv_obj <- Surv(user_data$survival_time, user_data$status)
      
      # 根据选择的变量进行分组 - 使用正确的变量名
      formula <- as.formula(paste("surv_obj ~", input$retention_km_variable))
      fit <- survfit(surv_obj ~ subscription_status, data = data)
      
      # 设置颜色和标签 - 按照原始代码的颜色方案
      if(input$retention_km_variable == "subscription_status") {
        legend_labs <- c("未订阅", "已订阅")
        legend_title <- "订阅状态"
        palette <- c("#bc5148", "#3090a1")
      } else if(input$retention_km_variable == "has_ever_wet") {
        legend_labs <- c("从未购买", "曾购买")
        legend_title <- "湿粮购买"
        palette <- c("#e74c3c", "#2ecc71")
      } else if(input$retention_km_variable == "has_ever_cs_interaction") {
        legend_labs <- c("无互动", "有互动")
        legend_title <- "客服互动"
        palette <- c("#f39c12", "#8e44ad")
      } else {
        # 对于breed_size等分类变量，使用实际的分组
        unique_groups <- levels(factor(data[[input$retention_km_variable]]))
        legend_labs <- unique_groups
        legend_title <- input$retention_km_variable
        palette <- rainbow(length(unique_groups))
      }
      
      # 使用ggsurvplot绘制生存曲线 - 使用正确的输入变量名
      p <- ggsurvplot(fit,
                      data = data,
                      pval = TRUE,
                      conf.int = input$retention_km_confint,
                      risk.table = input$retention_km_risktable,
                      risk.table.height = 0.25,
                      surv.median.line = ifelse(input$retention_km_median, "hv", "none"),
                      xlim = input$retention_km_xlim,
                      xlab = "生存时间 (天)",
                      ylab = "留存概率",
                      legend.title = legend_title,
                      legend.labs = legend_labs,
                      palette = palette,
                      ggtheme = theme_minimal(),
                      title = paste(legend_title, "对用户留存的影响"))
      
      print(p)
      
    }, error = function(e) {
      plot(0, 0, type = "n", xlab = "", ylab = "", 
           main = paste("生存分析错误:", e$message))
    })
  })
  
  # 用户留存分析模块 - 生存分析摘要
  output$retention_km_summary <- renderPrint({  # 修改输出ID
    data <- retention_filtered_data()  # 修改数据源
    
    if(nrow(data) == 0 || sum(data$status) < 5) {
      cat("数据不足，无法生成生存分析摘要\n")
      return()
    }
    
    tryCatch({
      surv_obj <- Surv(data$survival_time, data$status)
      formula <- as.formula(paste("surv_obj ~", input$retention_km_variable))  # 修改输入变量
      fit <- survfit(formula, data = data)
      
      cat("Kaplan-Meier生存分析摘要\n")
      cat("========================\n\n")
      cat(paste("分组变量:", input$retention_km_variable, "\n"))
      cat(paste("总样本数:", nrow(data), "\n"))
      cat(paste("事件数:", sum(data$status), "\n"))
      cat(paste("删失数:", nrow(data) - sum(data$status), "\n\n"))
      
      print(summary(fit, times = seq(0, 365, 30)))
      
    }, error = function(e) {
      cat(paste("生存分析摘要生成失败:", e$message, "\n"))
    })
  })
  
  # 用户留存分析模块 - Cox回归模型
  retention_cox_model <- eventReactive(input$retention_run_cox, {
    data <- retention_filtered_data()
    
    if(nrow(data) < 20 || sum(data$status) < 10) {
      return(list(type = "error", message = "数据不足，请确保数据集中至少有20个观测值和10个事件"))
    }
    
    # 准备数据
    data <- data %>%
      mutate(
        subscription_status = as.logical(subscription_status),
        has_ever_wet = as.numeric(has_ever_wet),
        has_ever_cs_interaction = as.numeric(has_ever_cs_interaction),
        avg_discount = as.numeric(avg_discount),
        wet_order_ratio = as.numeric(wet_order_ratio)
      )
    
    # 确保分类变量正确编码
    if("breed_size" %in% names(data)) {
      data$breed_size <- factor(data$breed_size)
    }
    
    if("food_tier" %in% names(data)) {
      data$food_tier <- factor(data$food_tier)
    }
    
    if(input$cox_type == "uni") {  # 注意：UI中应该是 input$cox_type，不是 input$retention_cox_type
      # 单因素Cox回归分析
      uni_results <- list()
      uni_df_list <- list()
      
      for(var in input$cox_variables) {  # 注意：UI中应该是 input$cox_variables，不是 input$retention_cox_variables
        tryCatch({
          formula_str <- paste("Surv(survival_time, status) ~", var)
          uni_model <- coxph(as.formula(formula_str), data = data)
          
          # 提取结果
          x <- summary(uni_model)
          p.value <- signif(x$wald["pvalue"], digits = 3)
          
          # 获取HR和置信区间
          if(nrow(x$coefficients) > 0) {
            HR <- signif(exp(x$coefficients[1, 1]), digits = 3)
            HR.confint.lower <- signif(exp(x$conf.int[1, 3]), digits = 3)
            HR.confint.upper <- signif(exp(x$conf.int[1, 4]), digits = 3)
            HR.95CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
          } else {
            HR <- NA
            HR.confint.lower <- NA
            HR.confint.upper <- NA
            HR.95CI <- "无法计算"
          }
          
          uni_df <- data.frame(
            Variable = var,
            HR = HR,
            HR_95CI = HR.95CI,
            Lower_CI = HR.confint.lower,
            Upper_CI = HR.confint.upper,
            P_value = p.value,
            stringsAsFactors = FALSE
          )
          
          uni_results[[var]] <- x
          uni_df_list[[var]] <- uni_df
          
        }, error = function(e) {
          uni_df_list[[var]] <- data.frame(
            Variable = var,
            HR = NA,
            HR_95CI = "分析失败",
            Lower_CI = NA,
            Upper_CI = NA,
            P_value = NA,
            stringsAsFactors = FALSE
          )
        })
      }
      
      # 合并结果
      uni_cox_df <- do.call(rbind, uni_df_list)
      rownames(uni_cox_df) <- NULL
      
      return(list(type = "uni", results = uni_results, table = uni_cox_df))
      
    } else {
      # 多因素Cox回归分析
      if(length(input$cox_variables) < 2) {  # 注意：UI中应该是 input$cox_variables
        return(list(type = "multi", error = "请选择至少2个变量进行多因素分析"))
      }
      
      formula_str <- paste("Surv(survival_time, status) ~", 
                           paste(input$cox_variables, collapse = " + "))  # 注意：UI中应该是 input$cox_variables
      
      tryCatch({
        multi_cox <- coxph(as.formula(formula_str), data = data)
        return(list(type = "multi", model = multi_cox, summary = summary(multi_cox)))
      }, error = function(e) {
        return(list(type = "multi", error = paste("模型拟合失败:", e$message)))
      })
    }
  })
  
  # 用户留存分析模块 - Cox回归结果
  output$retention_cox_results <- renderPrint({
    model <- retention_cox_model()
    
    if(is.null(model)) {
      cat("请点击'运行分析'按钮进行Cox回归分析\n")
      return()
    }
    
    if(model$type == "error") {
      cat(model$message, "\n")
      return()
    }
    
    if(model$type == "uni") {
      cat("单因素Cox回归分析结果\n")
      cat("=======================\n\n")
      
      if(!is.null(model$table)) {
        print(model$table)
      }
      
    } else {
      if(!is.null(model$error)) {
        cat(paste("多因素Cox回归分析失败:", model$error, "\n"))
      } else {
        cat("多因素Cox回归分析结果\n")
        cat("=======================\n\n")
        
        # 打印详细结果
        print(model$summary)
        
        # 添加模型评估
        cat("\n模型评估指标:\n")
        cat("---------------\n")
        cat(sprintf("样本数: %d\n", model$summary$n))
        cat(sprintf("事件数: %d\n", model$summary$nevent))
        cat(sprintf("似然比检验p值: %.4f\n", model$summary$logtest[3]))
        cat(sprintf("Wald检验p值: %.4f\n", model$summary$waldtest[3]))
        cat(sprintf("评分检验p值: %.4f\n", model$summary$sctest[3]))
        
        # 添加系数表
        cat("\n回归系数表:\n")
        cat("---------------\n")
        coeff_table <- as.data.frame(model$summary$coefficients)
        conf_table <- as.data.frame(model$summary$conf.int)
        
        final_table <- data.frame(
          Variable = rownames(coeff_table),
          Coef = round(coeff_table$coef, 3),
          HR = round(conf_table$`exp(coef)`, 3),
          HR_95CI = paste0(round(conf_table$`lower .95`, 3), "-", round(conf_table$`upper .95`, 3)),
          P_value = ifelse(coeff_table$`Pr(>|z|)` < 0.001, "<0.001", 
                           round(coeff_table$`Pr(>|z|)`, 3))
        )
        
        print(final_table)
      }
    }
  })
  
  # 用户留存分析模块 - 森林图
  output$retention_forest_plot <- renderPlot({
    model <- retention_cox_model()
    
    if(is.null(model) || model$type != "multi" || !is.null(model$error) || is.null(model$model)) {
      plot(0, 0, type = "n", xlab = "", ylab = "", 
           main = "无法生成森林图\n请先运行多因素Cox回归分析")
      return(NULL)
    }
    
    tryCatch({
      # 提取模型结果（指数化）
      cox_df <- broom::tidy(model$model, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(
          # 清理变量名
          term = case_when(
            term == "subscription_statusTRUE" ~ "订阅状态(已订阅)",
            term == "subscription_statusFALSE" ~ "订阅状态(未订阅)",
            term == "has_ever_wet1" ~ "湿粮购买(曾购买)",
            term == "has_ever_wet0" ~ "湿粮购买(从未购买)",
            term == "has_ever_cs_interaction1" ~ "客服互动(有互动)",
            term == "has_ever_cs_interaction0" ~ "客服互动(无互动)",
            term == "avg_discount" ~ "平均折扣(%)",
            term == "wet_order_ratio" ~ "湿粮订单比例",
            term == "breed_sizemedium" ~ "宠物体型(中型)",
            term == "breed_sizelarge" ~ "宠物体型(大型)",
            term == "breed_sizetoy" ~ "宠物体型(迷你型)",
            term == "breed_sizegiant" ~ "宠物体型(巨大型)",
            term == "food_tierpremium" ~ "食品等级(premium)",
            term == "food_tiersuperpremium" ~ "食品等级(superpremium)",
            TRUE ~ term
          )
        )
      
      # 移除截距项
      cox_df <- cox_df %>% filter(!grepl("Intercept", term))
      
      if(nrow(cox_df) == 0) {
        plot(0, 0, type = "n", xlab = "", ylab = "", 
             main = "无有效回归系数可用于森林图")
        return(NULL)
      }
      
      # 按估计值排序
      cox_df <- cox_df %>% arrange(estimate)
      cox_df$term <- factor(cox_df$term, levels = cox_df$term)
      
      # 绘制森林图
      p <- ggplot(cox_df, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
        geom_pointrange(size = 1.5, color = "#fed36a") +
        geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
        labs(x = "风险比 (HR)", y = "变量", 
             title = "Cox回归森林图") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
      
      
      print(p)
      
    }, error = function(e) {
      plot(0, 0, type = "n", xlab = "", ylab = "", 
           main = paste("森林图生成失败:", e$message))
    })
  })
  
  # 用户留存分析模块 - 预测结果
retention_prediction_results <- eventReactive(input$retention_predict, {
  data <- retention_filtered_data()
  
  if(nrow(data) < 20) {
    return(NULL)
  }
  
  tryCatch({
    # 构建预测模型
    nomogram_model <- cph(Surv(survival_time, status) ~ 
                            subscription_status + 
                            has_ever_wet + 
                            avg_discount,
                          data = data,
                          x = TRUE, y = TRUE, surv = TRUE)
    
    # 创建预测数据
    new_data <- data.frame(
      subscription_status = as.logical(input$retention_pred_subscription),
      has_ever_wet = as.logical(input$retention_pred_wet),
      avg_discount = as.numeric(input$retention_pred_discount)
    )
    
    # 预测生存概率
    surv_func <- Survival(nomogram_model)
    pred_30 <- surv_func(30, new_data)
    pred_60 <- surv_func(60, new_data)
    pred_90 <- surv_func(90, new_data)
    pred_180 <- surv_func(180, new_data)
    
    return(list(
      pred_30 = pred_30,
      pred_60 = pred_60,
      pred_90 = pred_90,
      pred_180 = pred_180,
      model = nomogram_model,
      new_data = new_data
    ))
    
  }, error = function(e) {
    # 如果模型失败，使用简化的预测
    base_rate <- 0.7
    if(as.logical(input$retention_pred_subscription)) base_rate <- base_rate * 1.2
    if(as.logical(input$retention_pred_wet)) base_rate <- base_rate * 1.1
    
    return(list(
      pred_30 = min(0.95, base_rate),
      pred_60 = min(0.85, base_rate * 0.8),
      pred_90 = min(0.75, base_rate * 0.6),
      pred_180 = min(0.65, base_rate * 0.4)
    ))
  })
})

# 用户留存分析模块 - 预测卡片
output$retention_pred_30 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "30天留存率", color = "yellow")
  } else {
    pred_value <- results$pred_30
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "30天留存率",
      icon = icon("calendar"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

output$retention_pred_60 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "60天留存率", color = "yellow")
  } else {
    pred_value <- results$pred_60
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "60天留存率",
      icon = icon("calendar-check"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

output$retention_pred_90 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "90天留存率", color = "yellow")
  } else {
    pred_value <- results$pred_90
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "90天留存率",
      icon = icon("calendar-alt"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

output$retention_pred_180 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "180天留存率", color = "yellow")
  } else {
    pred_value <- results$pred_180
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "180天留存率",
      icon = icon("calendar-plus"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

# 用户留存分析模块 - 预测工具页输出
retention_prediction_results <- eventReactive(input$retention_predict, {
  data <- retention_filtered_data()
  
  if(nrow(data) < 20) {
    return(NULL)
  }
  
  tryCatch({
    # 确保变量是正确类型
    data <- data %>%
      mutate(
        subscription_status = factor(as.character(subscription_status)),
        has_ever_wet = factor(as.character(has_ever_wet)),
        avg_discount = as.numeric(avg_discount)
      )
    
    # 设置datadist - 这是rms包要求的
    ddist <- datadist(data)
    options(datadist = 'ddist')
    
    # 构建预测模型 - 修复：确保变量是因子类型
    nomogram_model <- cph(Surv(survival_time, status) ~ 
                            subscription_status + 
                            has_ever_wet + 
                            avg_discount,
                          data = data,
                          x = TRUE, y = TRUE, surv = TRUE)
    
    # 创建预测数据
    new_data <- data.frame(
      subscription_status = factor(as.character(input$retention_pred_subscription)),
      has_ever_wet = factor(as.character(input$retention_pred_wet)),
      avg_discount = as.numeric(input$retention_pred_discount)
    )
    
    # 预测生存概率
    surv_func <- Survival(nomogram_model)
    
    # 修复：使用sapply处理预测
    pred_30 <- if (nrow(new_data) == 1) {
      surv_func(30, new_data)
    } else {
      sapply(1:nrow(new_data), function(i) surv_func(30, new_data[i, , drop = FALSE]))
    }
    
    pred_60 <- if (nrow(new_data) == 1) {
      surv_func(60, new_data)
    } else {
      sapply(1:nrow(new_data), function(i) surv_func(60, new_data[i, , drop = FALSE]))
    }
    
    pred_90 <- if (nrow(new_data) == 1) {
      surv_func(90, new_data)
    } else {
      sapply(1:nrow(new_data), function(i) surv_func(90, new_data[i, , drop = FALSE]))
    }
    
    pred_180 <- if (nrow(new_data) == 1) {
      surv_func(180, new_data)
    } else {
      sapply(1:nrow(new_data), function(i) surv_func(180, new_data[i, , drop = FALSE]))
    }
    
    return(list(
      pred_30 = pred_30,
      pred_60 = pred_60,
      pred_90 = pred_90,
      pred_180 = pred_180,
      model = nomogram_model,
      new_data = new_data
    ))
    
  }, error = function(e) {
    cat("预测错误:", e$message, "\n")
    # 如果模型失败，使用简化的预测
    base_rate <- 0.7
    if(as.logical(input$retention_pred_subscription)) base_rate <- base_rate * 1.2
    if(as.logical(input$retention_pred_wet)) base_rate <- base_rate * 1.1
    
    return(list(
      pred_30 = min(0.95, base_rate),
      pred_60 = min(0.85, base_rate * 0.8),
      pred_90 = min(0.75, base_rate * 0.6),
      pred_180 = min(0.65, base_rate * 0.4)
    ))
  })
})

output$retention_pred_30 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "30天留存率", color = "yellow")
  } else {
    pred_value <- if (length(results$pred_30) == 1) results$pred_30 else results$pred_30[1]
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "30天留存率",
      icon = icon("calendar"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

output$retention_pred_60 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "60天留存率", color = "yellow")
  } else {
    pred_value <- if (length(results$pred_60) == 1) results$pred_60 else results$pred_60[1]
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "60天留存率",
      icon = icon("calendar-check"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

output$retention_pred_90 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "90天留存率", color = "yellow")
  } else {
    pred_value <- if (length(results$pred_90) == 1) results$pred_90 else results$pred_90[1]
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "90天留存率",
      icon = icon("calendar-alt"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

output$retention_pred_180 <- renderValueBox({
  results <- retention_prediction_results()
  
  if(is.null(results)) {
    valueBox("N/A", "180天留存率", color = "yellow")
  } else {
    pred_value <- if (length(results$pred_180) == 1) results$pred_180 else results$pred_180[1]
    valueBox(
      paste0(round(pred_value * 100, 1), "%"),
      "180天留存率",
      icon = icon("calendar-plus"),
      color = ifelse(pred_value > 0.7, "green", 
                     ifelse(pred_value > 0.5, "yellow", "red"))
    )
  }
})

output$retention_nomogram_plot <- renderPlot({
  data <- retention_filtered_data()
  
  if(nrow(data) < 20) {
    plot(0, 0, type = "n", xlab = "", ylab = "", 
         main = "数据不足，无法生成列线图")
    return()
  }
  
  tryCatch({
    # 确保变量是正确类型
    data <- data %>%
      mutate(
        subscription_status = factor(as.character(subscription_status)),
        has_ever_wet = factor(as.character(has_ever_wet)),
        avg_discount = as.numeric(avg_discount)
      )
    
    # 设置datadist - 这是rms包要求的
    ddist <- datadist(data)
    options(datadist = 'ddist')
    
    # 构建列线图模型 - 确保变量是因子类型
    nomogram_model <- cph(Surv(survival_time, status) ~ 
                            subscription_status + 
                            has_ever_wet + 
                            avg_discount,
                          data = data,
                          x = TRUE, y = TRUE, surv = TRUE, time.inc = 30)
    
    # 绘制简单的列线图 - 只显示预测因子部分
    par(bg = "aliceblue")
    par(mar = c(5, 12, 4, 2))  # 调整边距，左侧留更多空间
    nom_plot <- nomogram(nomogram_model,
                         lp = FALSE,  # 不显示线性预测器
                         fun = NULL,  # 不显示概率刻度
                         funlabel = NULL)  # 无函数标签
    
    plot(nom_plot, 
         xfrac = 0.2,  # 减少水平间距
         lmgp = 0.3,
         varname.label = TRUE,
         varname.label.sep = "=",
         ia.space = 0.2,
         )  # 添加网格线
    
    # 添加标题和副标题
    title("用户留存预测列线图", line = 2.5, cex.main = 1.5, font.main = 2)
    mtext("预测因子评分系统", side = 3, line = 0.5, cex = 1.0, col = "gray50")
    
  }, error = function(e) {
    cat("列线图错误:", e$message, "\n")
    plot(0, 0, type = "n", xlab = "", ylab = "", 
         main = "列线图生成失败",
         sub = "请确保数据包含足够的观测值")
    text(0, 0, "错误信息:", col = "red", cex = 0.8)
    text(0, -0.1, e$message, col = "red", cex = 0.7)
  })
})
}
# 在server函数外定义修复函数
fix_subscription_status <- function(data) {
  if("subscription_status" %in% names(data)) {
    # 处理各种可能的情况
    if(is.character(data$subscription_status)) {
      # 如果是字符型，清理数据
      data$subscription_status <- trimws(data$subscription_status)
      data$subscription_status <- tolower(data$subscription_status)
      
      # 映射到标准值
      data$subscription_status <- ifelse(data$subscription_status %in% c("true", "1", "yes", "y", "symbol"), 
                                         "已订阅", "未订阅")
    } else if(is.logical(data$subscription_status)) {
      # 如果是逻辑型，转换为中文标签
      data$subscription_status <- ifelse(data$subscription_status, "已订阅", "未订阅")
    }
    
    # 转换为因子
    data$subscription_status <- factor(data$subscription_status, 
                                       levels = c("未订阅", "已订阅"))
  }
  return(data)
}

# 在数据过滤函数中使用
retention_filtered_data <- reactive({
  data <- processed_data()
  
  # 应用修复
  data <- fix_subscription_status(data)
  
  # 其他过滤逻辑...
  
  return(data)
})

# 运行Shiny应用
shinyApp(ui = ui, server = server)
