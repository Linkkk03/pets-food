# 加载必要的包
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(caret)
library(nnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(vcd)
library(gmodels)

# 1. 数据导入与查看
df <- read.csv("C:/Users/Linkk/Desktop/R/宠物食品分析/pet_food_customer_orders.csv", stringsAsFactors = FALSE)

# 查看数据结构
cat("数据维度:", dim(df), "\n")
print(names(df))
str(df)
View(df)
summary(df)

# 查看缺失值情况
missing_summary <- sapply(df, function(x) sum(is.na(x) | x == "" | x == "None" | x == "Null & Default"))
print(missing_summary)

# 2. 数据预处理
# 2.1 处理特殊缺失值
df <- df %>%
  mutate(across(where(is.factor), as.character))
df <- df %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)),
    across(where(is.character), ~ replace_na(.x, "None"))
  )
df_clean <- df %>%
  mutate(
    # 处理signup_promo中的特殊值
    signup_promo = ifelse(signup_promo %in% c("Null & Default", "", "None", NA), 
                          "Unknown", signup_promo),
    
    # 处理空字符串为NA
    across(where(is.character), ~ ifelse(.x == "" | .x == "None", NA, .x)),
    
    # 转换逻辑变量
    pet_has_active_subscription = as.logical(pet_has_active_subscription),
    neutered = as.logical(neutered),
    
    # 转换日期时间
    pet_signup_datetime = as.POSIXct(pet_signup_datetime),
    order_payment_date = as.Date(order_payment_date),
    last_customer_support_ticket_date = ifelse(
      last_customer_support_ticket_date == "" | is.na(last_customer_support_ticket_date),
      NA,
      as.POSIXct(last_customer_support_ticket_date)
    ),
    
    # 处理数值型变量中的NA
    across(c(kibble_kcal, wet_kcal, total_order_kcal, wet_trays,
             wet_food_discount_percent, premium_treat_packs, dental_treat_packs,
             total_web_sessions, total_web_sessions_since_last_order,
             total_minutes_on_website, total_minutes_on_website_since_last_order,
             total_wet_food_updates, total_wet_food_updates_since_last_order),
           ~ ifelse(is.na(.x), 0, .x))
  )

# 2.2 创建食品类型分类
df_clean <- df_clean %>%
  mutate(
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
    
    # 简化口味偏好
    flavor_main = str_extract(pet_fav_flavour_list, "^[^,]+"),
    
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

# 2.3 转换分类变量为因子
categorical_vars <- c('food_type', 'pet_food_tier', 'gender', 'neutered', 
                      'pet_breed_size', 'breed_size_group', 'signup_promo',
                      'dry_food_brand_pre_tails', 'pet_life_stage_at_order',
                      'wet_tray_size', 'customer_support_ticket_category',
                      'health_issue_main', 'flavor_main')

df_clean <- df_clean %>%
  mutate(across(all_of(categorical_vars), as.factor))

# 检查处理结果
print(table(df_clean$food_type, useNA = "always"))

#湿粮比例统计
print(summary(df_clean$wet_food_ratio))

# 3. 描述性统计分析
# 3.1 整体消费结构分析

# 按订单统计
order_summary <- df_clean %>%
  group_by(food_type) %>%
  summarise(
    n_orders = n(),
    pct_orders = n() / nrow(df_clean) * 100,
    avg_total_kcal = mean(total_order_kcal, na.rm = TRUE),
    avg_wet_ratio = mean(wet_food_ratio, na.rm = TRUE)
  )
print(order_summary)

# 按宠物统计（每个宠物的主要消费模式）
pet_summary <- df_clean %>%
  group_by(pet_id) %>%
  summarise(
    total_orders = n(),
    wet_orders = sum(wet_food_purchased),
    main_food_type = ifelse(
      sum(wet_food_purchased) == 0, "纯干粮",
      ifelse(sum(wet_food_purchased) == total_orders, "纯湿粮", "混合")
    ),
    avg_wet_ratio = mean(wet_food_ratio)
  )

cat("\n按宠物统计的消费模式:\n")
pet_type_summary <- pet_summary %>%
  group_by(main_food_type) %>%
  summarise(
    n_pets = n(),
    pct_pets = n() / nrow(pet_summary) * 100,
    avg_orders_per_pet = mean(total_orders)
  )
print(pet_type_summary)

# 3.2 宠物特征分布对比

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

# 对比关键特征
features_to_compare <- c('breed_size_group', 'gender', 'neutered', 
                         'pet_life_stage_at_order', 'pet_food_tier')

for (feature in features_to_compare) {
  cat(paste0("\n--- ", feature, " 分布 ---\n"))
  comp_table <- compare_features(df_clean, feature)
  print(comp_table)
  
  # 可视化
  p <- ggplot(comp_table, aes(x = !!sym(feature), y = percentage, fill = food_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(feature, "分布对比"),
         x = feature, y = "百分比 (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

# 3.3 消费者行为特征分析

behavior_summary <- df_clean %>%
  group_by(food_type) %>%
  summarise(
    n = n(),
    avg_web_sessions = mean(total_web_sessions, na.rm = TRUE),
    avg_website_minutes = mean(total_minutes_on_website, na.rm = TRUE),
    avg_wet_updates = mean(total_wet_food_updates, na.rm = TRUE),
    subscription_rate = mean(has_subscription, na.rm = TRUE) * 100,
    avg_discount = mean(wet_food_discount_percent, na.rm = TRUE),
    avg_treat_packs = mean(premium_treat_packs + dental_treat_packs, na.rm = TRUE)
  )
print(behavior_summary)
# 创建多图表布局
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# 1. 网站会话次数分布
hist(df_clean$total_web_sessions, 
     main = "网站会话次数分布",
     xlab = "会话次数",
     ylab = "用户数量",
     col = "lightblue",
     border = "white",
     breaks = 20)

# 2. 网站停留时间分布
hist(df_clean$total_minutes_on_website,
     main = "网站停留时间分布",
     xlab = "停留时间(分钟)",
     ylab = "用户数量",
     col = "lightgreen",
     border = "white",
     breaks = 20)

# 3. 湿粮更新次数分布
hist(df_clean$total_wet_food_updates,
     main = "湿粮更新次数分布",
     xlab = "更新次数",
     ylab = "用户数量",
     col = "lightcoral",
     border = "white",
     breaks = 15)

# 4. 订阅情况分布
barplot(table(df_clean$has_subscription),
        main = "订阅情况分布",
        xlab = "是否订阅",
        ylab = "用户数量",
        col = c("skyblue", "salmon"),
        names.arg = c("未订阅", "已订阅"))

# 5. 折扣百分比分布
hist(df_clean$wet_food_discount_percent,
     main = "折扣百分比分布",
     xlab = "折扣百分比(%)",
     ylab = "用户数量",
     col = "lightgoldenrod",
     border = "white",
     breaks = 15)

# 6. 零食包总数分布
df_clean$total_treat_packs <- df_clean$premium_treat_packs + df_clean$dental_treat_packs
hist(df_clean$total_treat_packs,
     main = "零食包总数分布",
     xlab = "零食包数量",
     ylab = "用户数量",
     col = "lightpink",
     border = "white",
     breaks = 15)

# 重置图表布局
par(mfrow = c(1, 1))
# 4. 统计推断分析
# 4.1 卡方检验

# 对每个分类特征进行卡方检验
chi_square_results <- list()

for (feature in features_to_compare) {
  # 创建列联表
  contingency_table <- table(df_clean[[feature]], df_clean$food_type)
  
  # 执行卡方检验
  chi_test <- chisq.test(contingency_table)
  
  # 保存结果
  chi_square_results[[feature]] <- list(
    table = contingency_table,
    test = chi_test
  )
  
  cat(paste0("\n", feature, " 的卡方检验:\n"))
  cat(paste0("X-squared = ", round(chi_test$statistic, 3), 
             ", df = ", chi_test$parameter,
             ", p-value = ", format.pval(chi_test$p.value, digits = 3), "\n"))
  
  # 如果p值显著，打印标准化残差
  if (chi_test$p.value < 0.05) {
    cat("标准化残差（绝对值>2表示显著差异）:\n")
    print(round(chi_test$stdres, 2))
  }
}

# 4.2 逻辑回归分析（是否购买湿粮）

# 准备回归数据（移除缺失值）
reg_data <- df_clean %>%
  select(wet_food_purchased, breed_size_group, gender, neutered,
         pet_life_stage_at_order, pet_food_tier, 
         total_web_sessions, has_subscription,
         pet_breed_size, pet_health_issue_list) %>%
  na.omit()

# 简化健康问题为是否有健康问题
reg_data <- reg_data %>%
  mutate(has_health_issue = !is.na(pet_health_issue_list) & pet_health_issue_list != "")

# 拟合逻辑回归模型
logit_model <- glm(wet_food_purchased ~ breed_size_group + gender + neutered +
                     pet_life_stage_at_order + pet_food_tier + 
                     total_web_sessions + has_subscription + has_health_issue,
                   data = reg_data, family = binomial())


print(summary(logit_model))

# 计算优势比
odds_ratios <- exp(coef(logit_model))
cat("\n优势比（OR）:\n")
print(data.frame(
  Variable = names(odds_ratios),
  Odds_Ratio = round(odds_ratios, 3),
  CI_lower = round(exp(confint(logit_model)[, 1]), 3),
  CI_upper = round(exp(confint(logit_model)[, 2]), 3)
))

library(broom)
library(ggplot2)
library(dplyr)

# 提取系数、标准误、置信区间，并转换为 OR
or_results <- logit_model %>%
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%  # 通常不画截距
  mutate(
    term = gsub("breed_size_group", "Breed: ", term),
    term = gsub("pet_life_stage_at_order", "Life Stage: ", term),
    term = gsub("pet_food_tier", "Food Tier: ", term),
    term = gsub("gendermale", "Gender: Male", term),
    term = gsub("neuteredTRUE", "Neutered", term),
    term = gsub("total_web_sessions", "Web Sessions", term),
    term = gsub("has_subscriptionTRUE", "Has Subscription", term),
    term = gsub("has_health_issueTRUE", "Has Health Issue", term)
  ) %>%
  arrange(estimate)  # 按 OR 从小到大排序
# 创建森林图
ggplot(or_results, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  coord_flip() +
  labs(
    x = "Variables",
    y = "Odds Ratio (95% CI)",
    title = "Forest Plot of Odds Ratios for Wet Food Purchase"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
# 5. 决策树和随机森林
# 构建随机森林模型
# 检查并清理 food_type
tree_data <- df_clean %>%
  filter(food_type %in% c("纯干粮", "混合", "纯湿粮")) %>%
  mutate(food_type = factor(food_type, levels = c("纯干粮", "混合", "纯湿粮"))) %>%
  select(food_type, breed_size_group, gender, neutered,
         pet_life_stage_at_order, pet_food_tier,
         total_web_sessions, has_subscription) %>%
  na.omit()
table(tree_data$food_type)

# 删除空类别（假设 "both" 是空的）
tree_data$food_type <- ifelse(tree_data$food_type == "both", "dry", tree_data$food_type)
tree_data$food_type <- factor(tree_data$food_type)

# 删除 NA
tree_data <- na.omit(tree_data)

# 构建随机森林
rf_model <- randomForest(food_type ~ ., data = tree_data, 
                         ntree = 500, importance = TRUE)

#随机森林模型OOB误差
print(rf_model$err.rate[nrow(rf_model$err.rate), ])

# 特征重要性分析
importance_df <- as.data.frame(rf_model$importance)
importance_df$Variable <- rownames(importance_df)
importance_df <- importance_df[order(-importance_df$MeanDecreaseGini), ]

#随机森林特征重要性（按基尼指数）
print(importance_df[, c("Variable", "MeanDecreaseGini")])

# 绘制特征重要性条形图
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "随机森林特征重要性",
       x = "变量", y = "基尼指数减少量") +
  theme_minimal(base_size = 14) + # 调整字体大小以提高可读性
  theme(plot.title = element_text(hjust = 0.5)) # 居中标题


# 6. 时间序列与行为模式分析

# 6.1 消费模式演变
# 按宠物和时间分析
time_analysis <- df_clean %>%
  arrange(pet_id, order_payment_date) %>%
  group_by(pet_id) %>%
  mutate(
    order_sequence = row_number(),
    first_wet_order = min(order_payment_date[wet_food_purchased], na.rm = TRUE),
    days_since_first_wet = ifelse(is.infinite(first_wet_order), 
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
    avg_wet_ratio = mean(wet_food_ratio)
  )

# 可视化湿粮采用趋势
ggplot(wet_adoption, aes(x = days_since_first_wet, y = avg_wet_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "湿粮采用趋势（首次购买后）",
       x = "首次湿粮购买后天数", y = "平均湿粮比例") +
  theme_minimal()

# 6.2 网站行为与购买决策关联
behavior_cor <- df_clean %>%
  select(wet_food_ratio, total_web_sessions, total_minutes_on_website,
         total_wet_food_updates, wet_food_discount_percent) %>%
  cor(use = "complete.obs")

print(round(behavior_cor, 3))

# 加载必要包
library(ggplot2)
library(tidyr)
library(tidyverse)
# 计算相关系数矩阵
behavior_cor <- df_clean %>%
  select(wet_food_ratio, total_web_sessions, total_minutes_on_website,
         total_wet_food_updates, wet_food_discount_percent) %>%
  cor(use = "complete.obs")

# 转换为长格式（用于 ggplot）
behavior_cor_long <- behavior_cor %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation") %>%
  mutate(
    Var1 = factor(Var1, levels = colnames(behavior_cor)),
    Var2 = factor(Var2, levels = colnames(behavior_cor))
  )

# 绘制热力图
ggplot(behavior_cor_long, aes(x = Var2, y = Var1, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "#4E79A7",   # 蓝色（负相关）
    mid = "white",     # 中性（0）
    high = "#F28E2B",  # 橙色（正相关）
    midpoint = 0,
    limits = c(-1, 1),
    name = "相关系数"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    x = "",
    y = "",
    title = "网站行为与湿粮购买比例的相关性热力图"
  ) +
  # 在每个格子中显示数值（可选）
  geom_text(aes(label = round(Correlation, 3)), size = 4)

# 7. 聚类分析（用户细分）

# 准备聚类数据
cluster_data <- df_clean %>%
  select(wet_food_ratio, total_web_sessions, total_minutes_on_website,
         premium_treat_packs, dental_treat_packs, has_subscription) %>%
  scale() %>%
  na.omit()

# 确定最佳聚类数（肘部法则）
dev.new()  # 创建新图形窗口
wss <- sapply(1:10, function(k) {
  kmeans(cluster_data, k, nstart = 25)$tot.withinss
})

# 绘制肘部法则图（单独窗口）
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "聚类数", ylab = "组内平方和",
     main = "肘部法则确定最佳聚类数")
elbow_point <- 4  # 根据图形判断，可以手动调整这个值
abline(v = elbow_point, col = "red", lty = 2)

# 执行K-means聚类
set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 4, nstart = 25)

# 将聚类结果添加到原始数据
df_clustered <- df_clean[complete.cases(df_clean[, c("wet_food_ratio", "total_web_sessions",
                                                     "total_minutes_on_website", 
                                                     "premium_treat_packs",
                                                     "dental_treat_packs", "has_subscription")]), ]
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
    # 添加宠物特征
    large_breed_pct = mean(breed_size_group == "大型", na.rm = TRUE) * 100,
    mature_pct = mean(pet_life_stage_at_order == "mature", na.rm = TRUE) * 100
  )

cat("\n聚类特征描述:\n")
print(cluster_profiles)
# 使用fviz_cluster进行可视化
fviz_cluster(kmeans_result, 
             data = cluster_data,
             geom = "point",  # 显示点
             ellipse.type = "norm",  # 绘制正态分布椭圆
             palette = "Set2",  # 设置颜色方案
             ggtheme = theme_minimal())  # 设置主题
# 7.4 可视化聚类结果

#dev.new()
# 使用ggplot绘制聚类特征对比条形图
library(reshape2)

# 准备条形图数据
bar_data <- cluster_profiles %>%
  select(cluster, avg_wet_ratio, avg_web_sessions, avg_website_minutes, 
         subscription_rate, large_breed_pct, mature_pct) %>%
  mutate(
    avg_wet_ratio = avg_wet_ratio * 100  # 转换为百分比
  )

# 重塑为长格式
bar_data_long <- melt(bar_data, id.vars = "cluster")

# 重命名变量以便更好理解
variable_names <- c(
  "avg_wet_ratio" = "湿粮比例(%)",
  "avg_web_sessions" = "平均网站访问次数",
  "avg_website_minutes" = "平均网站停留时间(分钟)",
  "subscription_rate" = "订阅率(%)",
  "large_breed_pct" = "大型犬比例(%)",
  "mature_pct" = "成熟期比例(%)"
)

bar_data_long$variable <- factor(bar_data_long$variable, 
                                 levels = names(variable_names),
                                 labels = variable_names)

# 绘制聚类特征对比条形图
ggplot(bar_data_long, aes(x = variable, y = value, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(value, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1","#ffd5b3","#f28e2a","#6487b1"),
                    labels = c("聚类1", "聚类2", "聚类3","聚类4","聚类5","聚类6")) +
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
  ) +
  ylim(0, max(bar_data_long$value) * 1.2)  # 为文本标签留出空间

# PCA降维可视化聚类分布
# 执行PCA降维
pca_result <- prcomp(cluster_data, center = TRUE, scale. = TRUE)

# 提取前两个主成分
pca_scores <- as.data.frame(pca_result$x[, 1:2])
pca_scores$cluster <- as.factor(kmeans_result$cluster)

# 计算方差解释比例
var_explained <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 1)

# 绘制PCA聚类图
#dev.new()
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.2) +
  scale_color_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1","#ffd5b3","#f28e2a","#6487b1")) +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1","#ffd5b3","#f28e2a","#6487b1")) +
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


# 7.5 聚类用户画像生成
cat("\n=== 聚类用户画像分析 ===\n")

# 为每个聚类创建详细的用户画像
for (clust in sort(unique(df_clustered$cluster))) {
  cat(paste0("\n--- 聚类 ", clust, " 用户画像 ---\n"))
  
  cluster_subset <- df_clustered %>% filter(cluster == clust)
  
  # 计算画像特征
  profile <- cluster_subset %>%
    summarise(
      # 基本统计
      n_orders = n(),
      n_pets = n_distinct(pet_id),
      avg_wet_ratio = mean(wet_food_ratio, na.rm = TRUE) * 100,
      
      # 宠物特征
      breed_size_dist = paste(
        "大型:", round(mean(breed_size_group == "大型", na.rm = TRUE) * 100, 1), "%",
        "中小型:", round(mean(breed_size_group == "中小型", na.rm = TRUE) * 100, 1), "%"
      ),
      gender_dist = paste(
        "雌性:", round(mean(gender == "female", na.rm = TRUE) * 100, 1), "%",
        "雄性:", round(mean(gender == "male", na.rm = TRUE) * 100, 1), "%"
      ),
      life_stage_dist = paste(
        "幼年期:", round(mean(pet_life_stage_at_order %in% c("puppy", "kitten"), na.rm = TRUE) * 100, 1), "%",
        "成熟期:", round(mean(pet_life_stage_at_order == "mature", na.rm = TRUE) * 100, 1), "%"
      ),
      neutered_rate = round(mean(neutered, na.rm = TRUE) * 100, 1),
      
      # 食品偏好
      food_tier_dist = paste(
        "超高端:", round(mean(pet_food_tier == "superpremium", na.rm = TRUE) * 100, 1), "%",
        "高端:", round(mean(pet_food_tier == "premium", na.rm = TRUE) * 100, 1), "%"
      ),
      food_type_dist = paste(
        "纯干粮:", round(mean(food_type == "纯干粮", na.rm = TRUE) * 100, 1), "%",
        "混合:", round(mean(food_type == "混合", na.rm = TRUE) * 100, 1), "%",
        "纯湿粮:", round(mean(food_type == "纯湿粮", na.rm = TRUE) * 100, 1), "%"
      ),
      
      # 消费者行为
      subscription_rate = round(mean(has_subscription, na.rm = TRUE) * 100, 1),
      avg_web_sessions = round(mean(total_web_sessions, na.rm = TRUE), 1),
      avg_website_minutes = round(mean(total_minutes_on_website, na.rm = TRUE), 1),
      avg_treats = round(mean(premium_treat_packs + dental_treat_packs, na.rm = TRUE), 2),
      
      # 网站参与度
      high_engagement = round(mean(total_web_sessions > median(df_clean$total_web_sessions, na.rm = TRUE), na.rm = TRUE) * 100, 1),
      
      # 健康问题
      health_issue_rate = round(mean(!is.na(pet_health_issue_list) & pet_health_issue_list != "", na.rm = TRUE) * 100, 1)
    )
  
  # 打印画像
  cat(paste0("订单数: ", profile$n_orders, "\n"))
  cat(paste0("宠物数: ", profile$n_pets, "\n"))
  cat(paste0("平均湿粮比例: ", round(profile$avg_wet_ratio, 1), "%\n"))
  cat(paste0("体型分布: ", profile$breed_size_dist, "\n"))
  cat(paste0("性别分布: ", profile$gender_dist, "\n"))
  cat(paste0("生命阶段: ", profile$life_stage_dist, "\n"))
  cat(paste0("绝育比例: ", profile$neutered_rate, "%\n"))
  cat(paste0("食品等级: ", profile$food_tier_dist, "\n"))
  cat(paste0("食品类型: ", profile$food_type_dist, "\n"))
  cat(paste0("订阅率: ", profile$subscription_rate, "%\n"))
  cat(paste0("平均网站访问: ", profile$avg_web_sessions, " 次\n"))
  cat(paste0("平均网站停留: ", profile$avg_website_minutes, " 分钟\n"))
  cat(paste0("平均零食购买: ", profile$avg_treats, " 包\n"))
  cat(paste0("高参与度用户: ", profile$high_engagement, "%\n"))
  cat(paste0("有健康问题比例: ", profile$health_issue_rate, "%\n"))
  
  # 生成用户画像标签
  if (profile$avg_wet_ratio > 50) {
    wet_label <- "湿粮偏好型"
  } else if (profile$avg_wet_ratio > 20) {
    wet_label <- "混合粮型"
  } else {
    wet_label <- "干粮偏好型"
  }
  
  if (profile$subscription_rate > 50) {
    sub_label <- "高订阅率"
  } else {
    sub_label <- "低订阅率"
  }
  
  if (profile$avg_web_sessions > median(df_clean$total_web_sessions, na.rm = TRUE)) {
    eng_label <- "高参与度"
  } else {
    eng_label <- "低参与度"
  }
  
  cat(paste0("\n用户画像标签: ", wet_label, " | ", sub_label, " | ", eng_label, "\n"))
}


