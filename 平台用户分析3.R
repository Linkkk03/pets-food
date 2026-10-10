
# 1. 加载必要的包
library(survival)     # 生存分析核心包
library(survminer)    # 生存分析可视化
library(tidyverse)    # 数据处理和可视化
library(lubridate)    # 日期处理
library(rms)          # 回归建模和模型评估
library(timeROC)      # 时间依赖性ROC曲线
library(dcurves)      # 决策曲线分析
library(gridExtra)    # 多图布局

# 2. 数据导入与预处理
# 读取数据
df_raw <- read.csv("C:/Users/Linkk/Desktop/R/宠物食品分析/pet_food_customer_orders.csv", stringsAsFactors = FALSE)

# 查看数据结构
cat("数据维度:", dim(df_raw), "\n")
str(df_raw)

# 3. 数据清洗与关键变量构建
df <- df_raw %>%
  mutate(across(where(is.character), ~na_if(., "")))

df <- df_raw %>%
  mutate(across(where(is.factor), as.character))
df <- df_raw %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)),
    across(where(is.character), ~ replace_na(.x, "None"))
  )
str(df)
df <- df_raw %>%
  # 处理缺失值
  mutate(
    # 处理空字符串和特殊值
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
    cs_category = factor(ifelse(is.na(customer_support_ticket_category), "none", 
                                customer_support_ticket_category)),
    
    # 日期转换
    order_payment_date = as.Date(order_payment_date),
    pet_signup_datetime = as.Date(pet_signup_datetime),
    
    # 因子转换
    pet_breed_size = factor(pet_breed_size, levels = c("small", "medium", "large","toy","giant")),
    gender = factor(gender),
    neutered = factor(neutered),
    pet_life_stage_at_order = factor(pet_life_stage_at_order),
    pet_food_tier = factor(pet_food_tier),
    signup_promo = factor(signup_promo),
    pet_has_active_subscription = as.logical(pet_has_active_subscription)
  )

# 4. 构建用户级别数据集用于生存分析
# 4.1 计算每个宠物的关键指标
user_data <- df %>%
  arrange(pet_id, order_payment_date) %>%
  group_by(pet_id, customer_id) %>%
  summarise(
    # 时间相关变量
    first_order_date = min(order_payment_date),
    last_order_date = max(order_payment_date),
    observation_end_date = max(df$order_payment_date) + 90,  # 假设90天观察期
    
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
    food_tier = last(pet_food_tier),
    life_stage = last(pet_life_stage_at_order),
    gender = last(gender),
    neutered = last(neutered),
    
    .groups = 'drop'
  ) %>%
  mutate(
    # 计算生存分析相关变量
    # 生存时间：从首次订单到最后一次订单的天数
    survival_time = as.numeric(last_order_date - first_order_date),
    
    # 判断是否流失（90天内无新订单）
    time_since_last = as.numeric(observation_end_date - last_order_date),
    is_churned = ifelse(time_since_last > 90 & total_orders > 1, 1, 0),
    
    # 事件状态（1=流失，0=删失/仍留存）
    status = is_churned,
    
    # 计算其他衍生指标
    wet_order_ratio = ifelse(total_orders > 0, total_wet_orders / total_orders, 0),
    kcal_wet_ratio = ifelse((total_kibble_kcal + total_wet_kcal) > 0, 
                            total_wet_kcal / (total_kibble_kcal + total_wet_kcal), 0),
    has_ever_wet = ifelse(total_wet_orders > 0, 1, 0)
  )

# 查看用户数据集
cat("用户数据集维度:", dim(user_data), "\n")
print(head(user_data))

# 5. 描述性统计分析
# 5.1 整体流失率
churn_rate <- mean(user_data$is_churned, na.rm = TRUE)
cat(sprintf("整体流失率: %.2f%% (n = %d/%d)\n", 
            churn_rate * 100, 
            sum(user_data$is_churned), 
            nrow(user_data)))

# 5.2 按关键特征分组的流失率
group_churn_rates <- user_data %>%
  group_by(breed_size) %>%
  summarise(
    n = n(),
    churned = sum(is_churned),
    churn_rate = mean(is_churned),
    .groups = 'drop'
  )
print("按宠物体型分组的流失率:")
print(group_churn_rates)

# 6. Kaplan-Meier生存分析
# 6.1 创建生存对象
surv_obj <- Surv(user_data$survival_time, user_data$status)

# 6.2 按订阅状态分组
fit_subscription <- survfit(surv_obj ~ subscription_status, data = user_data)
print("按订阅状态分组的生存分析结果:")
print(summary(fit_subscription))

# 绘制生存曲线
p1 <- ggsurvplot(fit_subscription,
                 data = user_data,
                 pval = TRUE,
                 conf.int = TRUE,
                 risk.table = TRUE,
                 risk.table.height = 0.25,
                 surv.median.line = "hv",
                 xlab = "生存时间 (天)",
                 ylab = "留存概率",
                 legend.title = "订阅状态",
                 legend.labs = c("未订阅", "已订阅"),
                 palette = c("#bc5148", "#3090a1"),
                 ggtheme = theme_minimal(),
                 title = "订阅状态对用户留存的影响")
p1
# 6.3 按是否购买过湿粮分组
fit_wet_food <- survfit(surv_obj ~ has_ever_wet, data = user_data)

p2 <- ggsurvplot(fit_wet_food,
                 data = user_data,
                 pval = TRUE,
                 conf.int = TRUE,
                 risk.table = TRUE,
                 risk.table.height = 0.25,
                 surv.median.line = "hv",
                 xlab = "生存时间 (天)",
                 ylab = "留存概率",
                 legend.title = "湿粮购买",
                 legend.labs = c("从未购买", "曾购买"),
                 palette = c("#e74c3c", "#2ecc71"),
                 ggtheme = theme_minimal(),
                 title = "湿粮购买对用户留存的影响")
p2
# 6.4 按客服互动分组
fit_cs <- survfit(surv_obj ~ has_ever_cs_interaction, data = user_data)

p3 <- ggsurvplot(fit_cs,
                 data = user_data,
                 pval = TRUE,
                 conf.int = TRUE,
                 risk.table = TRUE,
                 risk.table.height = 0.25,
                 surv.median.line = "hv",
                 xlab = "生存时间 (天)",
                 ylab = "留存概率",
                 legend.title = "客服互动",
                 legend.labs = c("无互动", "有互动"),
                 palette = c("#f39c12", "#8e44ad"),
                 ggtheme = theme_minimal(),
                 title = "客服互动对用户留存的影响")
p3
# 7. Cox比例风险回归模型
# 7.1 单因素Cox回归分析
# 选择要分析的变量
variables_to_test <- c("subscription_status", "has_ever_wet", "has_ever_cs_interaction",
                       "breed_size", "food_tier", "avg_discount", "wet_order_ratio")

# 批量进行单因素Cox回归
uni_cox_results <- list()

for(var in variables_to_test) {
  formula_str <- paste("Surv(survival_time, status) ~", var)
  uni_model <- coxph(as.formula(formula_str), data = user_data)
  
  # 提取结果
  x <- summary(uni_model)
  p.value <- signif(x$wald["pvalue"], digits = 3)
  HR <- signif(x$coef[2], digits = 3)
  HR.confint.lower <- signif(x$conf.int[,"lower .95"], digits = 3)
  HR.confint.upper <- signif(x$conf.int[,"upper .95"], digits = 3)
  HR.95CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
  
  uni_cox_results[[var]] <- data.frame(
    Variable = var,
    HR = HR,
    HR_95CI = HR.95CI,
    Lower_CI = HR.confint.lower,
    Upper_CI = HR.confint.upper,
    P_value = p.value,
    stringsAsFactors = FALSE
  )
}

# 合并结果
uni_cox_df <- do.call(rbind, uni_cox_results)
rownames(uni_cox_df) <- NULL
print("单因素Cox回归分析结果:")
print(uni_cox_df)

# 7.2 多因素Cox回归分析
# 选择显著变量构建多因素模型
multi_cox <- coxph(Surv(survival_time, status) ~ 
                     subscription_status + 
                     has_ever_wet + 
                     has_ever_cs_interaction +
                     avg_discount +
                     wet_order_ratio +
                     breed_size,
                   data = user_data)

print("多因素Cox回归分析结果:")
summary(multi_cox)

#森林图
library(broom)
library(dplyr)
library(ggplot2)

# 提取模型结果（指数化）
cox_df <- tidy(multi_cox, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    # 将 subscription_statusTRUE 改为 subscription_status
    term = case_when(
      term == "subscription_statusTRUE" ~ "subscription_status",
      term == "has_adopted_wet_foodTRUE" ~ "has_adopted_wet_food",
      TRUE ~ term  # 其他保持不变
    )
  )

# 绘制森林图
ggplot(cox_df, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "风险比 (HR)", y = "变量", title = "Cox 回归森林图") +
  theme_minimal()
#把avg_discount去除再查看结果
cox_df <- tidy(multi_cox, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "avg_discount") %>%
  mutate(
    # 将 subscription_statusTRUE 改为 subscription_status
    term = case_when(
      term == "subscription_statusTRUE" ~ "subscription_status",
      term == "has_adopted_wet_foodTRUE" ~ "has_adopted_wet_food",
      TRUE ~ term  # 其他保持不变
    )
  )
# 绘制森林图
ggplot(cox_df, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "风险比 (HR)", y = "变量", title = "Cox 回归森林图") +
  theme_minimal()


# 7.3 变量选择（后退法）
# 准备数据
ddist <- datadist(user_data)
options(datadist = 'ddist')

# 拟合完整模型
full_fit <- cph(Surv(survival_time, status) ~ 
                  subscription_status + 
                  has_ever_wet + 
                  has_ever_cs_interaction +
                  avg_discount +
                  wet_order_ratio +
                  breed_size +
                  food_tier +
                  gender,
                data = user_data, 
                x = TRUE, y = TRUE, surv = TRUE)

# 使用AIC进行变量选择
selected_model_aic <- fastbw(full_fit, rule = "aic")
print("基于AIC的变量选择结果:")
print(selected_model_aic)

# 8. 模型评估与验证
# 8.1 计算C-index
c_index <- rcorrcens(Surv(survival_time, status) ~ predict(multi_cox), 
                     data = user_data)
cat(sprintf("C-index: %.3f\n", 1 - c_index[1]))

# 8.2 校准曲线
# 创建数据摘要对象
ddist <- datadist(user_data)
options(datadist = 'ddist')

# 拟合校准模型（30天留存）
cal_model_30 <- cph(Surv(survival_time, status) ~ 
                      subscription_status + has_ever_wet,
                    data = user_data, 
                    x = TRUE, y = TRUE, surv = TRUE,
                    time.inc = 30)
surv <- Survival(cal_model_30)
simple_model_30 <- cph(Surv(survival_time, status) ~ subscription_status,
                       data = user_data, 
                       x = TRUE, y = TRUE, surv = TRUE,
                       time.inc = 30)

# 然后进行校准
cal_model_30 <- calibrate(simple_model_30, 
                          cmethod = 'KM', 
                          method = "boot",
                          u = 30,
                          m = 10,  # 减少分箱数
                          B = 100)  # 增加Bootstrap次数以提高稳定性

# 绘制校准曲线
plot(cal_model_30,
     lwd = 2,
     lty = 0,
     conf.int = TRUE,
     errbar.col = "#2166AC",
     col = "red",
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "预测的30天留存率 (%)",
     ylab = "实际观察的30天留存率 (%)",
     cex.lab = 1.2,
     cex.axis = 1,
     main = "30天留存校准曲线")

lines(cal_model_30[, c('mean.predicted', "KM")],
      type = 'b',
      lwd = 3,
      pch = 16,
      col = "tomato")
abline(0, 1, lty = 3, lwd = 2, col = "grey70")
box(lwd = 2)

# 8.3 时间依赖性ROC曲线
# 准备数据（将状态变量调整为0/1格式）
user_data_roc <- user_data %>%
  mutate(status_01 = status)  # 已经是0/1格式

# 计算多个时间点的ROC
roc_times <- quantile(user_data_roc$survival_time[user_data_roc$status == 1], 
                      probs = seq(0.2, 0.8, 0.2))

roc_result <- timeROC(
  T = user_data_roc$survival_time,
  delta = user_data_roc$status_01,
  marker = predict(multi_cox),
  times = roc_times,
  cause = 1,
  weighting = "marginal",
  iid = FALSE,
)
print(roc_result)

# 绘制多个时间点的ROC曲线
plot(roc_result, time = roc_times[1], col = "red", title = FALSE)
for(i in 2:length(roc_times)) {
  plot(roc_result, time = roc_times[i], add = TRUE, col = c("blue", "green", "purple")[i-1])
}
legend("bottomright",
       legend = paste0(round(roc_times), "天"),
       col = c("red", "blue", "green", "purple")[1:length(roc_times)],
       lty = 1,
       cex = 0.8,
       bty = "n")

# 8.4 决策曲线分析
# 准备预测概率
user_data_dca <- user_data %>%
  mutate(
    predicted_risk = 1 - exp(-predict(multi_cox, type = "expected"))
  )

# 执行决策曲线分析（30天留存）
dca_result <- dca(Surv(survival_time, status) ~ predicted_risk,
                  data = user_data_dca,
                  time = 30,
                  thresholds = seq(0.01, 0.5, by = 0.01))

# 绘制决策曲线
plot(dca_result,
     xlab = "阈值概率",
     ylab = "净收益",
     main = "30天留存的决策曲线分析")

# 9. 列线图构建
# 9.1 拟合简化模型用于列线图
nomogram_model <- cph(Surv(survival_time, status) ~ 
                        subscription_status + 
                        has_ever_wet + 
                        avg_discount,
                      data = user_data,
                      x = TRUE, y = TRUE, surv = TRUE)

# 9.2 创建生存函数
surv_func <- Survival(nomogram_model)

# 9.3 绘制列线图（预测30天和90天留存率）
dev.new()
par(bg = "aliceblue")
plot(nomogram(nomogram_model,
              fun = list(function(x) surv_func(30, x),
                         function(x) surv_func(90, x)),
              funlabel = c("30天留存概率",
                           "90天留存概率"),
              lp.at = seq(-2, 2, by = 0.5)),
     xfrac = 0.3,
     tcl = 0.5)

