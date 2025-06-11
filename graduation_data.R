library(tidyverse)
library(readxl)
library(mice)
df <- read_excel("数据.xlsx")
df <- df %>%
  mutate(
    MMSE_total          = rowSums(select(., starts_with("MMSE_")), na.rm = TRUE),
    SocialSupport_total = rowSums(select(., starts_with("SocialSupport_")), na.rm = TRUE),
    Depression_total    = rowSums(select(., starts_with("Depression_")), na.rm = TRUE),
    PSM_total           = rowSums(select(., starts_with("PMS_")),  na.rm = TRUE),
    SocialFrailty_total = rowSums(select(., starts_with("SocialFrailty_")), na.rm = TRUE)
  )

md.pattern(df)

# 5. 定义插补方法：针对不同类型变量，指定不同的插补算法
#    - 连续变量 (age, MMSE_total, PSM_total, SocialFrailty_total) 用 pmm（预测均值匹配）
#    - 有序分类 (education) 用 polyreg（多项式回归）
#    - 二分类 (residence) 用 logreg（逻辑回归）
#    - 抑郁、社会支持如果是连续，也可用 pmm；若为量表，也可视为连续
meth <- make.method(df)
meth[c("Age", "MMSE_total", "PSM_total", "SocialFrailty_total", 
       "Depression", "SocialSupport")] <- "pmm"
meth["Education"]  <- "polyreg"
meth["Residence"]  <- "logreg"

# 6. 设置预测矩阵：只用关键协变量互相预测
pred <- make.predictorMatrix(df)
# 不用自身预测自身
pred[c("Age","Education","MMSE_total","Residence",
       "Depression","SocialSupport","SocialFrailty_total"), 
     c("Age","Education","MMSE_total","Residence",
       "Depression","SocialSupport","SocialFrailty_total")] <- 0
# 用年龄、教育、MMSE、居住地、抑郁、社会支持去预测社会衰弱
pred["SocialFrailty_total", c("Age","Education","MMSE_total",
                              "Residence","Depression","SocialSupport")] <- 1
# 同理，如果需要，也可以让这些协变量互相预测：
pred[c("Age","Education","MMSE_total","Residence",
       "Depression","SocialSupport"), 
     "SocialFrailty_total"] <- 1

# 7. 运行多重插补
imp <- mice(df, 
            m            = 5,      # 生成 5 份插补数据集
            method       = meth, 
            predictorMatrix = pred,
            maxit        = 20,     # 迭代次数
            seed         = 123)

# 8. 诊断插补收敛情况
stripplot(imp, pch = 20, cex = 1.2)      # 看插补分布
plot(imp)                                # 看参数轨迹

# 9. 提取某份完整数据或合并多份结果
# 单份完整数据：
df_complete1 <- complete(imp, 1)
# 或者长格式查看所有插补：
df_long <- complete(imp, "long")

# 10. 在插补后数据上做临床预测模型（以二元 Logistic 回归为例）
#    假定 SocialFrailty_total ≥ 某阈值 视为“高衰弱”（0/1）
df_long <- df_long %>%
  mutate(SF_binary = as.integer(SocialFrailty_total >= YOUR_THRESHOLD))

fit <- with(imp, 
            glm(SF_binary ~ Age + Education + MMSE_total + 
                  Residence + Depression + SocialSupport,
                family = binomial))

# 11. 合并（pool）模型结果
pooled <- pool(fit)
summary(pooled)

# 12. 将插补后的完整数据保存到文件
write_xlsx(df_complete1, "数据_填补_MICE.xlsx")
