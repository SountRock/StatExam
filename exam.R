library(readxl)
library(psych)
library(car)
library(ggplot2)
library(dplyr)
library(gridExtra)

data <- read_excel('Data/PTSR.xlsx')
colnames(data) = gsub(" ", "_", colnames(data))

#2#################################################################
# EDA Нулевая гипотеза: все методы лечения равноэффективны
data$Treatment_type <- as.factor(data$Treatment_type) 

str(data)
plot(data)

#Разбиение данных##########################
library(dplyr)
data_nums <- data[grep("Social_support", colnames(data)) : 
                    grep("PTSD_symptoms", colnames(data))] %>% mutate(data[grep("Age", colnames(data))])

data_char <- data[grep("Marital_status", colnames(data)) : 
                    grep("Metastasis", colnames(data))] %>% mutate(data[grep("Marital_status_Education", colnames(data))])

#Нормализация###########################
normilize_0 <- function(x) {
  return(x - min(x)) / (max(x) - min(x))
}

normilize_1 <- function(x) {
  return(scale(x, center = TRUE))
}

normilize_2 <- function(x) {
  return((x - mean(x)) / sd(x))
}

library("dplyr")
#data_nums <- data_nums %>% summarise(across(everything(), normilize_0))
#data_nums <- data_nums %>% summarise(across(everything(), normilize_1))
#data_nums <- data_nums %>% summarise(across(everything(), normilize_2))
#Нормализация###########################

data[,1] <- data_nums[,5]
data[,8:11] <- data_nums[,1:4]
#Разбиение данных##########################

#Treatment_type и Metastasis
p_Hope <- ggplot(data, aes(Hope)) +
  geom_density() +
  facet_grid(Treatment_type~Metastasis) +
  #scale_y_continuous(limits = c(0, 1)) + 
  theme_bw()

p_Resilience <- ggplot(data, aes(Resilience)) +
  geom_density() +
  #geom_histogram() +
  facet_grid(Treatment_type~Metastasis) +
  #scale_y_continuous(limits = c(0, 1)) + 
  theme_bw()

library(cowplot)
plot_grid(p_Hope, p_Resilience)

#Обьединяем признаки
data$Treatment_type_Metastasis <- factor(paste(data$Treatment_type, data$Metastasis, sep = "_"))

data_temp <- data %>% filter(Treatment_type == 'Combined treatment')

p_Treatment_type_Metastasis <- ggplot(data, aes(Hope, Resilience, color = Treatment_type_Metastasis)) +
  geom_point() +
  facet_grid(Metastasis~.) + 
  theme_bw()

data_group_treatment <- data %>% group_by(Treatment_type) %>% summarise(
  count_No_metastasis = sum(Metastasis == "No"),  count_Yes_metastasis = sum(Metastasis == "Yes"), 
  diff_No_Yes = sum(Metastasis == "No") - sum(Metastasis == "Yes")) 

p_Treatment_diff_No_Yes_metastasis <- 
  ggplot(data = data_group_treatment, 
         aes(y = diff_No_Yes, x = Treatment_type, fill =  factor(Treatment_type))) + 
  geom_bar(stat = "identity")

p_Treatment_diff_No_Yes_metastasis
#Гипотеза: лечение комбинированной терапией эффективнее, чем химиотераипией

#3###############################################################
#Первоначальная модель
model <- lm(PTSD_symptoms ~ Hope + Resilience + Social_support + Age, data = data_nums)
summary(model) 

vif(model)
drop1(model, test = "F") #!!! Удаление по самому большому p_value

#Проверям предикторы на корреляцию#################################################
corelation_data <- data.frame(type = character(),  value = integer())

corelation_data <- rbind(corelation_data, 
  c("PTSD_symptoms_Social_support", cor(data_nums$PTSD_symptoms, data$Social_support)))

corelation_data <- rbind(corelation_data, 
  c("PTSD_symptoms_Hope", cor(data_nums$PTSD_symptoms, data$Hope)))

corelation_data <- rbind(corelation_data, 
  c("PTSD_symptoms_Resilience", cor(data_nums$PTSD_symptoms, data$Resilience)))

corelation_data <- rbind(corelation_data, 
  c("PTSD_symptoms_Age", cor(data_nums$PTSD_symptoms, data$Age)))

corelation_data <- rbind(corelation_data, 
                         c("Social_support_Hope", cor(data_nums$Social_support, data$Hope)))

corelation_data <- rbind(corelation_data, 
                         c("Social_support_Resilience", cor(data_nums$Social_support, data$Resilience)))

corelation_data <- rbind(corelation_data, 
                         c("Social_support_Age", cor(data_nums$Social_support, data$Age)))

corelation_data <- rbind(corelation_data, 
                         c("Hope_Age", cor(data_nums$Hope, data$Age)))

corelation_data <- rbind(corelation_data, 
                         c("Hope_Resilience", cor(data_nums$Hope, data$Resilience)))

corelation_data <- rbind(corelation_data, 
                         c("Resilience_Age", cor(data_nums$Resilience, data$Age)))

colnames(corelation_data) <- c("type", "value")
#Проверям предикторы на корреляцию################################################# 

#Убираем Age 
model_2 <- update(model, .~. - Age)
drop1(model_2, test = "F") 

model_3 <- update(model_2, .~. - Resilience)
drop1(model_3, test = "F") 

model_diag <- data.frame(fortify(model_3), data_nums) 

#Проверка модели в целом
gg_resid <- ggplot(data = model_diag, aes(x = .fitted, y = .stdresid)) + 
geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 2, color = "red") +
  geom_hline(yintercept = -2, color = "red")  

gg_resid
#Наблюдаються песочные часы, но видны выбросы за границы intercept

res_1 <- gg_resid + aes(x = Hope)
res_3 <- gg_resid + aes(x = Social_support)

grid.arrange(res_1, res_3, nrow = 2)
#Каких либо особых корреляций не замечано, клювов не замечано

qqPlot(model_diag$.stdresid)
#Модель 3 более стабильна по qqPlot

#Предсказание##################################################
predictions_data <- data.frame(
  Hope = as.numeric(seq(min(data$Hope), max(data$Hope), length.out = 100)),
  Social_support = as.numeric(mean(data$Social_support)))

predictions <- predict(model_3, newdata = predictions_data,  interval = 'confidence')
predictions_data <- data.frame(predictions_data, predictions)

Pl_predict <- ggplot(predictions_data, aes(x = Hope, y = fit)) +
  geom_ribbon(alpha = 0.2, aes(ymin = lwr, ymax = upr)) +
  geom_line()
Pl_predict 

#Тест
predictions_data_test <- data.frame(
  Hope = data$Hope,
  Social_support = data$Social_support,
  PTSD_symptoms = data$PTSD_symptoms)

predictions_test <- predict(model_3, newdata = predictions_data_test,  interval = 'confidence')
predictions_data_test <- data.frame(predictions_data_test, predictions_test)
###############################################################

#Дисперсионный анализ______________________________________________________
data$Cancer_stage <- factor(data$Cancer_stage)
model_disp <- lm(data = data, PTSD_symptoms ~ Cancer_stage)

ggplot(data, aes(Cancer_stage, PTSD_symptoms, color = Cancer_stage)) + 
  stat_summary(fun.data = "mean_cl_normal") #Видна незначительность различий

model_disp_anova <- Anova(model_disp)
model_disp_anova
summary(model_disp)

model_disp_diag <- fortify(model_disp)

p_kuke <- ggplot(model_disp_diag, aes(x = 1:nrow(model_disp_diag), y = .cooksd)) + geom_bar(stat = "identity") + ggtitle("График расстояний Кука")
p_kuke

p_remains <- ggplot(model_disp_diag, aes(x = Cancer_stage, y = .stdresid)) + geom_boxplot() + ggtitle("График остатков")
p_remains #Не особо сильное смещение медиальных значений

qqPlot(model_disp, id = FALSE)#На концах графика видны выбросы

#Пост-хок тест
post_hoch <- glht(model_disp, linfct = mcp(Cancer_stage = "Tukey"))
result_post_hoch <- summary(post_hoch)
result_post_hoch #Посттравматическое стрессовое расстройство имет больщее 
#распространение на стадиях Ⅲ and Ⅳ - Ⅰ  имеют 

predictions_data_disp <- data.frame(Cancer_stage = factor(levels(data$Cancer_stage), levels = levels(data$Cancer_stage)))
predictions_data_disp <- data.frame(predictions_data_disp,
                                    predict(model_disp, newdata = predictions_data_disp, interval = "confidence")
)

gg_bars <- ggplot(data = predictions_data_disp, aes(x = Cancer_stage, y = fit)) +
  geom_bar(stat = "identity", aes(fill = Cancer_stage), width = 0.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1)
gg_bars #Видны незначительные различия между группами

#gg_bars + ggtitle(label = "График зависимости числа эксцистирований \nот условий обработки (пост-хок тест)") + geom_text(aes(y = 1.6, label = c("*", "*", "*", "", "*"))) + ylab(label = "PTSD_symptoms")
#Дисперсионный анализ______________________________________________________


