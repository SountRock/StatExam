library(readxl)
library(psych)
library(car)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(Hmisc)
library(multcomp)

data <- read_excel('Data/PTSR.xlsx')
colnames(data) = gsub(" ", "_", colnames(data))

data <- na.omit(data)

#EDA_______________________________________________________________________ 
#Нулевая гипотеза: все методы лечения равноэффективны
str(data)
plot(data)

data$Treatment_type <- as.factor(data$Treatment_type) 

#Разбиение данных##########################
data_nums <- data[grep("Social_support", colnames(data)) : 
                    grep("PTSD_symptoms", colnames(data))] %>% mutate(data[grep("Age", colnames(data))])

data_char <- data[grep("Marital_status", colnames(data)) : 
                    grep("Metastasis", colnames(data))] %>% mutate(data[grep("Marital_status_Education", colnames(data))])

#Нормализация*************************
normilize_0 <- function(x) {
  return(x - min(x)) / (max(x) - min(x))
}

normilize_1 <- function(x) {
  return(scale(x, center = TRUE))
}

normilize_2 <- function(x) {
  return((x - mean(x)) / sd(x))
}

#data_nums <- data_nums %>% summarise(across(everything(), normilize_0))
#data_nums <- data_nums %>% summarise(across(everything(), normilize_1))
#data_nums <- data_nums %>% summarise(across(everything(), normilize_2))
#Нормализация*************************

#data[,1] <- data_nums[,5]
#data[,8:11] <- data_nums[,1:4]
#Разбиение данных##########################

p_Hope <- ggplot(data, aes(Hope)) +
  geom_density() +
  facet_grid(Treatment_type~Metastasis) +
  #scale_y_continuous(limits = c(0, 1)) + 
  theme_bw()

p_Resilience <- ggplot(data, aes(Resilience)) +
  geom_density() +
  facet_grid(Treatment_type~Metastasis) +
  #scale_y_continuous(limits = c(0, 1)) + 
  theme_bw()

plot_grid(p_Hope, p_Resilience) #Видно нормальное распределение для обоих параметров

#Обьединяем признаки
data$Treatment_type_Metastasis <- factor(paste(data$Treatment_type, data$Metastasis, sep = "_"))

p_Treatment_type_Metastasis <- ggplot(data, aes(Hope, Resilience, color = Treatment_type_Metastasis)) +
  geom_point() +
  facet_grid(Treatment_type~.) + 
  theme_bw()
p_Treatment_type_Metastasis 

#Создадим новый dataframe, где опредилим такой показатель как 
#разность между количесвом имеющихся метастаз и их отсуствия в методе
data_group_treatment <- data %>% group_by(Treatment_type) %>% summarise(
  count_No_metastasis = sum(Metastasis == "No"),  count_Yes_metastasis = sum(Metastasis == "Yes"), 
  diff_No_Yes = sum(Metastasis == "No") - sum(Metastasis == "Yes")) 

#Для отображения эффективности методов лечения построим график
p_Treatment_diff_No_Yes_metastasis <- 
  ggplot(data = data_group_treatment, 
         aes(y = diff_No_Yes, x = Treatment_type, fill =  factor(Treatment_type))) + 
  geom_bar(stat = "identity")

p_Treatment_diff_No_Yes_metastasis
#Гипотеза 1: комбинированная терапия эффективнее, чем химиотераипия.
#Гипотеза 2: хирургическое лечение самое мало эффективное лечение. 

#EDA_______________________________________________________________________ 

#Линейная модель___________________________________________________________
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
#Исходя из коэффицентов самая большая связь (не считая связь с PTSD_symptoms)
#Наблюдаеться у Social_support и Hope (0.46), на втором месте Hope и Resilience
#(0.39), на третьем Social_support и Resilience(0.25)

#Исходя из этого построим линейную модель отобразив более или менее сильные взаимосвязи
model <- lm(PTSD_symptoms ~ Social_support:Hope + Resilience:Hope + Age, data = data_nums)
summary(model) 
vif(model) #Неких значений болше 2 невыявлено

model_diag_original <- data.frame(fortify(model), data_nums) 

#До улучшения*************************
gg_resid_original <- ggplot(data = model_diag_original, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 2, color = "red") +
  geom_hline(yintercept = -2, color = "red")  

gg_resid_original
#Наблюдаеться не совсем хороший график, остатки больше организуються в клюв, чем в песочные часы

drop1(model, test = "F")

res_1_original <- gg_resid_original + aes(x = Hope)
res_2_original <- gg_resid_original + aes(x = Resilience)
res_3_original <- gg_resid_original + aes(x = Social_support)
res_4_original <- gg_resid_original + aes(x = Age)

grid.arrange(res_1_original, res_2_original, res_3_original, res_4_original, nrow = 2)
#Из графиков как ни странно видна корреляция между Hope и Social_support.
#Но между Social_support и Resilience кажеться, что имеется хорошая корреляция,
#хоть и коффицент дает неувереность в этом. Так же подобное можно и придположить 
#между Hope и Resilience. Но из значений коэффицентов корреляции предположим, 
#что эта взаимосвязь менее вероятна. 
#До улучшения*************************

#Поэкспериментируем с моделью и сначала учием что все выше описанные взаимосвязи возможны
model_1 <- lm(PTSD_symptoms ~ Social_support*Hope*Resilience + Age, data = data_nums)
summary(model_1) 
#p-value значительно уменьшилось (в отрицательной степени). Но: 
model_2 <- lm(PTSD_symptoms ~ Social_support*Hope + Resilience + Age, data = data_nums)
summary(model_2) 
#Видим тоже значение p-value. Предпологаем, тогда что взаимосвязь 
#между Resilience с Social_support и Hope ложна. Но, проверим еще кое что:
model_3 <- lm(PTSD_symptoms ~ Social_support*Hope + Resilience:Age, data = data_nums)
summary(model_3) #Не видим измений p-value. Residual standard error и Multiple R-squared 
#тоже не сильно меняються. model_3 фактически безполезна

#Для model_3***************************
model_diag_model_2 <- data.frame(fortify(model_2), data_nums) 
gg_resid_model_2 <- ggplot(data = model_diag_model_2, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 2, color = "red") +
  geom_hline(yintercept = -2, color = "red")  

gg_resid_model_2
#Попробуем избавиться от записей на которых замечены выбросы:
#---------------------------------
data_nums_without_emissions <- data_nums %>% mutate(.stdresid = model_diag_model_2$.stdresid) %>% 
  filter(.stdresid < 1.999 & .stdresid > -1.999)

#Перестроим модель по той же схеме:
model_2_we <- lm(PTSD_symptoms ~ Social_support*Hope + Resilience + Age, data = data_nums_without_emissions)

#Постром график остатков снова
model_diag_model_2_without_emissions <- data.frame(fortify(model_2_we), data_nums_without_emissions)

model_diag_model_2_without_emissions <- data.frame(fortify(model_2_we), data_nums_without_emissions) 
gg_resid_model_2_without_emissions <- ggplot(data = model_diag_model_2_without_emissions, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 2, color = "red") +
  geom_hline(yintercept = -2, color = "red") 

gg_resid_model_2_without_emissions
#Из графика видно, что выбросы хоть и не изменились в количестве, но уменьшили свои растояния от нормы 
#---------------------------------

res_1_model_2 <- gg_resid_model_2_without_emissions + aes(x = Hope)
res_2_model_2 <- gg_resid_model_2_without_emissions + aes(x = Resilience)
res_3_model_2 <- gg_resid_model_2_without_emissions + aes(x = Social_support)
res_4_model_2 <- gg_resid_model_2_without_emissions + aes(x = Age)

grid.arrange(res_1_model_2, res_2_model_2, res_3_model_2, res_4_model_2, nrow = 2)
#Ничего не поменялось
#Для model_3***************************

#Улучшим модель************************
drop1(model_2_we, test = "F")

#Удаляем Age
model_3 <- update(model_2_we, .~. - Age)
drop1(model_3, test = "F")
summary(model_3)

#Удаляем Resilience
model_4 <- update(model_3, .~. - Resilience)
drop1(model_4, test = "F")
summary(model_4)

model_diag_model_4 <- data.frame(fortify(model_4), data_nums_without_emissions) 
gg_resid_model_4 <- ggplot(data = model_diag_model_4, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 2, color = "red") +
  geom_hline(yintercept = -2, color = "red")

gg_resid_model_4
#Судя по p-value не то, чтобы это сильно помогло, да и график остатков стал больше стремиться в клюв
#Улучшим модель************************
res_1_model_4 <- gg_resid_model_4 + aes(x = Hope)
res_2_model_4 <- gg_resid_model_4 + aes(x = Social_support)
grid.arrange(res_1_model_4, res_2_model_4, nrow = 2)

qqPlot(model_diag_model_4$.stdresid)

#Предсказание**************************
predictions_data <- data.frame(
  Hope = seq(min(data$Hope), max(data$Hope), length.out = 100),
  Social_support = mean(data$Social_support))

predictions <- predict(model_4, newdata = predictions_data,  interval = 'confidence')
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

predictions_test <- predict(model_4, newdata = predictions_data_test,  interval = 'confidence')
predictions_data_test <- data.frame(predictions_data_test, predictions_test)
#Предсказания имеют сильные погрешности. Особенно при резко малых истиных 
#значениях PTSD_symptoms. 

#Предсказание**************************
#Линейная модель___________________________________________________________

#Дисперсионный анализ______________________________________________________
data$Cancer_stage <- factor(data$Cancer_stage)
model_disp <- lm(data = data, PTSD_symptoms ~ Cancer_stage)

ggplot(data, aes(Cancer_stage, PTSD_symptoms, color = Cancer_stage)) + 
  stat_summary(fun.data = "mean_cl_normal") #Видна незначительность различий

model_disp_anova <- Anova(model_disp)
model_disp_anova
summary(model_disp)

model_disp_diag <- fortify(model_disp)

p_kuke <- ggplot(model_disp_diag, aes(x = 1:nrow(model_disp_diag), y = .cooksd)) + 
  geom_bar(stat = "identity") + 
  ggtitle("График расстояний Кука")
p_kuke

p_remains <- ggplot(model_disp_diag, aes(x = Cancer_stage, y = .stdresid)) + 
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ggtitle("График остатков")
p_remains #Не особо сильное смещение медиальных значений

qqPlot(model_disp, id = FALSE)#На концах графика видны сильные выбросы

#Пост-хок тест
post_hoch <- glht(model_disp, linfct = mcp(Cancer_stage = "Tukey"))
result_post_hoch <- summary(post_hoch)
result_post_hoch #Посттравматическое стрессовое расстройство имет большее 
#распространение на стадиях Ⅲ and Ⅳ - Ⅰ  имеют 

predictions_data_disp <- data.frame(Cancer_stage = factor(levels(data$Cancer_stage), levels = levels(data$Cancer_stage)))
predictions_data_disp <- data.frame(predictions_data_disp,
                     predict(model_disp, newdata = predictions_data_disp, interval = "confidence")
)

gg_bars <- ggplot(data = predictions_data_disp, aes(x = Cancer_stage, y = fit)) +
  geom_bar(stat = "identity", aes(fill = Cancer_stage), width = 0.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1)
gg_bars #Видны незначительные различия между группами
#Дисперсионный анализ______________________________________________________
