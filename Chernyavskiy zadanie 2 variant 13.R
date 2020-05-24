library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")
options(warn=-1) 
eddypro = read.csv("eddypro.csv", skip = 1, na = c ("","NA","-9999","-9999.0"), comment = c("["))
eddypro = eddypro [-1,]
eddypro = select(eddypro,-(roll))
eddypro = eddypro %>% mutate_if(is.character,factor)
eddypro = eddypro %>% mutate_if(is.character,factor)

names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")

glimpse(eddypro)
eddypro = drop_na(eddypro)
eddypro = filter(eddypro,DOY >= 60 & DOY < 151)
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]
row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]

#Модель 1 по обучающей выборке
mod1 = lm(co2_flux ~ (.), data = teaching_tbl)
#Информация 
summary(mod1)
#Коэффициенты 
coef(mod1)
#Остатки
resid(mod1)
#Доверительный интервал
confint(mod1)
#Дисперсионный анализ модели
anova(mod1)
#Графическое представление модели
plot(mod1)

# МОДЕЛЬ 2
mod2 = lm ( co2_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux
            + rand_err_co2_flux + rand_err_h2o_flux + H_strg 
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio 
            + sonic_temperature + air_temperature + air_pressure + air_density 
            + air_heat_capacity + air_molar_volume + water_vapor_density + e + es 
            + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot 
            + v_rot + w_rot + max_speed + yaw + pitch + TKE + L + bowen_ratio 
            + x_peak + x_offset  + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux 
            + w_spikes + ts_spikes + mean_value + v_var + ts_var
            + co2_signal_strength_7200, data = teaching_tbl)

coef(mod2)
resid(mod2)
confint(mod2)
summary(mod2)
anova(mod2)
anova(mod2,mod1)
plot(mod2)

# МОДЕЛЬ 3
mod3 = lm ( co2_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux
            + rand_err_co2_flux + H_strg 
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio 
            + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot 
            + v_rot + w_rot + max_speed + yaw + pitch + TKE + L + bowen_ratio 
            + x_peak + x_offset  + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + 
            + w_spikes + ts_spikes + mean_value + v_var + ts_var 
            + co2_signal_strength_7200, data = teaching_tbl)

coef(mod3)
resid(mod3)
confint(mod3)
summary(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

# МОДЕЛЬ 4
mod4 = lm ( h2o_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux
            + rand_err_co2_flux + H_strg 
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio 
            + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot 
            + x_peak + x_offset  + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux 
            + w_spikes + ts_spikes + mean_value + v_var + ts_var
            + co2_signal_strength_7200, data = teaching_tbl)

coef(mod4)
resid(mod4)
confint(mod4)
summary(mod4)
anova(mod4)
anova(mod4, mod3)
plot(mod4)

#Корреляционный анализ переменных участвующих в линейной модели
cor_teaching_tbl = select(teaching_tbl,DOY,file_records,Tau, qc_Tau,rand_err_Tau,H, qc_H, 
                          rand_err_H, LE, qc_LE, rand_err_LE,co2_flux,
                          rand_err_co2_flux,H_strg,
                          co2_molar_density, co2_mole_fraction, co2_mixing_ratio, 
                          specific_humidity, RH, VPD, Tdew, u_unrot, v_unrot, w_unrot, u_rot, 
                          x_peak, x_offset, un_Tau, Tau_scf,un_H, H_scf, un_LE, LE_scf,un_co2_flux, 
                          w_spikes, ts_spikes, mean_value, v_var,ts_var,h2o_var,
                          co2_signal_strength_7200)

#Получение таблицы коэффициентов корреляций
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#Построение графиков по полученной моделе
#Построение точек по значениями обучающей выборки и наложение предсказанных значений по 4 модели
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))

#Построение точек по значением тестирующей выборки и наложение предсказанных значений по 4 модели
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))

#Примеры
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))
qplot(co2_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))

