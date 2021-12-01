#Cardiovascular risks - Initial Visualizations and Analysis


library(faraway)

df <- diabetes

?diabetes


summary(df$location)
summary(df$ratio)
summary(df$time.ppn)


library(tidyverse)


ggplot(df, aes(x=glyhb ,y= chol))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Glycosolated Hemoglobin", y="Cholesterol", title="Cholesterol against Glycosolated Hemoglobin")

ggplot(df, aes(x=weight ,y=ratio ))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Weight", y="Ratio of Cholesterol to HDL", title="Ratio against Weight")


ggplot(df, aes(x=hdl ,y= chol))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="High Density Lipotene", y="Cholesterol", title="High Density Lipotene against Glycosolated Hemoglobin")

ggplot(df, aes(x=chol, color = gender))+
  geom_boxplot()+
  labs(x = "Cholesterol", title= "Cholesterol Box Plot by Gender", 
       color = "Gender")

ggplot(df, aes(x = weight, y = chol))+
  
  
  
  
  ggplot(df, aes(x=weight ,y= chol))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Weight", y="Cholesterol", title="Cholesterol against Weight Scatter Plot")

ggplot(df, aes(x=bmi, color = diabetes))+
  geom_histogram(na.rm=TRUE)+
  labs(x = "Body Mass Index", color = "Diabetes")

ggplot(df, aes(x= chol))+
  geom_histogram()+
  labs(x = "Cholesterol (mg/dL)", y = "Total Observations", title = "Cholesterol Histogram")

ggplot(df, aes(x = weight, y = ratio, color = gender))+
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE)+
  labs(x = "Weight", y = "Cholesterol to HDL Ratio", color = "Gender", title = "Cholesterol to HDL Ratio against Weight between Genders")

df$logratio=log(df$ratio)


ggplot(df, aes(x = ratio, y = weight,))+
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE)+
  labs(x = "Ratio between Cholesterol and HDL", y = "Weight", title = "Ratio against Weight")

ratio_weight <-lm(ratio~weight, data=df).
summary(ratio_weight)

df<-drop_na(chol,weight,glyhb, data = df)

df$yhat<-ratio_weight$fitted.values
df$residual<-ratio_weight$residuals

ggplot(df, aes(x=yhat ,y=residual))+
  geom_point(shape=5)+
  geom_hline(yintercept=0, color="red")+
  labs(x="yfit", y="Residual", title="Residual Plot of Weight to Cholesterol")


ggplot(df, aes(x = chol, color= gender))+
  geom_histogram()+
  labs(x = "Cholesterol", y = "Total", title = "Cholesterol Levels Histogram")



correlations <- cor(df)



df<-drop_na(chol,weight,glyhb, data = df)

df_line <- lm(chol~weight, data=df)
summary(df_line)

all_line <- lm(glyhb~., data= df)
summary(all_line)

acf(all_line)

complete.cases()
hdl_line <- lm(ratio~., data = df)
summary(hdl_line)

acf(hdl_line)

df$yhat<-df_line$fitted.values
df$residual<-df_line$residuals

ggplot(df, aes(x=yhat ,y=residual))+
  geom_point(shape=5)+
  geom_hline(yintercept=0, color="red")+
  labs(x="yfit", y="Residual", title="Residual Plot of Weight to Cholesterol")

ggplot(df, aes(x=))

gly <- cor.test(df$chol, df$glyhb, 
                method = "pearson")
gly

weight <-cor.test(df$chol, df$weight)
weight

weight<-cor.test(df$chol, df$weight)
weight

chol_line <- lm(chol~., data = df)
summary(chol_line)



df$bmi <- (df$weight / (df$height)^2) * 703

ggplot(df, aes(x=glyhb ,y= glyhb, color=location))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="ratio", y="bmi", title="bmi against hdl ratio")

choltobmi_line <- lm(ratio~bmi, data = df)
summary(choltobmi_line)

df <- subset(df, select = -id)

ggplot(df, aes(x = weight, y = glyhb, color = gender))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  scale_x_continuous(n.breaks=10)+
  labs(x="Weight", y = "Glycosolated Hemoglobin",
       title = "Weight against Glycosolated Hemoglobin", color = "Gender")



weight (lb) / height (in)^2 * 703


anova(df_line)

summary(df$diabetes)


pairs(df, lower.panel = NULL)

59/329


ggplot(df, aes(x=weight ,y= hdl))+
  geom_point(shape=5)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Weight", y="HDL", title="High Density Lipotene against Weight")




ggplot(df, aes(x=age, y = weight))+
  geom_point(shape=5)+
  geom_smooth(method="lm", se=FALSE)+
  labs(x="Age", y= "Weight", title = "Weight Against Age")


hdl_line <- lm(hdl~., data = df)
summary(hdl_line)

acf(hdl_line)

regnull <- lm(hdl~1, data=df)
##model with all predictors
regfull <- lm(hdl~., data=df)
step(regfull, scope=list(lower=regfull, upper=regnull), direction="backward")

df$diabetes <- df$glyhb > 7

ggplot(df, aes(x=chol, color = diabetes))+
  geom_histogram()+
  labs(x="Diabetes", title = "Diabetes by Gender")

ggplot(df, aes(x=diabetes, y = hdl))+
  geom_boxplot(method="lm", se=FALSE)+
  labs(x="Diabetes", y= "HDL", title = "Choleasdferoasdf")


ggplot(df, aes(x=diabetes))+
  geom_bar()+
  labs(x="Diabetes", y= "Total", title = "Positive Diabetes Tests")

ggplot(df, aes(x=bmi, y = weight, color = gender))+
  geom_point(shape=5)+
  geom_smooth(method="lm", se=FALSE)+
  labs(x="Weight", y= "Cholesterol", title = "Cholesterol against Weight and Gender")

ggplot(df, aes(x=diabetes, y = chol))+
  geom_boxplot()+
  labs(x="Diabetes Risk", y= "Cholesterol", title = "Cholesterol against Diabetes Risk")



ggplot(df, aes(x=diabetes, fill = gender))+
  geom_bar()+
  labs(x = "Diabetes Risk", y = "Total Observations",
       title = "Diabetes Risk by Gender", fill = "Gender")




summary(df$hip)
summary(df$waist)
summary(df$age)
summary(df$gender)
summary(df$height)
mean(df$height)
summary(df$weight)
summary(df$frame)