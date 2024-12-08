library(psych)
library(ggplot2)
library(pheatmap)
library(FactoMineR)
library(factoextra)

data = diamonds

#UNIVARIATE ANALYSIS----
## Data overview and summary statistics-----
data_type = str(data)

### defining the numerical columns----
numerical_columns = c('carat','depth','table','price','x','y','z')
data_description_num = as.data.frame(describe(data)[numerical_columns,])
View(data_description_num)

# Ploting and oulier detection
par(mfrow=c(1,2))
for (i in numerical_columns){
  hist(data[[i]],col='lightblue',main=paste("Histogram of",i,""),
       xlab=i, ylab="Frequency")
  boxplot(data[[i]],col='lightblue',outpch=19,outcol='red',
          main=paste("Histogram of",i,""), xlab=i, ylab="Frequency")
}
par(mfrow=c(1,1),mar=c(4,2,4,2))

### defining the categorical columns----
categorical_columns = c('cut','color','clarity')
for (j in categorical_columns){
  col_counts = table(data[[j]])
  barplot(col_counts,xlab=j,ylab="frequency",main=j,col='lightblue',ylim = c(0, max(col_counts) + 2))
  text(x = seq_along(col_counts),y = col_counts,labels = col_counts,pos = 3, 
       col = "red")
}
#MULTIVARIATE ANALYSIS----
## Corrrelation analysis----

corr_data = data[,numerical_columns]
corr_matrix = cor(corr_data)

pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 30,fontsize_col=30,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=30,
         main="Pearson's correlation cofficient",col=terrain.colors(512))



##Scatter-Plot----

plot(data$carat, data$price,
     xlab = 'carat', ylab = 'price', 
     main = "Scatter Plot of miles per gallon vs vehicle weight",pch=19, col='lightblue')
reg_line = lm(price ~ carat, data=data)
abline(reg_line, col = "red", lwd = 2)
intercept = coef(reg_line)[1]
slope = coef(reg_line)[2]
equation_text = paste("y = ", round(slope, 2), "x +", round(intercept, 2))
text(x = max(data$carat) * 0.8,
     y = max(data$price) * 0.9,
     labels = equation_text, col = 'red', cex = 0.8)
print(paste("equation of line:",equation_text,""))
print(summary(reg_line))
qqnorm(reg_line$residuals)
qqline(reg_line$residuals, col = "red")


numerical_columns = c('carat','depth','table','price','x','y','z')
## Multiple regression----
par(mfrow=c(2,2))
mult_reg = c('carat','depth','table','x','y','z','price')
mult_reg_data = data[,mult_reg]
fit_mult = lm(price~carat+depth+table+x+y+z,data=mult_reg_data)
summary(fit_mult)
plot(fit_mult)



# ADVANCED ANLYTICS----

pca_cols = c('carat','depth','table','x','y','z')
data_scaled = scale(data[,pca_cols])  
pca_result = prcomp(data_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)
pca_result$x
screeplot(pca_result, main = "Scree Plot", col = "lightblue", addlabels=TRUE)

biplot(pca_result, main = "PCA Biplot")
fviz_eig(pca_result, addlabels=TRUE, color='lightblue')
fviz_pca_var(pca_result, col.var = "cos2",gradient.cols = c("black", "orange", "green"),repel = TRUE)

