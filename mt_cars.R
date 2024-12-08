library(psych)
library(pheatmap)
library(FactoMineR)
library(ggplot2)
library(factoextra)

data = mtcars
#UNIVARIATE ANALYSIS----
## Data overview and summary statistics-----
data_type = str(data)

### defining the numerical columns----
numerical_columns = c('mpg', 'hp', 'wt')
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
par(mfrow=c(1,1))

### defining the categorical columns----
categorical_columns = c('cyl','gear','carb')
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
for (i in 1:(length(numerical_columns) - 1)) {
  for (j in (i + 1):length(numerical_columns)) {
    print(paste("Regression statistics for ",numerical_columns[i],"and",numerical_columns[j]))
    
    # Scatter plots
    plot(data[[numerical_columns[i]]], data[[numerical_columns[j]]], 
         xlab = numerical_columns[i], ylab = numerical_columns[j], 
         main = paste("Scatter Plot of", numerical_columns[j], "vs", numerical_columns[i]),
         pch=19, col='lightblue')
    reg_line = lm(data[[numerical_columns[j]]] ~ data[[numerical_columns[i]]])
    abline(reg_line, col = "red", lwd = 2)
    # Adding line to the plot
    intercept = coef(reg_line)[1]
    slope = coef(reg_line)[2]
    equation_text = paste("y = ", round(slope, 2), "x +", round(intercept, 2))
    text(x = max(data[[numerical_columns[i]]]) * 0.8,
         y = max(data[[numerical_columns[j]]]) * 0.9,
         labels = equation_text, col = 'red', cex = 0.8)
    print(paste("equation of line:",equation_text,""))
    print(summary(reg_line))
    qqnorm(reg_line$residuals)
    qqline(reg_line$residuals, col = "red")
    
  }
}

#Plot only for required output
plot(data$wt, data$mpg,
    xlab = 'vehicle weight', ylab = 'miles per gallon', 
    main = "Scatter Plot of miles per gallon vs vehicle weight",pch=19, col='lightblue')
reg_line = lm(mpg ~ wt, data=data)
abline(reg_line, col = "red", lwd = 2)
intercept = coef(reg_line)[1]
slope = coef(reg_line)[2]
equation_text = paste("y = ", round(slope, 2), "x +", round(intercept, 2))
text(x = max(data$wt) * 0.8,
     y = max(data$mpg) * 0.9,
     labels = equation_text, col = 'red', cex = 0.8)
print(paste("equation of line:",equation_text,""))
print(summary(reg_line))
qqnorm(reg_line$residuals)
qqline(reg_line$residuals, col = "red")


## Multiple regression----
par(mfrow=c(2,2))
mult_reg = c('mpg','hp','wt')
mult_reg_data = data[,mult_reg]
fit_mult = lm(mpg~hp+wt,data=mult_reg_data)
print(summary(fit_mult))
plot(fit_mult)


shapiro.test(residuals(fit_mult))
par(mfrow=c(1,1))



# ADVANCED ANLYTICS----
pca_cols=c('mpg','disp','hp','drat','qsec')

data_scaled = scale(data[,pca_cols])  
pca_result = prcomp(data_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)
pca_result$x
screeplot(pca_result, main = "Scree Plot", col = "lightblue", addlabels=TRUE)

biplot(pca_result, main = "PCA Biplot")
fviz_eig(pca_result, addlabels=TRUE, color='lightblue')
fviz_pca_var(pca_result, col.var = "cos2",gradient.cols = c("black", "orange", "green"),repel = TRUE)



