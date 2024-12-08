library(psych)
library(pheatmap)
library(FactoMineR)
library(ggplot2)
library(MASS)
library(factoextra)
data = Boston

#UNIVARIATE ANALYSIS----
## Data overview and summary statistics-----
data_type = str(data)

### defining the numerical columns----
numerical_columns = c('medv','nox','tax','indus','ptratio','black','lstat')
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

### defining the categoriacal columns----
categorical_columns = c('rm','chas','rad')
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

pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 15,fontsize_col=15,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=15,
         main="Pearson's correlation cofficient",col=terrain.colors(512))

##Scatter-Plot----
for (i in 1:(length(numerical_columns) - 1)) {
  for (j in (i + 1):length(numerical_columns)) {
    # Plot the pairwise scatter plot
    plot(data[[numerical_columns[i]]], data[[numerical_columns[j]]], 
         xlab = numerical_columns[i], ylab = numerical_columns[j], 
         main = paste("Scatter Plot of", numerical_columns[i], "vs", numerical_columns[j]),
         pch=19, col='lightblue')
    reg_line = lm(data[[numerical_columns[j]]] ~ data[[numerical_columns[i]]])
    
    # Add the regression line to the plot
    abline(reg_line, col = "red", lwd = 2)
    # Extract regression statistics
    intercept = coef(reg_line)[1]
    slope = coef(reg_line)[2]
    r_squared = summary(reg_line)$r.squared
    p_value = summary(reg_line)$coefficients[2, 4]
    
    # Format the equation and statistics text
    equation_text = paste("y = ", round(slope, 2), "x +", round(intercept, 2))
    stats_text = paste("R² = ", round(r_squared, 3), "\np-value = ", round(p_value, 10))
    
    # Add the equation and statistics to the plot
    text(x = max(data[[numerical_columns[i]]]) * 0.6, 
         y = max(data[[numerical_columns[j]]]) * 0.95, 
         labels = equation_text, col = "blue", cex = 0.8)
    # text(x = max(data[[numerical_columns[i]]]) * 0.6, 
    #      y = max(data[[numerical_columns[j]]]) * 0.85, 
    #      labels = stats_text, col = "blue", cex = 0.8)
  }
}


#Plot only for required output
plot(data$lstat, data$medv,
     xlab = 'percentage of the population in the neighborhood that belongs to the lower socioeconomic status', ylab = 'Median value of owner-occupied homes in $1000s', 
    pch=19, col='lightblue')
reg_line = lm(medv ~ lstat, data=data)
abline(reg_line, col = "red", lwd = 2)
intercept = coef(reg_line)[1]
slope = coef(reg_line)[2]
equation_text = paste("y = ", round(slope, 2), "x +", round(intercept, 2))
text(x = max(data$lstat) * 0.8,
     y = max(data$medv) * 0.9,
     labels = equation_text, col = 'red', cex = 0.8)
print(paste("equation of line:",equation_text,""))
print(summary(reg_line))
qqnorm(reg_line$residuals)
qqline(reg_line$residuals, col = "red")


## Multiple regression----
par(mfrow=c(2,2))
mult_reg = c('nox','tax','indus','ptratio','black','lstat','medv')
mult_reg_data = data[,mult_reg]
fit_mult = lm(medv~nox+tax+indus+ptratio+black+lstat,data=mult_reg_data)
summary(fit_mult)
plot(fit_mult)

shapiro.test(residuals(fit_mult))
par(mfrow=c(1,1))

# ADVANCED ANLYTICS----

pca_cols=c('crim','zn','indus','lstat','tax','nox','age','dis','ptratio','black')

data_scaled = scale(data[,pca_cols])  
pca_result = prcomp(data_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)
pca_result$x
screeplot(pca_result, main = "Scree Plot", col = "lightblue", addlabels=TRUE)

biplot(pca_result, main = "PCA Biplot")
fviz_eig(pca_result, addlabels=TRUE, color='lightblue')
fviz_pca_var(pca_result, col.var = "cos2",gradient.cols = c("black", "orange", "green"),repel = TRUE)


