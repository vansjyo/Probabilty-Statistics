#impimetation of the prinicipal component analysis using the IRIS dataset
input = read.csv("/home/dell/Desktop/R_probability/Iris.csv")
names(input)
str(input)

#using the prcomp function
model = prcomp(input[,1:4], scale=TRUE)
model$sdev
model$rotation
model$center
model$scale
par(mfrow=c(2,2))
plot(model$x[,1], col=input[,5])
plot(model$x[,2], col=input[,5])
plot(model$x[,3], col=input[,5])
plot(model$x[,4], col=input[,5])
model$sdev^2 / sum(model$sdev^2)
plot(model)

## Normalize the input feature. (without using prcomp fucntion)
input$sepal_len1 = (input$SepalLengthCm - mean(input$SepalLengthCm) )/sd(input$SepalLengthCm)
input$sepal_wid1 = (input$SepalWidthCm - mean(input$SepalWidthCm))/sd(input$SepalWidthCm)
input$petal_len1 = (input$PetalLengthCm - mean(input$PetalLengthCm))/sd(input$PetalLengthCm)
input$petal_wid1 = (input$PetalWidthCm - mean(input$PetalWidthCm))/sd(input$PetalWidthCm)

##Get the covarience matrix and eigen vector.
matrix_form = matrix(c(input$sepal_len1, input$sepal_wid1, input$petal_len1, input$petal_wid1), ncol=4)
m = cov(matrix_form)
eigenV = eigen(m)
eigenV$vectors