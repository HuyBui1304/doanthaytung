install.packages("corrplot")
library(corrplot)
install.packages("rpart")
library(rpart)

# df <- read.csv("D:/BT/clonegit/doanthaytung/healthcare-dataset-stroke-data.csv")


df <- read.csv("/Users/huy/Documents/doanthaytung/healthcare-dataset-stroke-data.csv")

head(df)
str(df)

df$gender <- as.factor(df$gender)
df$ever_married <- as.factor(df$ever_married)
df$work_type <- as.factor(df$work_type)
df$Residence_type <- as.factor(df$Residence_type)
df$smoking_status <- as.factor(df$smoking_status)

df$bmi <- as.numeric(df$bmi)

df <- subset(df, select = -id)
str(df)

missing_values <- colSums(is.na(df))
print(missing_values)



df$gender <- as.numeric(as.factor(df$gender))
df$ever_married <- as.numeric(as.factor(df$ever_married))
df$work_type <- as.numeric(as.factor(df$work_type))
df$Residence_type <- as.numeric(as.factor(df$Residence_type))
df$smoking_status <- as.numeric(as.factor(df$smoking_status))

# Tạo ma trận tương quan, tránh mất dữ liệu
cor_matrix <- cor(df[, sapply(df, is.numeric)], use = "pairwise.complete.obs")

# Vẽ heatmap tương quan
corrplot(cor_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7, addCoef.col = "black")


cor_with_bmi <- cor_matrix["bmi", ]
cor_with_bmi <- sort(cor_with_bmi, decreasing = TRUE)
print(cor_with_bmi)



important_features <- names(cor_with_bmi[cor_with_bmi > 0.2]) 
important_features <- setdiff(important_features, "bmi") 

print(important_features)

train_data <- df[!is.na(df$bmi), c(important_features, "bmi")]
missing_data <- df[is.na(df$bmi), important_features]

bmi_model <- rpart(bmi ~ ., data = train_data, method = "anova")

df$bmi[is.na(df$bmi)] <- predict(bmi_model, newdata = missing_data)

print(sum(is.na(df)))

str(df)

df$gender <- factor(df$gender, levels = c(1, 2, 3), labels = c("Female", "Male", "Other"))
df$ever_married <- factor(df$ever_married, levels = c(1, 2), labels = c("No", "Yes"))
df$work_type <- factor(df$work_type, levels = c(1, 2, 3, 4, 5), 
                       labels = c("children", "Govt_job", "Never_worked", "Private", "Self-employed"))
df$Residence_type <- factor(df$Residence_type, levels = c(1, 2), labels = c("Rural", "Urban"))
df$smoking_status <- factor(df$smoking_status, levels = c(1, 2, 3, 4), 
                            labels = c("formerly smoked", "never smoked", "smokes", "Unknown"))

str(df)

