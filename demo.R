install.packages("corrplot")
install.packages("rpart")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
library(dplyr)
library(gridExtra)
library(corrplot)
library(rpart)
library(ggplot2)

df <- read.csv("/Users/huy/Documents/doanthaytung/healthcare-dataset-stroke-data.csv")
df <- read.csv('healthcare-dataset-stroke-data.csv')
str(df)
df <- df %>%
  mutate(
    gender = as.factor(gender),
    ever_married = as.factor(ever_married),
    work_type = as.factor(work_type),
    Residence_type = as.factor(Residence_type),
    smoking_status = as.factor(smoking_status),
    bmi = as.numeric(bmi)
  ) %>%
  select(-id)

print(colSums(is.na(df)))

df_numeric <- df %>%
  mutate_if(is.factor, as.numeric)

# Tính ma trận tương quan và vẽ heatmap
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7, addCoef.col = "black")

# Xác định các biến có tương quan cao với BMI (> 0.2)
cor_with_bmi <- sort(cor_matrix["bmi", ], decreasing = TRUE)
important_features <- names(cor_with_bmi[cor_with_bmi > 0.2]) %>% setdiff("bmi")

# Xử lý dữ liệu thiếu bằng hồi quy cây quyết định
train_data <- df[!is.na(df$bmi), c(important_features, "bmi")]
missing_data <- df[is.na(df$bmi), important_features]

bmi_model <- rpart(bmi ~ ., data = train_data, method = "anova")
df$bmi[is.na(df$bmi)] <- predict(bmi_model, newdata = missing_data)

# Kiểm tra lại dữ liệu sau khi điền giá trị thiếu
print(sum(is.na(df$bmi)))

# outliers


par(mfrow = c(1,3))
boxplot(df$age, main = "Boxplot of Age", col = "skyblue", border = "black")
boxplot(df$avg_glucose_level, main = "Boxplot of Avg Glucose Level", col = "skyblue", border = "black")
boxplot(df$bmi, main = "Boxplot of BMI", col = "skyblue", border = "black")

vars_to_check <- c("age", "avg_glucose_level", "bmi")

for (var in vars_to_check) {
  Q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  num_outliers <- sum(df[[var]] < lower_bound | df[[var]] > upper_bound, na.rm = TRUE)
  
  cat("Biến:", var, "- Tổng số outliers:", num_outliers, "\n")
}

total_rows <- nrow(df)

for (var in vars_to_check) {
  Q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  num_outliers <- sum(df[[var]] < lower_bound | df[[var]] > upper_bound, na.rm = TRUE)
  percent_outliers <- (num_outliers / total_rows) * 100  # Tính phần trăm outliers
  
  cat("Biến:", var, "- Tổng số outliers:", num_outliers, 
      "- Phần trăm outliers:", round(percent_outliers, 2), "%\n")
}


for (var in vars_to_check) {
  Q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  df[[var]][df[[var]] < lower_bound] <- lower_bound
  df[[var]][df[[var]] > upper_bound] <- upper_bound
}

# Kiểm tra lại bằng boxplot
par(mfrow = c(1,3))
boxplot(df$age, main = "Age (After Winsorization)", col = "skyblue", border = "black")
boxplot(df$avg_glucose_level, main = "Avg Glucose Level (After Winsorization)", col = "skyblue", border = "black")
boxplot(df$bmi, main = "BMI (After Winsorization)", col = "skyblue", border = "black")
par(mfrow = c(1,1))

# Biểu đồ Histogram các biến số
numeric_vars <- names(df)[sapply(df, is.numeric)]  
par(mfrow = c(ceiling(length(numeric_vars)/3),3))
for (var in numeric_vars) {
  hist(df[[var]], main = paste("Histogram of", var), xlab = var, col = "green", border = "black", breaks = 20)
}
par(mfrow = c(1,1))

# Biểu đồ Barplot cho các biến phân loại
categorical_vars <- names(df)[sapply(df, is.factor)]
par(mfrow = c(ceiling(length(categorical_vars) / 3), 3))
for (var in categorical_vars) {
  barplot(table(df[[var]]), main = paste("Barplot of", var),
          col = rainbow(length(unique(df[[var]]))), las = 2)
}

par(mfrow = c(1,1))


# Biểu đồ phân bố tỷ lệ đột quỵ
stroke_counts <- table(df$stroke)
stroke_df <- data.frame(stroke = names(stroke_counts), count = as.numeric(stroke_counts))

ggplot(stroke_df, aes(x = reorder(stroke, count), y = count, fill = stroke)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("0" = "#512b58", "1" = "#fe346e")) +
  labs(title = "Tỷ lệ người bị đột quỵ",
       subtitle = "Dữ liệu bị mất cân bằng, chỉ có 5% bị đột quỵ",
       x = "Trạng thái", y = "Số lượng") +
  theme_minimal()

df$age_cat <- cut(df$age, 
                  breaks = c(0, 13, 18, 45, 60, 200), 
                  labels = c("Children", "Teens", "Adults", "Mid Adults", "Elderly"),
                  include.lowest = TRUE)

# Nhóm dữ liệu theo nhóm tuổi và số lượng người bị đột quỵ
stroke_age <- df %>% filter(stroke == 1) %>% count(age_cat)
head(stroke_age)
healthy_age <- df %>% filter(stroke == 0) %>% count(age_cat)
head(healthy_age)

# Hợp nhất dữ liệu để vẽ Dumbbell Plot
dumbbell_data <- merge(healthy_age, stroke_age, by = "age_cat", all = TRUE)
head(dumbbell_data)
# đổi tên cột 
colnames(dumbbell_data) <- c("age_cat", "healthy", "stroke")

# Thay NA bằng 0
dumbbell_data[is.na(dumbbell_data)] <- 0

# Biểu đồ Dumbbell
p1 <- ggplot(dumbbell_data, aes(y = age_cat)) +
  # vẽ đường thẳng nối 2 điểm 
  geom_segment(aes(x = healthy, xend = stroke, yend = age_cat), color = "grey") +
  # vễ điểm 
  geom_point(aes(x = healthy), size = 4, color = "#512b58", alpha = 0.8) + 
  # geom_text(aes(x =  healthy, label =  healthy), 
  #           hjust = 1.5,  # Căn chỉnh nhãn lệch về bên trái
  #           color = "#fe346e", 
  #           size = 4) +
  geom_point(aes(x = stroke), size = 4, color = "#fe346e", alpha = 0.8) +  
  geom_text(aes(x = stroke, label = stroke), 
            hjust = 1.5,  # Căn chỉnh nhãn lệch về bên trái
            color = "#fe346e", 
            size = 4) +
  labs(title = "Ảnh hưởng của tuổi tác đối với đột quỵ",
       x = "Số lượng người", y = "Nhóm tuổi") +
  theme_minimal()

print(p1)
# Biểu đồ KDE (Density Plot) so sánh phân bố tuổi
p2 <- ggplot(df, aes(x = age, fill = as.factor(stroke))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#512b58", "#fe346e"), labels = c("Healthy", "Stroke")) +
  labs(title = "Phân bố tuổi giữa người bị đột quỵ và người khỏe mạnh",
       x = "Tuổi", y = "Mật độ", fill = "Tình trạng sức khỏe") +
  theme_minimal()

print(p2)

# biểu đồ KDE: phân bố mức đường huyết giữa hai nhóm 
p3 <- ggplot(df, aes(x=avg_glucose_level, fill = as.factor(stroke))) +
    geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#512b58", "#fe346e"), labels = c("Healthy", "Stroke")) +
  labs(title = "Phân bố mức đường huyết giữa hai nhóm",
       x = "Mức đường huyết trung bình (mg/dL)", y = "Mật độ", fill = "Tình trạng sức khỏe") +
  theme_minimal()

print(p3)

# Biểu đồ tỷ lệ đột quỵ theo huyết áp cao và bệnh tim
hypertension_heart_disease <- df %>%
  group_by(hypertension, heart_disease) %>%
  summarise(stroke_rate = mean(stroke))

p4 <- ggplot(hypertension_heart_disease, aes(x = as.factor(hypertension), y = stroke_rate, fill = as.factor(heart_disease))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#512b58", "#fe346e"), labels = c("Không có bệnh tim", "Có bệnh tim")) +
  labs(title = "Tỷ lệ đột quỵ theo huyết áp và bệnh tim",
       x = "Huyết áp cao (0 = Không, 1 = Có)", y = "Tỷ lệ đột quỵ", fill = "Bệnh tim") +
  theme_minimal()

print(p4)


# Biểu đồ tỷ lệ đột quỵ theo tình trạng hút thuốc
df_percent <- df %>%
  group_by(smoking_status, stroke) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Vẽ biểu đồ với nhãn phần trăm
p5 <- ggplot(df_percent, aes(x = smoking_status, y = percent, fill = as.factor(stroke))) +
  geom_bar(stat = "identity", position = "fill") +  
  geom_text(aes(label = sprintf("%.1f%%", percent)), 
            position = position_fill(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(values = c("0" = "#512b58", "1" = "#fe346e"), labels = c("Healthy", "Stroke")) +
  labs(title = "Tỷ lệ đột quỵ theo tình trạng hút thuốc",
       x = "Tình trạng hút thuốc", y = "Tỷ lệ phần trăm", fill = "Tình trạng sức khỏe") +
  theme_minimal()

print(p5)
