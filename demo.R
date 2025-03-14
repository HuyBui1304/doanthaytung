# df <- read.csv("/Users/huy/Documents/doanthaytung/healthcare-dataset-stroke-data.csv")
df <- read.csv("D:/BT/clonegit/doanthaytung/healthcare-dataset-stroke-data.csv")
head(df)
str(df)

# chuyển sang dạng factor 
df$gender <- as.factor(df$gender)
df$ever_married <- as.factor(df$ever_married)
df$work_type <- as.factor(df$work_type)
df$Residence_type <- as.factor(df$Residence_type)
df$smoking_status <- as.factor(df$smoking_status)

# xem dữ liệu
str(df)

# chuyển sang dạng numeric
df$bmi <- as.numeric(df$bmi)

# xem dữ liệu 
str(df)

# xoá cột id
df <- subset(df, select = -id)
str(df)

# kiểm tra dữ liệu thiếu 
missing_values <- colSums(is.na(df))
missing_values


