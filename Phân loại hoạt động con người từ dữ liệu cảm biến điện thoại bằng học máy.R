## --------------------------------------------------------
# train <-read.csv('/Users/huy/Documents/doanthaytung/archive/train.csv')
# train <-read.csv("D:/BT/clonegit/doanthaytung/archive/train.csv")
train <- read.csv('archive/train.csv')
# test <-read.csv('/Users/huy/Documents/doanthaytung/archive/test.csv')
# test <-read.csv("D:/BT/clonegit/doanthaytung/archive/test.csv")
test <- read.csv("archive/test.csv")
#head(train)
dim(train)
#head(test)
dim(test)


## --------------------------------------------------------
table(sapply(train, class))


## --------------------------------------------------------
names(train)[sapply(train, class) == "character"]


## --------------------------------------------------------
unique(train$Activity)



## --------------------------------------------------------
train$Activity <- as.factor(train$Activity)
test$Activity <- as.factor(test$Activity)


## --------------------------------------------------------
cat("Giá trị thiếu ở tập train:", sum(is.na(train)), "\n")
cat("Giá trị thiếu ở tập test:", sum(is.na(test)), "\n")


## --------------------------------------------------------
cat("Số dòng bị trùng lặp trong tập train:", sum(duplicated(train)), "\n")
cat("Số dòng bị trùng lặp trong tập test :", sum(duplicated(test)), "\n")


## --------------------------------------------------------
columns <- colnames(train)
columns <- gsub("\\.", "", columns)
colnames(train) <- columns
colnames(test) <- columns


## ----fig.width=16, fig.height=8, fig.cap=" Số lượng mẫu dữ liệu theo người dùng và hoạt động"----
library(ggplot2)
library(showtext)
showtext_auto()

ggplot(train, aes(x = factor(subject), fill = Activity)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "Số lượng mẫu dữ liệu theo người dùng và hoạt động",
    x = "Người dùng (Subject)",
    y = "Số lượng mẫu",
    fill = "Hoạt động"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


## ----fig.width=18, fig.height=8, fig.cap="Số lượng mẫu theo từng hoạt động"----

library(ggplot2)

ggplot(train, aes(x = Activity, fill = Activity)) +
  geom_bar() +
  labs(
    title = "Số lượng mẫu theo từng hoạt động",
    x = "Hoạt động",
    y = "Số lượng mẫu"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  guides(fill = "none")


## ----fig.width=18,warning=FALSE,message=FALSE, fig.height=8,fig.cap="Phân bố tBodyAccMagmean theo hoạt động"----
library(ggplot2)

ggplot(train, aes(x = tBodyAccMagmean, color = Activity)) +
  geom_density(size = 1.2) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(-1.1, 1)) + 
  labs(
    title = "Phân bố tBodyAccMagmean theo hoạt động",
    x = "tBodyAccMagmean",
    y = "Mật độ"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


## ----warning=FALSE,message=FALSE,fig.cap="Phân bố tBodyAccMagmean theo hoạt động sitting, standing, laying"----
library(ggplot2)
library(dplyr)
library(gridExtra)

p1 <- train %>%
  filter(Activity %in% c("SITTING", "STANDING", "LAYING")) %>%
  ggplot(aes(x = tBodyAccMagmean, color = Activity)) +
  geom_density(size = 1.2) +
  labs(
    title = "Static Activities (closer view)",
    x = "tBodyAccMagmean",
    y = "Density"
  ) +
  xlim(-1.05, -0.1) +
  ylim(0, 35) +
  theme_minimal(base_size = 14)
p1


## ----fig.cap="Phân bố tBodyAccMagmean theo hoạt động walking, walking_dowstairs, walking_upstairs"----
p2 <- train %>%
  filter(Activity %in% c("WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS")) %>%
  ggplot(aes(x = tBodyAccMagmean, color = Activity)) +
  geom_density(size = 1.2) +
  labs(
    title = "Dynamic Activities (closer view)",
    x = "tBodyAccMagmean",
    y = "Density"
  ) + xlim(-0.65, 1) +
  theme_minimal(base_size = 14)

p2



## ----fig.cap="Phân bố giữa tBodyAccMagmean và Activity"----
library(ggplot2)

ggplot(train, aes(x = Activity, y = tBodyAccMagmean)) +
  geom_boxplot(
    outlier.shape = NA,
    fill = "skyblue",
    color = "darkgreen",
    width = 0.6
  ) +
  labs(
    title = "Phân bố giữa tBodyAccMagmean và Activity",
    y = "tBodyAccMagmean",
    x = "Activity"
  ) +
  coord_cartesian(ylim = c(-1.1, 1.2)) + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10))
  )


## ----fig.cap="Phân bố giữa angleXgravityMean và Activity"----
library(ggplot2)

ggplot(train, aes(x = Activity, y = angleXgravityMean)) +
  geom_boxplot(
    fill = "lightblue",
    color = "darkblue",
    outlier.shape = NA,
    width = 0.6
  ) +
  labs(
    title = "Phân bố giữa angleXgravityMean và Activity",
    x = "Activity",
    y = "angleXgravityMean"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text.x = element_text(angle = 40, hjust = 1, size = 11)
  )


## ----fog.cap="Phân bố giữa angleYgravityMean và Activity"----
library(ggplot2)

ggplot(train, aes(x = Activity, y = angleYgravityMean)) +
  geom_boxplot(
    fill = "lightblue",
    color = "darkblue",
    outlier.shape = NA,
    width = 0.6
  ) +
  labs(
    title = "Phân bố giữa angleYgravityMean và Activity",
    x = "Activity",
    y = "angleYgravityMean"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text.x = element_text(angle = 40, hjust = 1, size = 11)
  )



## --------------------------------------------------------
subject_ids <- train$subject
activity_labels <- train$Activity
sensor_data <- train[, !(names(train) %in% c("Activity", "subject"))]
subject_ids_test <- test$subject
activity_labels_test <- test$Activity
sensor_data_test <- test[, !(names(test) %in% c("Activity", "subject"))]

is_outlier_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  x < lower_bound | x > upper_bound
}

outlier_matrix <- mapply(is_outlier_iqr, sensor_data)

total_outlier_values <- sum(outlier_matrix)
total_cells <- nrow(sensor_data) * ncol(sensor_data)
percent_outlier_cells <- round(total_outlier_values / total_cells * 100, 2)

cat("Tổng số giá trị outlier:", total_outlier_values, "/", total_cells, "\n")
cat("Tỷ lệ giá trị outlier:", percent_outlier_cells, "%\n")


## --------------------------------------------------------
 cap_outliers <- function(df, lower = 0.01, upper = 0.99) {
   for (col in names(df)) {
     q_low <- quantile(df[[col]], lower, na.rm = TRUE)
     q_high <- quantile(df[[col]], upper, na.rm = TRUE)
     df[[col]] <- pmin(pmax(df[[col]], q_low), q_high)
   }
   df
 }

 sensor_data_capped <- cap_outliers(sensor_data)
 sensor_data_test_capped <- cap_outliers(sensor_data_test)

 train_capped <- cbind(sensor_data_capped, subject = subject_ids, Activity = activity_labels)
 test_capped <- cbind(sensor_data_test_capped, subject = subject_ids_test, Activity = activity_labels_test)




## ----fig.width=16, fig.height=8, fig.cap={' trực quan hóa UMAP'}----
library(umap)
library(ggplot2)
library(gridExtra)

perform_umap_grid <- function(X_data, y_data,
                              neighbors_list = c(5, 15, 30),
                              min_dist_list = c(0.001, 0.01, 0.1)) {
  plots <- list()
  index <- 1

  for (n in neighbors_list) {
    for (d in min_dist_list) {
      config <- umap.defaults
      config$n_neighbors <- n
      config$min_dist <- d
      config$n_components <- 2

      umap_result <- umap(X_data, config = config)
      df <- as.data.frame(umap_result$layout)
      colnames(df) <- c("x", "y")
      df$label <- y_data

      p <- ggplot(df, aes(x = x, y = y, color = label)) +
        geom_point(size = 0.6, alpha = 0.6) +
        scale_color_brewer(palette = "Set1") +
        theme_minimal(base_size = 10) +
        labs(title = paste("n =", n, ", dist =", d), x = NULL, y = NULL) +
        theme(legend.position = "none",
              plot.title = element_text(size = 10, hjust = 0.5))

      plots[[index]] <- p
      index <- index + 1
    }
  }

  do.call(grid.arrange, c(plots, ncol = length(min_dist_list)))
}

perform_umap_grid(
  X_data = sensor_data_capped,
  y_data = train$Activity,
  neighbors_list = c(5, 15, 30),
  min_dist_list = c(0.001, 0.01, 0.1)
)


## ----fig.cap=" trực quan hóa kết quả giảm chiều dữ liệu bằng UMAP"----
library(umap)
library(ggplot2)

config <- umap.defaults
config$n_neighbors <- 30
config$min_dist <- 0.1
config$n_components <- 2

umap_result <- umap(sensor_data_capped, config = config)

umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP_1", "UMAP_2")
umap_df$Activity <- train$Activity

ggplot(umap_df, aes(x = UMAP_1, y = UMAP_2, color = Activity)) +
  geom_point(size = 0.7, alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  labs(
    title = "UMAP (n_neighbors = 30, min_dist = 0.01)",
    x = "UMAP 1", y = "UMAP 2"
  )


## --------------------------------------------------------
X_train <- train_capped[, !(names(train_capped) %in% c("subject", "Activity"))]
y_train <- train_capped$Activity

X_test <- test_capped[, !(names(test_capped) %in% c("subject", "Activity"))]
y_test <- test_capped$Activity

dim(X_train)
length(y_train)

dim(X_test)
length(y_test)


## ----warning=FALSE---------------------------------------
train_and_evaluate_model <- function(
  model_name,
  X_train, y_train, X_test, y_test,
  class_labels,
  tuneGrid = NULL,
  k_folds = 5
) {
  if (!require(caret)) install.packages("caret", quiet = TRUE)
  if (!require(ggplot2)) install.packages("ggplot2", quiet = TRUE)

  library(caret)
  library(ggplot2)

  y_train <- as.factor(y_train)
  y_test <- as.factor(y_test)

  ctrl <- trainControl(method = "cv", number = k_folds)

  set.seed(123)
  model_fit <- train(
    x = X_train,
    y = y_train,
    method = model_name,
    trControl = ctrl,
    tuneGrid = tuneGrid
  )

  best_param <- model_fit$bestTune

  final_model <- train(
    x = X_train,
    y = y_train,
    method = model_name,
    trControl = trainControl(method = "none"),
    tuneGrid = best_param
  )

  y_pred <- predict(final_model, X_test)

  cm <- confusionMatrix(y_pred, y_test)
  report <- cm$byClass[, c("Precision", "Recall", "F1")]
  test_acc <- mean(y_pred == y_test)

  macro_precision <- mean(report[, "Precision"], na.rm = TRUE)
  macro_recall <- mean(report[, "Recall"], na.rm = TRUE)
  macro_f1 <- mean(report[, "F1"], na.rm = TRUE)

  support <- as.numeric(table(y_test))
  weighted_precision <- weighted.mean(report[, "Precision"], w = support, na.rm = TRUE)
  weighted_recall <- weighted.mean(report[, "Recall"], w = support, na.rm = TRUE)
  weighted_f1 <- weighted.mean(report[, "F1"], w = support, na.rm = TRUE)

  cat('--------------------------\n')
  cat('|     Best Parameters     |\n')
  cat('--------------------------\n')
  for (param in names(best_param)) {
    cat(sprintf("%s = %s\n", param, best_param[[param]]))
  }

  cat('--------------------------\n')
  cat('|     Best CV Accuracy    |\n')
  cat('--------------------------\n')
  print(round(max(model_fit$results$Accuracy), 4))

  cat('--------------------------\n')
  cat('|      Test Accuracy      |\n')
  cat('--------------------------\n')
  print(round(test_acc, 4))

  cat('--------------------------\n')
  cat('| Precision / Recall / F1 |\n')
  cat('--------------------------\n')
  print(round(report, 4))

  cat('------------------------------\n')
  cat('|   Macro Avg (All Classes)  |\n')
  cat('------------------------------\n')
  cat(sprintf("Precision: %.4f\nRecall   : %.4f\nF1-score : %.4f\n",
              macro_precision, macro_recall, macro_f1))

  cat('------------------------------\n')
  cat('| Weighted Avg (By Support)  |\n')
  cat('------------------------------\n')
  cat(sprintf("Precision: %.4f\nRecall   : %.4f\nF1-score : %.4f\n",
              weighted_precision, weighted_recall, weighted_f1))

  cm_table <- as.data.frame(cm$table)
  colnames(cm_table) <- c("Predicted", "Actual", "Freq")

  p <- ggplot(data = cm_table, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 5) +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    labs(title = "Confusion Matrix", x = "True Label", y = "Predicted Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p)

  return(list(
    model = final_model,
    best_parameters = best_param,
    best_cv_accuracy = max(model_fit$results$Accuracy),
    test_accuracy = test_acc,
    report = report,
    macro_metrics = c(Precision = macro_precision, Recall = macro_recall, F1 = macro_f1),
    weighted_metrics = c(Precision = weighted_precision, Recall = weighted_recall, F1 = weighted_f1),
    confusion_matrix = cm$table,
    prediction = y_pred
  ))
}


## ----fig.cap="confusion matrix Logistic Regression"------
grid_logistic <- expand.grid(
  alpha = c(0, 1),
  lambda = 1 / c(0.01, 0.1, 1, 10, 20, 30)
)

results_log <- train_and_evaluate_model(
  model_name = "glmnet",
  X_train = X_train,
  y_train = y_train,
  X_test = X_test, 
  y_test = y_test,
  class_labels = labels,
  tuneGrid = grid_logistic,
  k_folds = 5
)


## ----fig.cap="confusion matrix SVC"----------------------
grid_linear_svc <- expand.grid(C = c(0.125, 0.5, 1, 2, 8, 16))

results_svc <- train_and_evaluate_model(
  model_name = "svmLinear",
  X_train = X_train, y_train = y_train,
  X_test = X_test, y_test = y_test,
  class_labels = labels,
  tuneGrid = grid_linear_svc,
  k_folds = 5
)


## ----fig.cap="confusion matrix decision tree"------------
grid_dt <- expand.grid(maxdepth = seq(3, 9, by = 2))

results_dt <- train_and_evaluate_model(
  model_name = "rpart2",
  X_train = X_train,
  y_train = y_train,
  X_test = X_test,
  y_test = y_test,
  class_labels = labels,
  tuneGrid = grid_dt,
  k_folds = 5
)



## ----warning=FALSE, message=FALSE, fig.cap="confusion matrix random forest"----
library(randomForest)
grid_rf <- expand.grid(mtry = c(16, 20, 24, 28, 32))
results_rf <- train_and_evaluate_model(
  model_name = "rf",
  X_train = X_train,
  y_train = y_train,
  X_test = X_test,
  y_test = y_test,
  class_labels = labels,
  tuneGrid = grid_rf,
  k_folds = 5
)


## ----fig.cap="confusion matrix KNN"----------------------
grid_knn <- expand.grid(k = seq(3, 30, by = 2))  

results_knn <- train_and_evaluate_model(
  model_name = "knn",
  X_train = X_train, 
  y_train = y_train,
  X_test = X_test, 
  y_test = y_test,
  class_labels = labels,
  tuneGrid = grid_knn,
  k_folds = 5
)


## --------------------------------------------------------
model_summary <- data.frame(
  Model = c("Logistic Regression", "SVM (Linear)", "Decision Tree", "Random Forest", "KNN"),
  Accuracy = c(
    results_log$test_accuracy,
    results_svc$test_accuracy,
    results_dt$test_accuracy,
    results_rf$test_accuracy,
    results_knn$test_accuracy
  ),
  Macro_F1 = c(
    results_log$macro_metrics["F1"],
    results_svc$macro_metrics["F1"],
    results_dt$macro_metrics["F1"],
    results_rf$macro_metrics["F1"],
    results_knn$macro_metrics["F1"]
  ),
  Weighted_F1 = c(
    results_log$weighted_metrics["F1"],
    results_svc$weighted_metrics["F1"],
    results_dt$weighted_metrics["F1"],
    results_rf$weighted_metrics["F1"],
    results_knn$weighted_metrics["F1"]
  )
)

print(model_summary)


## ----warning=FALSE, message=FALSE------------------------
library(ggplot2)
library(tidyr)
library(dplyr)

model_summary_long <- model_summary %>%
  pivot_longer(cols = c("Accuracy", "Macro_F1", "Weighted_F1"),
               names_to = "Metric", values_to = "Value")

ggplot(model_summary_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "So sánh hiệu suất các mô hình",
    x = "Mô hình",
    y = "Giá trị",
    fill = "Chỉ số đánh giá"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


