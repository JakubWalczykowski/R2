## stat
#' @title stat
#' @author JW
#' @export stat
#' @description little statistician helper



stat <- function(data) {
  maxx <- vector()
  minn <- vector()
  for (i in names(data)) {
    maxx <- c(maxx, max(table(data[, i])))
    minn <- c(minn, min(table(data[, i])))
  }

  result <- data.frame(nunique = sapply(data, function(x) length(unique(x))),
                       len = nrow(data),
                       types = sapply(data, class),
                       Nulls = colSums(is.na(data)),
                       `nunique/len` = sapply(data, function(x) length(unique(x))) / nrow(data),
                       Nullpercent = colSums(is.na(data)) / nrow(data),
                       `Value counts Max` = maxx,
                       `Value counts Min` = minn,
                       `Column Max` = sapply(data, max),
                       `Column Min` = sapply(data, min))

  result <- result[order(result$nunique, decreasing = TRUE), ]
  return(result)
}

countPlot <- function(col, num = 6, hue = NULL) {
  train <- as.data.frame(train) # Assuming 'train' is the R equivalent of the 'train' dataframe

  p <- ggplot(train, aes(x = reorder(col, -table(train[, col])[col]), fill = hue)) +
    geom_bar() +
    scale_x_discrete(limits = levels(factor(train[, col]))[1:num]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p)
}
