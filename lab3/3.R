# === 1. Задаємо числовий ряд ===
series <- c(3.2, 7.8, 1.5, 9.0, 4.6)

# === 2. Визначення функцій ===

# Функція для сортування числового ряду
sort_series <- function(series) {
  sort(series)
}

# Побудова рівномірних інтервалів на основі діапазону чисел
build_intervals <- function(series, alphabet_size) {
  min_val <- min(series)
  max_val <- max(series)
  step <- (max_val - min_val) / alphabet_size
  breaks <- seq(min_val, max_val, by = step)

  if (length(breaks) == alphabet_size) {
    breaks <- c(breaks, max_val)
  }
  return(breaks)
}

# Перетворення чисел у відповідні символи алфавіту
map_to_symbols <- function(series, breaks, alphabet) {
  indices <- findInterval(series, breaks, rightmost.closed = TRUE)

  indices[indices == 0] <- 1
  indices[indices > length(alphabet)] <- length(alphabet)

  return(alphabet[indices])
}

# Побудова матриці передування символів
build_transition_matrix <- function(symbols, alphabet) {
  n <- length(alphabet)
  matrix <- matrix(0, nrow = n, ncol = n,
                   dimnames = list(alphabet, alphabet))

  for (i in 1:(length(symbols) - 1)) {
    from <- symbols[i]
    to <- symbols[i + 1]
    matrix[from, to] <- matrix[from, to] + 1
  }
  return(matrix)
}

# Головна функція
run_lab <- function(series, alphabet) {
  alphabet_size <- length(alphabet)
  sorted <- sort_series(series)
  breaks <- build_intervals(sorted, alphabet_size)
  symbols <- map_to_symbols(series, breaks, alphabet)
  transition_matrix <- build_transition_matrix(symbols, alphabet)
  list(symbols = symbols, matrix = transition_matrix)
}

# === 3. Визначення алфавіту ===
alphabet <- c("A", "B", "C")

# === 4. Запуск алгоритму з вимірюванням часу виконання ===
start_time <- Sys.time()
result <- run_lab(series, alphabet)
end_time <- Sys.time()
execution_time <- end_time - start_time

# === 5. Вивід результатів ===
cat("\nЛінгвістичний ряд:\n")
cat(result$symbols, sep = " ")
cat("\n")

cat("\nМатриця передування:\n")
print(result$matrix)

cat("\nЧас виконання (мілісекунди):\n")
cat(round(as.numeric(execution_time, units = "secs") * 1000, 3))