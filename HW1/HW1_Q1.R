check = function(mat) {
  n = nrow(mat)
  m = n * (n * n + 1) / 2
  for (i in 1:n) {
    if (sum(mat[i, ]) != m || sum(mat[, i]) != m)
      return(FALSE)
  }
  return(TRUE)
}

n = 4
mat = matrix(1:n ^ 2, n, n)
for (i in c(1:(n / 4), (3 * n / 4 + 1):n)) {
  for (j in c(1:(n / 4), (3 * n / 4 + 1):n)) {
    mat[i, j] = n * n + 1 - mat[i, j]
  }
}
for (i in (n / 4 + 1):(3 * n / 4)) {
  for (j in (n / 4 + 1):(3 * n / 4)) {
    mat[i, j] = n * n + 1 - mat[i, j]
  }
}
mat
check(mat)

n = 5
odd_magic = function(n) {
  mat = matrix(rep(0, n * n), n, n)
  col = (n - 1) / 2
  row = 0
  for (i in 0:(n * n - 1)) {
    mat[row + 1, col + 1] = i + 1
    next_row = (row - 1) %% n
    next_col = (col + 1) %% n
    if (mat[next_row + 1, next_col + 1] != 0) {
      next_row = (row + 1) %% n
      next_col = col
    }
    row = next_row
    col = next_col
  }
  return(mat)
}
odd_magic(5)
check(odd_magic(5))

n = 6
m = (n - 2) / 4
mat = matrix(rep(0, n * n), n, n)
base = odd_magic(n / 2)
for (i in 1:(m + 1)) {
  for (j in 1:(n / 2)) {
    x = base[i, j]
    mat[2 * i, 2 * j] = 4 * x - 1
    mat[2 * i - 1, 2 * j - 1] = 4 * x
    mat[2 * i - 1, 2 * j] = 4 * x - 3
    mat[2 * i, 2 * j - 1] = 4 * x - 2
  }
}
i = m + 2
for (j in 1:(n / 2)) {
  x = base[i, j]
  mat[2 * i, 2 * j] = 4 * x - 1
  mat[2 * i - 1, 2 * j - 1] = 4 * x - 3
  mat[2 * i - 1, 2 * j] = 4 * x
  mat[2 * i, 2 * j - 1] = 4 * x - 2
}
if (m + 3 <= 2 * m + 1) {
  for (i in (m + 3):(n / 2)) {
    for (j in 1:(n / 2)) {
      x = base[i, j]
      mat[2 * i, 2 * j] = 4 * x - 2
      mat[2 * i - 1, 2 * j - 1] = 4 * x
      mat[2 * i - 1, 2 * j] = 4 * x - 3
      mat[2 * i, 2 * j - 1] = 4 * x - 1
    }
  }
}
tmp = mat[n / 2, n / 2]
mat[n / 2, n / 2] = mat[n / 2, n / 2 + 1]
mat[n / 2, n / 2 + 1] = tmp

tmp = mat[n / 2 + 2, n / 2]
mat[n / 2 + 2, n / 2] = mat[n / 2 + 2, n / 2 + 1]
mat[n / 2 + 2, n / 2 + 1] = tmp

mat
check(mat)
