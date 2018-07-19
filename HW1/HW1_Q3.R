getPossibleStates = function(n) {
  states = matrix(c(FALSE, TRUE), 2, 1)
  for (i in 1:(n - 1)) {
    m1 = cbind(matrix(rep(FALSE, 2 ^ i), 2 ^ i, 1), states)
    m2 = cbind(matrix(rep(TRUE, 2 ^ i), 2 ^ i, 1), states)
    states = rbind(m1, m2)
  }
  return (states)
}

n = 16
states = getPossibleStates(n)
answers = numeric(0)
for (i in 1:2 ^ n) {
  state = states[i, ]
  isAnswer = TRUE
  for (j in 1:n) {
    left = state[(j + n - 2) %% n + 1]
    right = state[j %% n + 1]
    if (!xor(left || right, state[j]))
      isAnswer = FALSE
  }
  if (isAnswer)
    answers = unique(c(answers, sum(!state)))
}
answers

n = 12
states = getPossibleStates(n)
answers = numeric(0)
for (i in 1:2 ^ n) {
  state = states[i, ]
  isAnswer = TRUE
  for (j in 1:n) {
    left = state[(j + n - 2) %% n + 1]
    right = state[j %% n + 1]
    if (xor(xor(left, right), state[j]))
      isAnswer = FALSE
  }
  if (isAnswer)
    answers = unique(c(answers, sum(!state)))
}
answers

n = 8
states = getPossibleStates(n)
answers = numeric(0)
for (i in 1:2 ^ n) {
  for (k in 1:choose(8, 4)) {
    state = states[i, ]
    type1 = combn(8, 4)[,k]
    isAnswer = TRUE
    for (j in 1:n) {
      left = state[(j + n - 2) %% n + 1]
      right = state[j %% n + 1]
      if(j %in% type1) {
        if (xor(xor(left, right), state[j]))
          isAnswer = FALSE
      }
      else {
        if (!xor(left || right, state[j]))
          isAnswer = FALSE
      }
    }
    if (isAnswer)
      answers = unique(c(answers, sum(!state)))
  }
}
answers
