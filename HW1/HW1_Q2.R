firstConditionAnswers = list()
for (a1 in 1:13)
  for (a2 in  setdiff(1:13, a1))
    for (a3 in  setdiff(1:13, c(a1, a2)))
      for (a4 in  setdiff(1:13, c(a1, a2, a3)))
        for (a5 in  setdiff(1:13, c(a1, a2, a3, a4))) {
          x = a1 / (a2 + a3)
          y = (a2 + a3) / (a3 + a4 + a5)
          if (x == y && x < 1) {
            answer = c(a1, a2, a3, a4, a5)
            index = length(firstConditionAnswers) + 1
            firstConditionAnswers[[index]] = answer
          }
        }
secondConditionAnswers = list()
for(first5 in firstConditionAnswers)
  for (a6 in  setdiff(1:13, first5))
    for (a7 in  setdiff(1:13, c(first5, a6)))
      for (a8 in  setdiff(1:13, c(first5, a6, a7)))
        for (a9 in  setdiff(1:13, c(first5, a6, a7, a8)))
          for (a10 in  setdiff(1:13, c(first5, a6, a7, a8, a9))) {
            x = a6 / (a6 + a7)
            y = (a6 + a7) / (a7 + a8 + a9)
            z = (a7 + a8 + a9) / (first5[5] + a9 + a10)
            if (x == y && y == z && x < 1) {
              answer = c(first5, a6, a7, a8, a9, a10)
              index = length(secondConditionAnswers) + 1
              secondConditionAnswers[[index]] = answer
            }
          }
answers = list()
for(first10 in secondConditionAnswers)
  for (a11 in  setdiff(1:13, first10))
    for (a12 in  setdiff(1:13, c(first10, a11)))
      for (a13 in  setdiff(1:13, c(first10, a11, a12))) {
        x = (a11 + a12) / (a12 + a13)
        y = (a12 + a13) / (a13 + first10[10])
        if (x == y && x < 1) {
          answer = c(first10, a11, a12, a13)
          index = length(answers) + 1
          answers[[index]] = answer
        }
      }
for (answer in answers) {
  print(answer)
}