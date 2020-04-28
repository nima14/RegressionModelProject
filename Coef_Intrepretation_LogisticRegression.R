x <- c(5,6) # one increase in x

h <- -2 + 0.7*x

P <- 1/(1+exp(-h))

odds <- P/(1-P)

ChangeInOdds <- odds[2] / odds[1]

ExpCoef <- exp(0.7)

