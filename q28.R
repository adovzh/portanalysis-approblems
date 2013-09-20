source("q27.R")

ar <- length(psp)
np <- sum(spsp[spsp > 0])
nn <- sum(-spsp[spsp < 0])
nt <- sum(psp)

er <- 2 * np * nn / nt + 1
sigmar <- sqrt(2 * np * nn * (2 * np * nn - nt) / (nt^2 * (nt - 1)))
Z <- (ar - er) / sigmar
pval <- 2 * (1 - pnorm(abs(Z)))
cat(sprintf("Z is %.2f\n", Z))
cat(sprintf("P-value for the test statistic is %.4f\n", pval))
cat(sprintf("We %s reject the null hypothesis that the returns are serially independent\n",
            ifelse(pval < .05, "can", "can't")))