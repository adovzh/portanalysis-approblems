source("q29.R")

cat(sprintf("Z is %.2f\n", rt$Z))
cat(sprintf("P-value for the test statistic is %.4f\n", rt$pval))
cat(sprintf("We %s reject the null hypothesis that the returns are serially independent\n",
            ifelse(rt$pval < .05, "can", "can't")))