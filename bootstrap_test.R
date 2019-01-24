# Bootstrap testing regarding n size

bootRisk(logR = dm$logRet, nBoot = 100)
v1 <- dp$risk["Port",c("VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile")]
quant_dist_n100 <- abs(dp$risk["Port","VaR bootstrap 05Quantile"]) - abs(dp$risk["Port","VaR bootstrap 95Quantile"])


bootRisk(logR = dm$logRet, nBoot = 1000)
v2 <- dp$risk["Port",c("VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile")]
quant_dist_n1000 <- abs(dp$risk["Port","VaR bootstrap 05Quantile"]) - abs(dp$risk["Port","VaR bootstrap 95Quantile"])

bootRisk(logR = dm$logRet, nBoot = 10000)
v3 <- dp$risk["Port",c("VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile")]
quant_dist_n10000 <- abs(dp$risk["Port","VaR bootstrap 05Quantile"]) - abs(dp$risk["Port","VaR bootstrap 95Quantile"])


## MSGARCH Filtered values

bootRisk(logR = dm$logRetFilMSGARCH1, nBoot = 100)
v1 <- dp$risk["Port",c("VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile")]
quant_dist_n100 <- abs(dp$risk["Port","VaR bootstrap 05Quantile"]) - abs(dp$risk["Port","VaR bootstrap 95Quantile"])


bootRisk(logR = dm$logRetFilMSGARCH1, nBoot = 1000)
v2 <- dp$risk["Port",c("VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile")]
quant_dist_n1000 <- abs(dp$risk["Port","VaR bootstrap 05Quantile"]) - abs(dp$risk["Port","VaR bootstrap 95Quantile"])

bootRisk(logR = dm$logRetFilMSGARCH1, nBoot = 10000)
v3 <- dp$risk["Port",c("VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile")]
quant_dist_n10000 <- abs(dp$risk["Port","VaR bootstrap 05Quantile"]) - abs(dp$risk["Port","VaR bootstrap 95Quantile"])



