base_did = data("base_did")
head(base_did)


est_did = feols(y ~ x1 + i(treat, period, 5) | id + period, base_did)
