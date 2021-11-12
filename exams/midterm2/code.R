pacman::p_load(extraDistr)
set.seed(1984)

n = 15
alpha1 = 1.1
alpha2 = 1.2
x1 = rlomax(n, 1, alpha1)
x2 = rlomax(n, 1, alpha2)

paste(round(x1, 2), collapse =", ")
paste(round(x2, 2), collapse =", ")

xbar1 = round(mean(x1), 2)
xbar2 = round(mean(x2), 2)
s1 = round(sd(x1), 2)
s2 = round(sd(x2), 2)

xbar1
xbar2
s1
s2

#2, 3, 4
s_sq_pooled = (n - 1) / (2 * n - 2) * s1^2 +(n - 1) / (2 * n - 2) * s2^2
s_pooled = round(sqrt(s_sq_pooled), 2)
s_pooled


(xbar1 - xbar2) / (sqrt(s1^2 / n + s2^2 / n))       # -0.4996635
(xbar1 - xbar2) / (s_pooled * sqrt(1 / n + 1 / n))  # -0.4996494
(xbar1 - xbar2) / (s_pooled * sqrt(1 / n))          # -0.7066109
(xbar1 - xbar2) / (s_pooled)                        # -0.1824462

#5
(1 + xbar2) / xbar2 # 1.095238

#8
thetahathatmle1 = n / sum(log(1 + x1))
thetahathatmle2 = n / sum(log(1 + x2))
thetahathatmle1 = round(thetahathatmle1, 3)
thetahathatmle2 = round(thetahathatmle2, 3)
thetahathatmle1
thetahathatmle2

I_thetahathatmle1inv = thetahathatmle1^2 
I_thetahathatmle2inv = thetahathatmle2^2 

var_thetahathatmle1 = I_thetahathatmle1inv / n
var_thetahathatmle2 = I_thetahathatmle2inv / n

sqrt(var_thetahathatmle1 + var_thetahathatmle2)

(thetahathatmle1 - thetahathatmle2) / sqrt(var_thetahathatmle1 + var_thetahathatmle2)^2
# -0.01509039
(thetahathatmle1 - thetahathatmle2) / sqrt(var_thetahathatmle1 + var_thetahathatmle2)
# -0.005493704
(thetahathatmle1 - thetahathatmle2) / sqrt(s1^2/n + s2^2/n)
# -0.0002225672

#9
ci_95a = thetahathatmle1 - 1.96 * sqrt(var_thetahathatmle1)
ci_95b = thetahathatmle1 + 1.96 * sqrt(var_thetahathatmle1)
ci_95a # 0.4919545
ci_95b # 1.500046


ci_95a = (thetahathatmle1 - thetahathatmle2) - 1.96 * sqrt(var_thetahathatmle1 + var_thetahathatmle2)
ci_95b = (thetahathatmle1 - thetahathatmle2) + 1.96 * sqrt(var_thetahathatmle1 + var_thetahathatmle2)
ci_95a # -0.7155441
ci_95b #  0.7115441

ci_95a = (thetahathatmle1 - thetahathatmle2) - 1.96 * sqrt(var_thetahathatmle1 + var_thetahathatmle2)^2
ci_95b = (thetahathatmle1 - thetahathatmle2) + 1.96 * sqrt(var_thetahathatmle1 + var_thetahathatmle2)^2
ci_95a # -0.2617679
ci_95b #  0.2577679

#10
theta_0 = 1
(thetahathatmle1^2 - theta_0^2) / (2 * theta_0 * sqrt(theta_0^2 / n))
# -0.01546095
(thetahathatmle1^2 - theta_0^2) / (2 * thetahathatmle1 * sqrt(thetahathatmle1^2 / n))
# -0.01558538