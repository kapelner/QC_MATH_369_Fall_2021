pacman::p_load(ggplot2)

x = 0:20
probs = dbinom(x, 20, 0.524)
ggplot(data.frame(y = probs, x = x)) + 
  geom_bar(aes(x = x, y = probs), stat = 'identity') +
  ylab("binomial probability")

ret_prob = sum(probs[7:16])
ret_prob
1 - ret_prob

ret_prob = sum(probs[6:15])
ret_prob
1 - ret_prob