import Control.Applicative

f x = lookup x [(3, "hello"), (4, "riz"), (5, "hehe")]
g y = lookup y [(11, "eleven"), (12, "twelve"), (13, "thirteen")]

h z = lookup z [(2, 3), (5, 6), (7,8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]
