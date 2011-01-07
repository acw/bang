module Data.List

data List a = Null | (:) a (List a)

(++) :: [a] -> [a] -> [a]
a ++ []      = a
[] ++ b      = b
(af:ar) ++ b = af:(ar ++ b)

null :: [a] -> Bool
null [] = True
null _  = False

length :: [a] -> Word
length [] = 0
length (a:rest) = 1 + length rest

reverse :: [a] -> [a]
reverse xs = helper xs []
 where
  helper [] acc = acc
  helper (a:rest) acc = helper rest (a:acc)


