module strings where

normaliseSpace :: String -> String
normaliseSpace [] = []
normaliseSpace [' '] = [' ']
normaliseSpace (x:xs) | (x == ' ' && head(xs) == ' ') = normaliseSpace(xs)
                      | otherwise = x : normaliseSpace(xs)

normaliseFront :: String -> String
normaliseFront [] = []
normaliseFront [' '] = []
normaliseFront (x:xs) | (x == ' ' && head(xs) /= ' ') = (xs)
                      | (x == ' ' && head(xs) == ' ') = normaliseFront(tail(xs))
                      | otherwise = (x:xs)

-- @assumption: string is of finite length
normaliseBack :: String -> String
normaliseBack [] = []
normaliseBack [' '] = []
normaliseBack (x:xs) = reverse(normaliseFront(reverse(x:xs)))

normalise :: String -> String
normalise [] = []
normalise (x:xs) = normaliseSpace(normaliseBack(normaliseFront(x:xs)))

prefix :: String -> String -> Bool
prefix[]_ = True
prefix _[] = False
prefix(x:xs)(y:ys) = x == y && prefix(xs)(ys)

substr :: String -> String -> Bool
substr []_ = True
substr _[] = False
substr(xs)(ys) | length(xs) > length(ys) = False
               | prefix (xs)(ys) = True
               | substr(xs)(tail ys) = True
               | otherwise = False

-- @assumption: both arguments finite
postfix :: String -> String -> Bool
postfix(xs)(ys) = prefix(reverse xs)(reverse ys)

-- substitution after first match. e.g. substitude "aa" "b" "aaa" = "ba"
substitute :: String -> String -> String -> String
substitute [] _ _ = undefined
substitute _ _ [] = []
substitute(xs)(ys)(z:zs) | prefix (xs) (z:zs) = ys ++ substitute(xs)(ys)(drop(length(xs))(z:zs))
                         | otherwise = z : substitute (xs)(ys)(zs)
