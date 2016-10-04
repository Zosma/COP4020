module RegularExpressions where

-- regular expression

infixl 8 <|>
infixl 9 <.>

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "") -- operator section

char :: Char -> RegExp
char c = (== [c])

--(<|>) :: String -> Bool -> String -> Bool -> String -> Bool
(<|>) :: RegExp -> RegExp -> RegExp
(<|>) e1 e2 = \s -> e1 s || e2 s 

splits :: [a] -> [([a],[a])]
splits xs = map (flip splitAt xs) [0..length xs]

-- concatenation .
(<.>) :: RegExp -> RegExp -> RegExp
(<.>) e1 e2 =
  \s -> or [e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<..>) :: RegExp -> RegExp -> RegExp
(<..>) e1 e2 =
  \s -> or [e1 prefix && e2 suffix | (prefix,suffix) <- drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon <|> (e <..> star e)

-- extra functions

letter :: RegExp               -- if you put $ then you don't have
                               -- to put parenthesis around ['a' ... 'Z']
letter = foldl1 (<|>) (map char $ ['a'..'z'] ++ ['A'..'Z'])

a = char 'a'
b = char 'b'

-- put your solutions here

-- option

option :: RegExp -> RegExp
option e = epsilon <|> e

-- plus

plus :: RegExp -> RegExp
plus e = e <|> (e <.> plus e) 

-- number

number :: RegExp
number [] = False
number [x] = foldl1 (<|>) (map char $ ['0'..'9']) [x]
number xs = if length xs > 1 && head xs == '0' then False else foldl1 (<|>) (map char $ ['0'..'9']) [(head xs)] && number (filter (/= '0') (tail xs))

-- fractional number

fractional :: RegExp
fractional [] = False
fractional "0.0" = True
fractional xs = if (length xs > 3 && ((head xs == '0' && head (tail xs) /= '.') ||
                   (last xs == '0' && last (init xs) /= '.'))) || nDots /= 1
     then False
     else number filtered
     where
          nDots = length (filter (== '.') xs)
          filtered = filter (\x -> x /= '0' && x /= '.') xs
--foldl1 (<|>) (map char $ ['0'..'9']++['.']) [head xs] && fractional filTail







