{-# LANGUAGE RankNTypes #-}
module MetaTest where



func :: String -> [String] -> String -> String
func name args body = "function " ++ name ++ (toArgs args) ++ "{" ++ body ++ "}"

ifFun :: String -> String -> String
ifFun condition todo = 
        "if (" ++ condition ++ ") {" 
               ++ todo 
               ++ "return true;"
               ++ "} return false;"


tryAttachRefinery :: String
tryAttachRefinery = func "tryAttach" ["command"] (ifFun "this.attachedRefineryNum < 2" "this.attachedRefineryNum++;")


toArgs' :: [String] -> String
toArgs' (x:xs) | length xs /= 0 = x ++ ", " ++ toArgs' xs
               | otherwise      = x
toArgs' _      = []

toArgs :: [String] -> String
toArgs args = "(" ++ toArgs' args ++ ")"


a :: String
a = func "mineralsMined" ["workers"] "return 0;"


data If = If { predicate :: String, todo :: String }
        | ElseIf { predicate :: String, todo :: String }
        | Else { todo :: String } deriving Show


lessThan' :: String -> String -> String
lessThan' s1 s2 = s1 ++ " < " ++ s2

lessThan'' :: forall a a1. (Show a1, Show a) => a -> a1 -> [Char]
lessThan'' s' i = (show s') ++ " < " ++ (show i)

--lt :: forall a a1. (Show a1, Show a) => a -> a1 -> [Char]
--lt s' i = (show s') ++ " < " ++ (show i)
--lessThan'' = `lessThan'`

data JSTypes = JString { jstring :: String }
             | JNumber { jnumber :: Double } deriving Show

class Eq' a where
   (==) :: a -> a -> String
   (<=) :: a -> a -> String

instance Eq' JSTypes where
   a' <= b' = show a' ++ " <= " ++ show b'
   a' == b' = show a' ++ " == " ++ show b'

workers = "workers"
lessThan = "<"

workers' = JString "workers"

s :: Int -> String
s i = show i

js :: String -> String -> String -> String
js s1 s2 s3 = s1 ++ s2 ++ s3


b = iff [ If "workers <= 16" "mined = 20;"
        , Else "mined = 34;"
        ]

c = iff [ If (workers ++ lessThan ++ (s 16)) "mined = 20;"
        , Else "mined = 34;"
        ]

d = iff [ If (js workers lessThan $ s 16) "mined = 20;"
        , Else "mined = 34;"
        ]
        
e = iff [ If (workers `lessThan'` (s 16)) "mined = 20;"
        , Else "mined = 34;"
        ]

f = iff [ If (workers `lessThan''` 16) "mined = 20;"
        , Else "mined = 34;"
        ]
        

g = iff' (If (workers `lessThan''` 16) "mined = 20;")
         (Else "mined = 34;")

h = iff' (If (workers `lessThan''` 16) "mined = 20;")
         (Else "mined = 34;")        
        
ifBlock' :: If -> String
ifBlock' (If p t) = "if (" ++ p ++ ")" ++ " { " ++ t ++ " } "
ifBlock' (ElseIf p t) = "else if (" ++ p ++ ")" ++ " { " ++ t ++ " } "
ifBlock' (Else t) = "else { " ++ t ++ " } "


iff :: [If] -> String
iff ifs = foldr (++) [] (map ifBlock' ifs)

iff' :: If -> If -> String
iff' if1 if2 = ifBlock' if1 ++ ifBlock' if2

