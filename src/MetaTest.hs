{-# LANGUAGE RankNTypes, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module MetaTest where
import Prelude hiding ((<=))


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



class Compare a where
   (==) :: a -> Double -> String
   (<=) :: a -> Double -> String
   (~=) :: a -> Double -> String

instance Compare String where
   a' <= b' = a' ++ " <= " ++ show b'
   a' == b' = a' ++ " == " ++ show b'
   a' ~= b' = a' ++ " = " ++ show b' ++ ";"


workers :: String
workers =  "workers"

mined :: [Char]
mined = "mined"


h :: String
h = eval [ If (workers <= 16) (mined ~= 20)
         , Else (mined ~= 34)
         ]
        

        
ifBlock :: If -> String
ifBlock (If p t) = "if (" ++ p ++ ")" ++ " { " ++ t ++ " } "
ifBlock (ElseIf p t) = "else if (" ++ p ++ ")" ++ " { " ++ t ++ " } "
ifBlock (Else t) = "else { " ++ t ++ " } "


eval :: [If] -> String
eval ifs = foldr (++) [] (map ifBlock ifs)


