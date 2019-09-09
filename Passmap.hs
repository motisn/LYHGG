import qualified Data.Map as Map

--module Mytype
--(
--
--) where

data RegistState = Unsigned | ID String deriving (Show, Eq)

type Service = String
type Password = String

type PassMap = Map.Map Service (RegistState, Password)

passShow :: Service -> PassMap -> Either String Password
passShow serviceName map = case Map.lookup serviceName map of
    Nothing -> Left $ "Servise " ++ serviceName ++ " doesn't exist!"
    Just (state, pass) -> if state == Unsigned
                            then Left $ "Survice " ++ serviceName ++ " is " ++ show Unsigned ++ " !"
                            else Right $ "Survice " ++ serviceName ++ " is signed as " ++ show state ++ ", the pass is " ++ pass ++ " !"

mypass :: PassMap
mypass = Map.fromList [("Twitter", (ID "motisn", "pass1"))
                      ,("Facebook", (Unsigned, ""))
                      ,("mstdn.jp", (ID "motisn", "pass2"))
                      ]