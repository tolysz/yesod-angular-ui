
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# Language LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# Language ViewPatterns      #-}
{-# Language BangPatterns      #-}
{-# Language CPP      #-}

module Yesod.AngularUI.Router where

import Data.Text (Text)
import Text.Hamlet
import Text.Julius
import Text.Lucius
import Text.Cassius
import Text.Naked.Coffee
import Text.TypeScript

import Language.Haskell.TH.Lib (ExpQ)
import qualified Data.Text as T
import Data.Either
import Prelude
import Control.Monad

import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

import Data.Maybe

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

whenRight :: (Functor f, Monad f) => Either t t1 -> (t1 -> f a) -> f ()
whenRight (Right a) b = void $ b a
whenRight _ _ = return ()

fnCreate :: Text -> Text -> Text -> String
fnCreate (T.null -> True)  (T.replace "@" "AT" -> nam) suffix = T.unpack $ T.concat ["angular/", nam , ".", suffix]
fnCreate parent            (T.unpack -> "")            suffix = T.unpack $ T.concat ["angular/", T.replace "." "/" parent, ".", suffix]
fnCreate parent            (T.replace "@" "AT" -> nam) suffix = T.unpack $ T.concat ["angular/", T.replace "." "/" parent, ".", nam, ".", suffix]


autoMaybe :: (String -> ExpQ) -> Text -> Text -> Text -> Maybe ExpQ
autoMaybe ex ext state view =
    let fn = fnCreate state view ext
    in
    if unsafePerformIO $ doesFileExist fn
       then Just [| $(ex fn) |]
       else Nothing
{-# NOINLINE autoMaybe #-}

autoHamlet :: Text -> Text -> ExpQ
autoHamlet state view = do
  let fn = fnCreate state view "hamlet"
#if DEVELOPMENT
  let !boo = unsafePerformIO $ print fn
#endif
  if unsafePerformIO $ doesFileExist fn
    then  [| $(ihamletFile fn) |]
    else  [| [ihamlet| <!-- #{fn} --> |] |]
{-# NOINLINE autoHamlet #-}

autoJulius :: Text -> Text -> Maybe ExpQ
autoJulius = autoMaybe juliusFile "julius"

autoCassius :: Text -> Text -> Maybe  ExpQ
autoCassius = autoMaybe cassiusFile "cassius"

autoLucius :: Text -> Text -> Maybe ExpQ
autoLucius = autoMaybe luciusFile "lucius"

autoCoffee :: Text -> Text -> Maybe ExpQ
autoCoffee = autoMaybe ncoffeeFile "coffee"

autoTypeScript :: Text -> Text -> Maybe ExpQ
autoTypeScript = autoMaybe typeScriptFile "tsc"
