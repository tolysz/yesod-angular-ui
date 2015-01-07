{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for CoffeeScript, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript with
-- the system tool @coffee@.
--
-- To use this module, @coffee@ must be installed on your system.
--
-- @#{...}@ is the Shakespearean standard for variable interpolation, but
-- CoffeeScript already uses that sequence for string interpolation. Therefore,
-- Shakespearean interpolation is introduced with @%{...}@.
--
-- If you interpolate variables,
-- the template is first wrapped with a function containing javascript variables representing shakespeare variables,
-- then compiled with @coffee@,
-- and then the value of the variables are applied to the function.
-- This means that in production the template can be compiled
-- once at compile time and there will be no dependency in your production
-- system on @coffee@. 
--
-- Your code:
--
-- >   b = 1
-- >   console.log(#{a} + b)
--
-- Function wrapper added to your coffeescript code:
--
-- > ((shakespeare_var_a) =>
-- >   b = 1
-- >   console.log(shakespeare_var_a + b)
-- > )
--
-- This is then compiled down to javascript, and the variables are applied:
--
-- > ;(function(shakespeare_var_a){
-- >   var b = 1;
-- >   console.log(shakespeare_var_a + b);
-- > })(#{a});
--
--
-- Further reading:
--
-- 1. Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
-- 2. CoffeeScript: <http://coffeescript.org/>
module Text.Naked.Coffee
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      ncoffee
    , ncoffeeFile
    , ncoffeeFileReload
    , ncoffeeFileDebug

#ifdef TEST_EXPORT
    , coffeeSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius
import Prelude
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)


-- just kill the last semipcolon
coffeeSettings :: Q ShakespeareSettings
coffeeSettings = do
  jsettings <- javascriptSettings
  drp <- [| \ex url -> Javascript . fromLazyText . {- T.drop 1 . T.init .-} T.init . T.init . renderJavascript $ ex url |]
  return $ jsettings { varChar = '%'
  , modifyFinalValue = Just drp
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "coffee" ["-spb"]
    , preEscapeIgnoreBalanced = "'\"`"     -- don't insert backtacks for variable already inside strings or backticks.
    , preEscapeIgnoreLine = "#"            -- ignore commented lines
    , wrapInsertion = Just WrapInsertion { 
        wrapInsertionIndent = Just "  "
      , wrapInsertionStartBegin = "("
      , wrapInsertionSeparator = ", "
      , wrapInsertionStartClose = ") =>"
      , wrapInsertionEnd = ""
      , wrapInsertionAddParens = False
      }
    }
  }

-- | Read inline, quasiquoted CoffeeScript.
ncoffee :: QuasiQuoter
ncoffee = QuasiQuoter { quoteExp = \s -> do
    rs <- coffeeSettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a CoffeeScript template file. This function reads the file once, at
-- compile time.
ncoffeeFile :: FilePath -> Q Exp
ncoffeeFile fp = do
    rs <- coffeeSettings
    shakespeareFile rs fp

-- | Read in a CoffeeScript template file. This impure function uses
-- unsafePerformIO to re-read the file on every call, allowing for rapid
-- iteration.
ncoffeeFileReload :: FilePath -> Q Exp
ncoffeeFileReload fp = do
    rs <- coffeeSettings
    shakespeareFileReload rs fp

-- | Deprecated synonym for 'coffeeFileReload'
ncoffeeFileDebug :: FilePath -> Q Exp
ncoffeeFileDebug = ncoffeeFileReload
{-# DEPRECATED ncoffeeFileDebug "Please use coffeeFileReload instead." #-}
