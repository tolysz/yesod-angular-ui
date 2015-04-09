module Yesod.AngularUI.Common where

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                 (fromMaybe, catMaybes)
import           Data.Monoid                (First (..), Monoid (..), (<>))
import           Data.Text                  (Text)
import           Text.Hamlet
import           Text.Blaze.Html
import           Text.Julius
import           Text.Lucius
import           Yesod.Core                 (Route, Yesod,
                                             addScriptEither,
                                             getUrlRenderParams, getMessageRender,  getYesod, lift,
                                             lookupGetParam, newIdent,
                                             sendResponse, notFound,
                                             toWidget, whamlet, toWidgetBody)
import           Yesod.Core.Widget
import qualified Text.Blaze.Html5 as H
import           Text.Jasmine (minify)
import           Yesod.Core.Types
import           Yesod.Core.Content
import           Yesod.Core.Json
import           Language.Haskell.TH.Syntax (Q, Exp (..), Lit (..))
import           Language.Haskell.TH (listE)
import qualified Data.Text as T
import           Data.Char (isAlpha)

import           Control.Monad (when, unless, (>=>))
import           Data.Either
import           Prelude   hiding (head, init, last, readFile, tail, writeFile)
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Text.Shakespeare.I18N
import           Data.List
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8, decodeUtf8)

-- import           Yesod.AngularUI.TH
-- import           Yesod.AngularUI.Router
-- import           Yesod.AngularUI.Types


emptyFunction = [julius| function(){} |]
nullAuth :: Monad m => m Bool
nullAuth = return True


