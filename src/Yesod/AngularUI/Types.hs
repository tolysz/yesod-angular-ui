
{-# Language OverloadedStrings
  , QuasiQuotes
  , RecordWildCards
  , LambdaCase
  , TemplateHaskell
  , BangPatterns
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , ScopedTypeVariables
  , RankNTypes
  , TypeFamilies
  , ViewPatterns
  #-}

module Yesod.AngularUI.Types where
import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad.Trans.Writer (Writer, WriterT, runWriter, runWriterT, tell)
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



class (Yesod master) => YesodAngular master where
    urlAngularJs :: [master -> Either (Route master) Text]
    urlAngularJs  = []-- > add bower packages
    angularUIEntry :: WidgetT master IO ()
    angularUIEntry = [whamlet|<div data-ui-view>|]
    wrapAngularUI :: Text ->  WidgetT master IO ()
    wrapAngularUI modname = [whamlet|ng-app="#{modname}"|]


data AngularWriter master m  = AngularWriter
    { awCommands     :: Map Text ( HandlerT master m Bool,  HandlerT master m ())
    , awPartials     :: Map Text ( HandlerT master m Bool,  HtmlUrlI18n (SomeMessage master) (Route master))
    , awRoutes       :: JavascriptUrl (Route master)
    , awControllers  :: JavascriptUrl (Route master)
    , awServices     :: JavascriptUrl (Route master)
    , awDirectives   :: JavascriptUrl (Route master)
    , awConfigs      :: JavascriptUrl (Route master)
    , awSetup        :: JavascriptUrl (Route master)
    , awModules      :: [Text]
    , awDefaultRoute :: [Text]
    , awLook         :: [CssUrl (Route master)]
    , awStates       :: JavascriptUrl (Route master)
--    , awMenu         :: (Map Text UrlOrState)
   -- Template cache
    , combined       :: HtmlUrlI18n (SomeMessage master) (Route master)
    -- , bower packages
    , awBower        :: [Text]
    , awStateName    :: [Text]
    , awUiState      :: [UiState master]
    }

instance Monoid (AngularWriter master m) where
    mempty = AngularWriter mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
    (AngularWriter a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
        `mappend` (AngularWriter b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16)
        = AngularWriter
            (mappend a1 b1)
            (mappend a2 b2)
            (mappend a3 b3)
            (mappend a4 b4)
            (mappend a5 b5)
            (mappend a6 b6)
            (mappend a7 b7)
            (mappend a8 b8)
            (nub $ sort $ mappend a9 b9) -- modules
            (mappend a10 b10)
            (mappend a11 b11)
            (mappend a12 b12)
            (mappend a13 b13)
            (mappend a14 b14)
            (mappend a15 b15)
            (mappend a16 b16)

data UiState master = UiState
  { uisName     :: First Text
  , uisUrl      :: Maybe Text
  , uiTC        :: UiTC master
  , uiV         :: [(Text, UiTC master)]
  , uiAbstract  :: Bool
  , uiResolve   :: Maybe (JavascriptUrl (Route master))
  , uiData      :: [ JavascriptUrl (Route master) ]
  }


-- template   | templateUrl  | templateProvider
-- controller (fn, name,  name as name2 | controllerProvider
data StateTemplate master
   = TmplNone
   | TmplInl     Text
   | TmplExt     (HtmlUrlI18n (SomeMessage master) (Route master))
   | TmplProvider (JavascriptUrl (Route master))

instance Monoid (StateTemplate master) where
    mempty = TmplNone
    TmplNone `mappend` a = a
    a `mappend` _ = a

data StateCtrl master
   = CtrlNone
   | CtrlName        Text
   | CtrlNameAs Text Text
   | CtrlExt         (JavascriptUrl (Route master))
   | CtrlExtAs Text  (JavascriptUrl (Route master))
   | CtrlProvider    (JavascriptUrl (Route master))

instance Monoid (StateCtrl master) where
    mempty = CtrlNone
    CtrlNone `mappend` a = a
    a `mappend` _ = a

data UiTC master = UiTC
  { tcTempl    :: StateTemplate master
  , tcCtrl     :: StateCtrl master
  , tcCss      :: [CssUrl (Route master)]
  }

instance Monoid (UiState master) where
    mempty = UiState mempty mempty mempty mempty False mempty mempty
    (UiState a1 a2 a3 a4 a5 a6 a7) `mappend` (UiState b1 b2 b3 b4 b5 b6 b7) = UiState
       (mappend a1 b1)
       (mappend a2 b2)
       (mappend a3 b3)
       (mappend a4 b4)
       (a5 || b5)
       (mappend a6 b6)
       (mappend a7 b7)


instance Monoid (UiTC maste) where
    mempty = UiTC mempty mempty mempty
    (UiTC a0 a1 a2) `mappend` (UiTC b0 b1 b2) = UiTC
       (mappend a0 b0)
       (mappend a1 b1)
       (mappend a2 b2)

type GAngular master m = WriterT (AngularWriter master m) (HandlerT master m)
type GUiState master = Writer (UiState master)

-- renSoMsg :: (SomeMessage master) -> Text.Hamlet.Translate (SomeMessage master)
