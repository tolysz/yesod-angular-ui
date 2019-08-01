
{-# Language OverloadedStrings
  , QuasiQuotes
  , LambdaCase
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , ScopedTypeVariables
  , RankNTypes
  , TypeFamilies
  #-}

module Yesod.AngularUI.Types where
import           Control.Monad.Trans.Writer (Writer, WriterT)
import           Data.Map.Strict                   (Map)
import           Data.Monoid                (First (..), Monoid (..))
import           Data.Text                  (Text)
import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Yesod.Core                 (Route, Yesod, HandlerFor)
import           Yesod.Core.Widget
import           Data.Either
import           Prelude   hiding (head, init, last, readFile, tail, writeFile)
import           Text.Shakespeare.I18N
import           Data.List

class (Yesod master) => YesodAngular master where
    urlAngularJs :: [master -> Either (Route master) Text]
    urlAngularJs  = []-- > add bower packages
    angularUIEntry :: WidgetFor master ()
    angularUIEntry = [whamlet|<div data-ui-view>|]
    wrapAngularUI :: Text ->  WidgetFor master ()
    wrapAngularUI modname = [whamlet|ng-app="#{modname}"|]


data AngularWriter master m  = AngularWriter
    { awCommands       :: Map Text (HandlerFor master ())
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
   -- Template cache
    , combined       :: HtmlUrlI18n (SomeMessage master) (Route master)
    -- , bower packages
    , awBower        :: [Text]
    , awStateName    :: [Text]
    , awUiState      :: [UiState master]
    }

instance Monoid (AngularWriter master m) where
    mempty = AngularWriter mempty  mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
instance Semigroup (AngularWriter master m) where
    (AngularWriter a1 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
        <> (AngularWriter b1 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16)
        = AngularWriter
            (mappend a1 b1)
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

data StateTemplate master
   = TmplNone
   | TmplInl     Text
   | TmplExt     (HtmlUrlI18n (SomeMessage master) (Route master))
   | TmplProvider (JavascriptUrl (Route master))

instance Monoid (StateTemplate master) where
    mempty = TmplNone
instance Semigroup (StateTemplate master) where
    TmplNone <> a = a
    a <> _ = a

data StateCtrl master
   = CtrlNone
   | CtrlName        Text
   | CtrlNameAs Text Text
   | CtrlExt         (JavascriptUrl (Route master))
   | CtrlExtAs Text  (JavascriptUrl (Route master))
   | CtrlProvider    (JavascriptUrl (Route master))

instance Monoid (StateCtrl master) where
    mempty = CtrlNone
instance Semigroup (StateCtrl master) where
    CtrlNone <> a = a
    a <> _ = a

data UiTC master = UiTC
  { tcTempl    :: StateTemplate master
  , tcCtrl     :: StateCtrl master
  , tcCss      :: [CssUrl (Route master)]
  }

instance Monoid (UiState master) where
    mempty = UiState mempty mempty mempty mempty False mempty mempty
instance Semigroup (UiState master) where
    (UiState a1 a2 a3 a4 a5 a6 a7) <> (UiState b1 b2 b3 b4 b5 b6 b7) = UiState
       (mappend a1 b1)
       (mappend a2 b2)
       (mappend a3 b3)
       (mappend a4 b4)
       (a5 || b5)
       (mappend a6 b6)
       (mappend a7 b7)

instance Monoid (UiTC maste) where
    mempty = UiTC mempty mempty mempty
instance Semigroup (UiTC maste) where
    (UiTC a0 a1 a2) <> (UiTC b0 b1 b2) = UiTC
       (mappend a0 b0)
       (mappend a1 b1)
       (mappend a2 b2)

type GAngularT  master a = WriterT (AngularWriter master                                      a) (HandlerFor master ) a
type GAngularR  master   = WriterT (AngularWriter master (Maybe (JavascriptUrl (Route master)))) (HandlerFor master ) (Maybe (JavascriptUrl (Route master)))
type GAngular   master   = WriterT (AngularWriter master (Maybe (JavascriptUrl (Route master)))) (HandlerFor master ) ()
type GUiState   master   = Writer (UiState master)
