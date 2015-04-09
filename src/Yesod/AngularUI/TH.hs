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

module Yesod.AngularUI.TH where

import           Yesod.AngularUI.Types as TS
import           Yesod.AngularUI.Router
import           Yesod.AngularUI.Common

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell,  execWriter )
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
import           Prelude   hiding (head, init, last, readFile, tail)
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Text.Shakespeare.I18N
import           Data.List
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8, decodeUtf8)



-- -- | create state
-- addStateTH :: Text -> Maybe Text ->  Q Exp
-- addStateTH state url =
--    [|addCtrlRawState
--        $(liftT state)
--        $(liftT (fromMaybe ("/" <> T.replace "." "/" state) url))
--        $(autoHamlet state "")
--        $(fromMaybe [| emptyFunction |] $ autoJulius state "")
--        $(listE $ catMaybes [autoLucius state "", autoCassius state ""])
--        |]
--   where
--     liftT t = do
--         p <- [|T.pack|]
--         return $ AppE p $ LitE $ StringL $ T.unpack t
--

url u = tell mempty {uisUrl  = Just u}
name n = tell mempty {uisName  = First (Just n)}

-- nameA :: Text -> UiState master
nameA n = tell mempty
  { uisName    = First (Just n)
  , uiTC       = mempty { tcTempl = TmplInl "<ui-view/>" }
  , uiAbstract = True
  }

addData d = tell mempty {uiData = [d]}

tcFile :: Text -> Q Exp
tcFile state =
   [|tell mempty { uisName = First (Just $(liftT state))
            , uiTC    = UiTC
                      (TmplExt $(autoHamlet state ""))
                      (CtrlExt $(fromMaybe [| emptyFunction |] $ autoJulius state ""))
                      $(listE $ catMaybes [autoLucius state "", autoCassius state ""])
            }|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

tcVFile :: Text -> Text -> Q Exp
tcVFile state view =
   [|tell mempty
      { uisName = First (Just $(liftT state))
      , uiV = [ ( $(liftT view)
                , UiTC
                    (TmplExt $(autoHamlet state view))
                    (CtrlExt $(fromMaybe [| emptyFunction |] $ autoJulius state view))
                    $(listE $ catMaybes [autoLucius state view, autoCassius state view])
                )
              ]
      }|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

--
--
-- addCtrlRawState :: ( Monad m
--               , MonadThrow m
--               , MonadBaseControl IO m
--               , MonadIO m
--               ) => Text                         -- ^ user-friendly name
--                 -> Text                         -- ^ route pattern
--                 -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
--                 -> JavascriptUrl (Route master) -- ^ controller
--                 -> [CssUrl (Route master)]
--                 -> GAngular master m ()
-- addCtrlRawState name'' route template controller xcss = do
--     let name' = T.filter isAlpha name''
--     name <- mappend (mappend name' "__") <$> lift newIdent
--     tell mempty
--         { awPartials    = Map.singleton name (nullAuth, template)
--         , combined      = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
--         , awStateName   = [name'']
--         , awStates      = [julius|.state("#{rawJS name''}", { url:"#{rawJS route}", controller:#{rawJS name}, templateUrl:"?partial=#{rawJS name}"})|]
--         , awControllers = [julius|var #{rawJS name} = ^{controller};|]
--         , awLook        = xcss
--         }

state
  :: ( Monad m
     , MonadThrow m
     , MonadBaseControl IO m
     , MonadIO m
     )
  => GUiState master () -> GAngular master m ()
state sa = do
    let a = execWriter sa
    tell mempty {awUiState = [a]}
    addUIState a

sanitName name = mappend (mappend (T.filter isAlpha name) "__") <$> lift newIdent

renderTemplate
  :: ( Monad m
     , MonadThrow m
     , MonadBaseControl IO m
     , MonadIO m
     )
  => StateTemplate master -> GAngular master m (Maybe (JavascriptUrl (Route master)))
renderTemplate = \case
  TmplExt t -> do
        name <- lift newIdent
        tell mempty  { awPartials = Map.singleton name (nullAuth, t)
                     , combined   = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{t} |]
                     }
        return $ Just [js|templateUrl:"?partial=#{rawJS name}"|]
  TmplInl t -> return $ Just [js|template:#{toJSON t}|]
  TmplProvider p -> return $ Just [js|templateProvider:^{p}|]
  TmplNone -> return Nothing

renderControler
  :: ( Monad m
     , MonadThrow m
     , MonadBaseControl IO m
     , MonadIO m
     )
  => StateCtrl master -> GAngular master m (Maybe (JavascriptUrl (Route master)))
renderControler = \case
  CtrlName     n -> return $ Just [js|controller:#{rawJS n}|]
  CtrlNameAs a n -> return $ Just [js|controller:#{rawJS n}, controllerAs: "#{rawJS a}"|]
  CtrlExt      j -> do
     name <- lift newIdent
     tell mempty { awControllers = [julius|var #{rawJS name} = ^{j};|]}
     return $ Just [js|controller:#{rawJS name}|]
  CtrlExtAs  a j -> do
     name <- lift newIdent
     tell mempty { awControllers = [julius|var #{rawJS name} = ^{j};|]}
     return $ Just [js|controller:#{rawJS name}, controllerAs: "#{rawJS a}"|]
  CtrlProvider  j -> do
     name <- lift newIdent
     tell mempty { awControllers = [julius|var #{rawJS name} = ^{j};|]}
     return $ Just [js|controllerProvider:#{rawJS name}|]
  CtrlNone -> return Nothing


concatJS c (catMaybes -> j:js) = j <> mconcat (map (c <>) js)
concatJS _ _ = mempty

addUIState
  :: ( Monad m
     , MonadThrow m
     , MonadBaseControl IO m
     , MonadIO m
     )
  => UiState master -> GAngular master m ()
addUIState UiState{..} = do
    let First (Just name'') = uisName
    let UiTC {..}           = uiTC
    let c = [js|,|]

    tplBit <- renderTemplate tcTempl
    ctlBit <- renderControler tcCtrl
    let absBit = if uiAbstract then Just [js|abstract: true|] else Nothing
    let urlBit = (\u -> [js|url:"#{rawJS u}"|]) <$> uisUrl
    let resBit = (\u -> [js|resolve:^{u}|]) <$> uiResolve
    let datBit = if null uiData then Nothing else Just [js|data:{^{concatJS c (map Just uiData)}}|]

    viwBit <- case uiV of
               [] -> return Nothing
               a  -> do
                 vs <- mapM (\(vi,ct) -> do
                     c1 <- renderControler (TS.tcCtrl ct)
                     t1 <- renderTemplate  (TS.tcTempl ct)
                     return $ Just [js| "#{rawJS vi}" : { ^{concatJS c [c1, t1]} } |]
                   ) a
                 return $ Just [js|views:{ ^{concatJS c vs} }|]

    let stateBody = concatJS c [absBit, urlBit, ctlBit, tplBit, resBit, datBit, viwBit]
    tell mempty
        { awStateName   = [name'']
        , awStates      = [julius|.state("#{rawJS name''}", { ^{stateBody} })|]
        , awLook        = tcCss <> concatMap (TS.tcCss . snd) uiV
        }

