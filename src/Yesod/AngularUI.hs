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

module Yesod.AngularUI
    ( YesodAngular (..)
    , runAngularUI
    , addCommand
    , addCommandMaybe
      -- ^ mostly internal
    , addModules
    , addDirective
    , addConfig
    , addConfigRaw
    , addService

    , addFactoryStore
   -- ^ naive persistent store
    , addFactory
    , addController
    , addProvide
    , addFilter
    , addConstant
    , addValue
   -- some normal stuff
    , addSetup
   -- ^ before the code, maybe some imports?

    , addWhen
    , setDefaultRoute
    , addREST
    , addRESTRaw
   -- placeholders for some generic rest api/ maybe a subsite?
    , tcFile
    , utcFile
    , tcVFile
    , utcVFile
    , state
    , url
    , name
    , nameA
    , addData
    , GAngular
    ) where


--- the chaos
import           Control.Applicative        ((<$>))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell, execWriter)
-- import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                 (fromMaybe, catMaybes)
import           Data.Monoid                (First (..), Monoid (..), (<>))
import           Data.Text                  (Text)
import           Text.Hamlet
import           Text.Blaze.Html
import           Text.Julius
import           Text.Lucius
import           Yesod.Core                 (Route,
                                             getUrlRenderParams, getMessageRender,  getYesod, lift,
                                             lookupGetParam, newIdent,
                                             sendResponse, notFound)
import           Yesod.Core.Widget
import qualified Text.Blaze.Html5 as H
import           Text.Jasmine (minify)
import           Yesod.Core.Types
import           Yesod.Core.Json
import           Language.Haskell.TH.Syntax (Q, Exp (..), Lit (..))
import           Language.Haskell.TH (listE)
import qualified Data.Text as T

import           Prelude   hiding (head, init, last, readFile, tail, writeFile)
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Text.Shakespeare.I18N
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8, decodeUtf8)

import           Yesod.AngularUI.Router
import           Yesod.AngularUI.Types as TS


renSoMsg :: (SomeMessage master  -> Text) -> SomeMessage master -> Html
renSoMsg f = toHtml . f

runAngularUI :: (YesodAngular master)
           => GAngular master IO ()                                     -- ^ angular app
           -> (Text -> WidgetT master IO () -> HandlerT master IO Html) -- ^ layout
           -> HandlerT master IO Html
runAngularUI ga dl = do
    master <- getYesod
    mrender <- renSoMsg <$> getMessageRender
    urender <- getUrlRenderParams

    ((), AngularWriter{..}) <- runWriterT ga

    mc <- lookupGetParam "command"
    fromMaybe (return ()) $ mc >>= flip Map.lookup awCommands
    modname <- newIdent
    let defaultRoute =
            case (awDefaultRoute, awStateName) of
                (filter (`elem` awStateName) -> x:_, _)  -> [julius|.otherwise("/#{rawJS x}")|]
                (_, x:_) ->             [julius|.otherwise("/#{rawJS x}")|]
                (_,[])   -> mempty
    dl modname $ do
        mapM_ (\x -> addScriptEither $ x master) urlAngularJs
        angularUIEntry
        toWidgetHead $ Minify awLook
        toWidget (combined mrender urender)
        toWidgetBody $ Minify [julius|
^{awSetup}

angular
    .module("#{rawJS modname}", #{rawJS $ show awModules }, function($provide) {
    // $provide.constant("menu",# {toJSON awMenu});
    ^{awServices}
    })
    ^{awDirectives}
    ^{awConfigs}
    .config(function($urlRouterProvider, $stateProvider) {
        $urlRouterProvider ^{awRoutes} ^{defaultRoute} ;
        $stateProvider ^{awStates};
    });
   ^{awControllers}
|]

newtype Minify a = Minify a

instance render ~ RY site => ToWidgetHead site (Minify [CssUrl (Route site)]) where
    toWidgetHead (Minify j) = toWidgetHead $ \r -> H.style $ preEscapedLazyText $  mconcat $ map (renderCssUrl r) j

instance render ~ RY site => ToWidgetBody site (Minify (render -> Javascript)) where
    toWidgetBody (Minify j) = toWidget $ \r -> H.script $ preEscapedLazyText $ E.decodeUtf8 $ minify $ E.encodeUtf8 $ renderJavascriptUrl r j

addModules :: (Monad m) => [Text] -> GAngular master m ()
addModules x = tell mempty{ awModules = x } 

addConfig :: (Monad m) => Text
                       -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                       -> GAngular master m ()
addConfig name' funcall = do
    let n = name' `mappend` "Provider"
    tell mempty
        { awConfigs = [julius|.config(["#{rawJS n}", function (#{rawJS n}){
        #{rawJS n}.^{funcall}; }])|]
        }

addConfigRaw :: (Monad m) => ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                          -> GAngular master m ()
addConfigRaw funcall = tell mempty { awConfigs = [julius|.config(^{funcall})|] }

addDirective :: (Monad m) => Text
                          -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                          -> GAngular master m ()
addDirective n funcall =
    tell mempty
        { awDirectives = [julius|.directive("#{rawJS n}", ^{funcall} )|]
        }

addController :: (Monad m) => Text
                           -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                           -> GAngular master m ()
addController n funcall =
    tell mempty
        { awDirectives = [julius|.controller("#{rawJS n}", ^{funcall} )|]
        }


addFilter :: (Monad m) => Text
                       -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                       -> GAngular master m ()
addFilter n funcall =
    tell mempty
        { awDirectives = [julius|.filter("#{rawJS n}", ^{funcall} )|]
        }


addFactory :: (Monad m) => Text
                        -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                        -> GAngular master m ()
addFactory n funcall = addProvide [js|factory("#{rawJS n}",^{funcall})|]

addREST :: (Monad m) => Text
                      -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                      -> GAngular master m ()
addREST n funcall =
    tell mempty
        { awModules = ["ngResource"]
        , awDirectives = [julius|
         .factory("#{rawJS n}",["$resource", function($resource){
          return $resource(^{funcall}); }])|]
        }

addRESTRaw :: (Monad m) => Text
                        -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                        -> GAngular master m ()
addRESTRaw n funcall =
    tell mempty
        { awModules = ["ngResource"]
        , awDirectives = [julius|
         .factory("#{rawJS n}",["$resource", function($resource){
      ^{funcall}
          }])|]
        }

addService :: (Monad m) =>
                      Text
                      -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                      -> GAngular master m ()
-- ^ adds a service
addService n funcall = addProvide [js|service("#{rawJS n}",^{funcall})|]

addProvide :: (Monad m) => ((Route master -> [(Text, Text)] -> Text) -> Javascript) -> GAngular master m ()
addProvide funcall =
    tell mempty
        { awServices = [julius|
        $provide.^{funcall} ;|]
        }

addConstant :: (Monad m) => Text -> ((Route master -> [(Text, Text)] -> Text) -> Javascript) -> GAngular master m ()
-- ^ add constant, remember to quote if raw text
addConstant n funcall = addProvide [julius|constant("#{rawJS n}",^{funcall})|]


addValue :: (Monad m) => Text -> ((Route master -> [(Text, Text)] -> Text) -> Javascript) -> GAngular master m ()
-- ^ add constant, remember to quote if raw text
addValue n funcall = addProvide [julius|value("#{rawJS n}",^{funcall})|]

addSetup :: (Monad m) => ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                      -> GAngular master m ()
-- ^ inject this code befor calling angular
addSetup funcall =
    tell mempty
        { awSetup = [julius|^{funcall};|]
        }

addCommand :: (FromJSON input, ToJSON output)
           => (input -> HandlerT master IO output)
           -> GAngular master IO Text
-- ^ add a command (which is always printed)
addCommand f = addCommandMaybe (fmap (Just <$>) f)

addCommandMaybe :: (FromJSON input, ToJSON output)
           => (input -> HandlerT master IO (Maybe output))
           -> GAngular master IO Text
addCommandMaybe f = do
    n <- lift newIdent
    tell (mempty :: AngularWriter master IO) { awCommands = Map.singleton n handler }
    return $ "?command=" `mappend` n
  where
    handler = requireJsonBody >>= f >>= \case
         Just output -> do repjson <- returnJson output
                           sendResponse repjson
         Nothing -> notFound

addWhen :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text -- ^ one route
                -> Text -- ^ the other one
                -> GAngular master m ()
addWhen fro to  = tell mempty {awRoutes = [julius|.when("#{rawJS fro}","#{rawJS to}")|] }

setDefaultRoute :: (Monad m) => Text -> GAngular master m()
setDefaultRoute x = tell mempty { awDefaultRoute = [x] }

addFactoryStore :: (Monad m) => Text -> GAngular master m ()
addFactoryStore n = addFactory (n <> "Store") [julius| function(){
      var lc = {};
      return { update: function (s){ _.extend(lc,s)}
             , full: function (s){ return lc; }
             , set: function (e,v) { lc[e] = v; }
             , get: function (e){ return lc[e]; }
             , has: function (e){ return _.has(lc,e); }
             , getD : function (d, e){ return _.has(lc,e) ? lc[e] : d; }
             , isEmpty : function (){ return _.isEmpty(lc) }
             , clear : function(){lc = {}}
             }
     }
    |]

url :: Monad m => Text -> WriterT (UiState master) m ()
url u = tell mempty {uisUrl  = Just u}

name :: Monad m => Text -> WriterT (UiState master) m ()
name n = tell mempty {uisName  = First (Just n)}

nameA :: Monad m => Text -> WriterT (UiState master) m ()
nameA n = tell mempty
  { uisName    = First (Just n)
  , uiTC       = mempty { tcTempl = TmplInl "<ui-view/>" }
  , uiAbstract = True
  }

addData :: Monad m => JavascriptUrl (Route master) -> WriterT (UiState master) m ()
addData d = tell mempty {uiData = [d]}

emptyFunction :: JavascriptUrl url
emptyFunction = [julius| function(){} |]

liftT :: Text -> Q Exp
liftT t = do
  p <- [|T.pack|]
  return $ AppE p $ LitE $ StringL $ T.unpack t

utcFile :: Text -> Text -> Q Exp
utcFile u st =
   [|tell mempty { uisName = First (Just $(liftT st))
            , uisUrl  = Just $(liftT u)
            , uiTC    = UiTC
                      (TmplExt $(autoHamlet st ""))
                      (CtrlExt $(fromMaybe [| emptyFunction |] $ autoJulius st ""))
                      $(listE $ catMaybes [autoLucius st "", autoCassius st ""])
            }|]

utcVFile :: Text -> Text -> Text -> Q Exp
utcVFile u st view =
   [|tell mempty
      { uisName = First (Just $(liftT st))
      , uisUrl  = Just $(liftT u)
      , uiV = [ ( $(liftT view)
                , UiTC
                    (TmplExt $(autoHamlet st view))
                    (CtrlExt $(fromMaybe [| emptyFunction |] $ autoJulius st view))
                    $(listE $ catMaybes [autoLucius st view, autoCassius st view])
                )
              ]
      }|]

tcFile :: Text -> Q Exp
tcFile st =
   [|tell mempty { uisName = First (Just $(liftT st))
            , uiTC    = UiTC
                      (TmplExt $(autoHamlet st ""))
                      (CtrlExt $(fromMaybe [| emptyFunction |] $ autoJulius st ""))
                      $(listE $ catMaybes [autoLucius st "", autoCassius st ""])
            }|]

tcVFile :: Text -> Text -> Q Exp
tcVFile st view =
   [|tell mempty
      { uisName = First (Just $(liftT st))
      , uiV = [ ( $(liftT view)
                , UiTC
                    (TmplExt $(autoHamlet st view))
                    (CtrlExt $(fromMaybe [| emptyFunction |] $ autoJulius st view))
                    $(listE $ catMaybes [autoLucius st view, autoCassius st view])
                )
              ]
      }|]

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

renderTemplate
  :: ( Monad m
     , MonadThrow m
     , MonadBaseControl IO m
     , MonadIO m
     )
  => StateTemplate master -> GAngular master m (Maybe (JavascriptUrl (Route master)))
renderTemplate = \case
  TmplExt t -> do
        n <- lift newIdent
        tell mempty  { combined   = [ihamlet|<script type="text/ng-template" id="?partial=#{n}">^{t} |]
                     }
        return $ Just [js|templateUrl:"?partial=#{rawJS n}"|]
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
     n <- lift newIdent
     tell mempty { awControllers = [julius|var #{rawJS n} = ^{j};|]}
     return $ Just [js|controller:#{rawJS n}|]
  CtrlExtAs  a j -> do
     n <- lift newIdent
     tell mempty { awControllers = [julius|var #{rawJS n} = ^{j};|]}
     return $ Just [js|controller:#{rawJS n}, controllerAs: "#{rawJS a}"|]
  CtrlProvider  j -> do
     n <- lift newIdent
     tell mempty { awControllers = [julius|var #{rawJS n} = ^{j};|]}
     return $ Just [js|controllerProvider:#{rawJS n}|]
  CtrlNone -> return Nothing

concatJS :: Monoid m => m -> [Maybe m] -> m
concatJS c (catMaybes -> j:rs) = j <> mconcat (map (c <>) rs)
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
