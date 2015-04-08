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
    , addCommandAuth
    , addCommandMaybe
    , addCommandMaybeAuth
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
   -- some normal stuff

    , addCtrl
    , addState
    , addStateAuth
    , addStateJ
    , addStateJAuth
    , addStateJAuthE
    , addStateA
    , addStateAAuth
    , addStateJC
    , addStateV
     -- ^ add view $(addStateVAuth "video.blab" "@" "/:param")
     --  $(addStateVAuth "video.blab" "@video"   "/:param" )
    , addStateVAuth
   -- ^ different ways of adding new controller 
   -- maybe some typeclass to abstract all of them?
   -- auth versions require function which evaluates to true.
    , addCtrlRaw
    , addSetup
   -- ^ before the code, maybe some imports?

    , addWhen
    , addWhenAuth
   --  , addWhenRE
    , setDefaultRoute
    , addREST
    , addRESTRaw
   -- placeholders for some generic rest api/ maybe a subsite?

    , GAngular
    , nullAuth
    ) where


--- the chaos

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                 (fromMaybe, catMaybes)
import           Data.Monoid                (First (..), Monoid (..), (<>))
import           Data.Text                  (Text)
import           Text.Hamlet
import Text.Blaze.Html
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
import Text.Jasmine (minify)
import           Yesod.Core.Types
import Yesod.Core.Content
import           Yesod.Core.Json
import Language.Haskell.TH.Syntax (Q, Exp (..), Lit (..))
import Language.Haskell.TH (listE)
import qualified Data.Text as T
import Data.Char (isAlpha)

import Control.Monad (when, unless, (>=>))
import           Data.Either
import           Prelude                    hiding (head, init,
                                                    last, readFile,
                                                    tail, writeFile)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Yesod.AngularUI.Router
import Text.Shakespeare.I18N
import Data.List
import qualified Data.Text.Lazy.Encoding as E (encodeUtf8, decodeUtf8)

class (Yesod master) => YesodAngular master where
    urlAngularJs :: [master -> Either (Route master) Text]
    urlAngularJs  = []
                  -- > add bower packages

    angularUIEntry :: WidgetT master IO ()
    angularUIEntry = [whamlet|<div data-ui-view>|]
    wrapAngularUI :: Text ->  WidgetT master IO ()
    wrapAngularUI modname = [whamlet|ng-app="#{modname}"|]


data (Monad m) => AngularWriter master m  = AngularWriter
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
    }
instance (Monad m) => Monoid (AngularWriter master m) where
    mempty = AngularWriter mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
    (AngularWriter a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
        `mappend` (AngularWriter b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
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

type GAngular master m = WriterT (AngularWriter master m) (HandlerT master m)
-- renSoMsg :: (SomeMessage master) -> Text.Hamlet.Translate (SomeMessage master)

renSoMsg :: (SomeMessage master  -> Text) -> SomeMessage master -> Html
-- renSoMsg :: (YesodAngular master, forall msg . RenderMessage master msg)=> (SomeMessage master -> Text ) -> (SomeMessage master -> Html)
renSoMsg f = toHtml . f

runAngularUI :: (YesodAngular master)
           => Bool                                                      -- ^ cache templates
           -> (HandlerT master IO Bool -> HandlerT master IO ())        -- ^ auth renderer
           -> GAngular master IO ()                                     -- ^ angular app
           -> (Text -> WidgetT master IO () -> HandlerT master IO Html) -- ^ layout
           -> HandlerT master IO Html
runAngularUI cache p ga dl = do
    master <- getYesod
    mrender <- renSoMsg <$> getMessageRender
    urender <- getUrlRenderParams
    ((), AngularWriter{..}) <- runWriterT ga
    mp <- lookupGetParam "partial"
    case mp >>= flip Map.lookup awPartials of
        Nothing -> when ( mp == Just "$combined" ) $
            sendResponse . toTypedContent $ combined mrender urender
        Just (a, !htmlurl) -> p a >> (sendResponse . toTypedContent $ htmlurl mrender urender)
    mc <- lookupGetParam "command"
    maybe (return ()) (\(a,b) -> p a >> b) $ mc >>= flip Map.lookup awCommands

    let cc = if cache -- maybe False for debug?
              then [julius|
       .config(["$provide", function($provide) {
    $provide.decorator("$templateCache", ["$delegate", "$http", "$injector","$log", function ($delegate, $http, $injector,$log) {

        var useCombined = true;
        var origGetMethod = $delegate.get;
        var allDirectivesTplPromise;

        var loadDirectivesTemplates = function(url){
            if (!allDirectivesTplPromise) {
                $log.debug("fetching all directives templates file");
                allDirectivesTplPromise = $http.get("?partial=$combined");
                allDirectivesTplPromise.error(function(data,status,header,config){
                    $log.debug("failed to load combined templates" + status);
                    useCombined = false; });

                allDirectivesTplPromise.then(function(response) {
                        $injector.get("$compile")(response.data);
                        return response;
                    });
            }
            return allDirectivesTplPromise.then(function(response) {
                return { status: response.status
                       , headers: response.headers
                       , config: response.config
                       , statusText: response.statusText
                       , data: origGetMethod(url)
                       };
            })
        };

        $delegate.get = function(url){
            if ( useCombined && (/^\?partial=[^$]/.test(url)) ) {
                return loadDirectivesTemplates(url);
            }
            return origGetMethod(url);
        };
        return $delegate;
    }]);
}])
       |]
              else [julius|
           |]

    modname <- newIdent
    let defaultRoute =
            case (awDefaultRoute, awStateName) of
                (filter (`elem` awStateName) -> x:_, _)  -> [julius|.otherwise("#{rawJS x}")|]
                (_, x:_) ->             [julius|.otherwise("#{rawJS x}")|]
                (_,[])   -> mempty
    dl modname $ do
        mapM_ (\x -> addScriptEither $ x master) urlAngularJs
        angularUIEntry --  = [whamlet|<div data-ui-view layout=row layout-fill>|] -- bla
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
    ^{cc}
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
    let name = name' `mappend` "Provider"
    tell mempty
        { awConfigs = [julius|.config(["#{rawJS name}", function (#{rawJS name}){ 
        #{rawJS name}.^{funcall}; }])|]
        }

addConfigRaw :: (Monad m) => ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                          -> GAngular master m ()
addConfigRaw funcall = tell mempty { awConfigs = [julius|.config(^{funcall})|] }

addDirective :: (Monad m) => Text
                          -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                          -> GAngular master m ()
addDirective name funcall =
    tell mempty
        { awDirectives = [julius|.directive("#{rawJS name}", ^{funcall} )|]
        }

addController :: (Monad m) => Text
                           -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                           -> GAngular master m ()
addController name funcall =
    tell mempty
        { awDirectives = [julius|.controller("#{rawJS name}", ^{funcall} )|]
        }


addFilter :: (Monad m) => Text
                       -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                       -> GAngular master m ()
addFilter name funcall =
    tell mempty
        { awDirectives = [julius|.filter("#{rawJS name}", ^{funcall} )|]
        }


addFactory :: (Monad m) => Text
                        -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                        -> GAngular master m ()
addFactory name funcall = addProvide [js|factory("#{rawJS name}",^{funcall})|]

addREST :: (Monad m) => Text
                      -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                      -> GAngular master m ()
addREST name funcall =
    tell mempty
        { awModules = ["ngResource"]
        , awDirectives = [julius|
         .factory("#{rawJS name}",["$resource", function($resource){
          return $resource(^{funcall}); }])|]
        }

addRESTRaw :: (Monad m) => Text
                        -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                        -> GAngular master m ()
addRESTRaw name funcall =
    tell mempty
        { awModules = ["ngResource"]
        , awDirectives = [julius|
         .factory("#{rawJS name}",["$resource", function($resource){
      ^{funcall}
          }])|]
        }


addService :: (Monad m) =>
                      Text
                      -> ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                      -> GAngular master m ()
-- ^ adds a service
addService name funcall = addProvide [js|service("#{rawJS name}",^{funcall})|]

addProvide :: (Monad m) => ((Route master -> [(Text, Text)] -> Text) -> Javascript) -> GAngular master m ()
addProvide funcall =
    tell mempty
        { awServices = [julius|
        $provide.^{funcall} ;|]
        }

addConstant :: (Monad m) => Text -> ((Route master -> [(Text, Text)] -> Text) -> Javascript) -> GAngular master m ()
-- ^ add constant, remember to quote if raw text
addConstant name funcall = addProvide [julius|constant("#{rawJS name}",^{funcall})|]

addSetup :: (Monad m) => ((Route master -> [(Text, Text)] -> Text) -> Javascript)
                      -> GAngular master m ()
-- ^ inject this code befor calling angular
addSetup funcall =
    tell mempty
        { awSetup = [julius|^{funcall};|]
        }

nullAuth :: Monad m => m Bool
nullAuth = return True

addCommand :: (FromJSON input, ToJSON output)
           => (input -> HandlerT master IO output)
           -> GAngular master IO Text
-- ^ add a command (which is always printed)
addCommand = addCommandAuth nullAuth

addCommandAuth :: (FromJSON input, ToJSON output)
           => HandlerT master IO Bool
           -> (input -> HandlerT master IO output)
           -> GAngular master IO Text
-- ^ add command conditionally depending on autenticator function
addCommandAuth a f = addCommandMaybeAuth a (fmap Just <$> f )

addCommandMaybeAuth :: (FromJSON input, ToJSON output)
           => HandlerT master IO Bool
           -> (input -> HandlerT master IO (Maybe output))
           -> GAngular master IO Text
-- ^ add command which on a failure goes 404
addCommandMaybeAuth a f = do
    name <- lift newIdent
    tell (mempty :: AngularWriter master IO) { awCommands = Map.singleton name (a, handler) }
    return $ "?command=" `mappend` name
  where
    handler = requireJsonBody >>= f >>= maybe notFound (returnJson >=> sendResponse)

addCommandMaybe :: (FromJSON input, ToJSON output)
           => (input -> HandlerT master IO (Maybe output))
           -> GAngular master IO Text
addCommandMaybe f = do
    name <- lift newIdent
    tell (mempty :: AngularWriter master IO) { awCommands = Map.singleton name (nullAuth, handler) }
    return $ "?command=" `mappend` name
  where
    handler = requireJsonBody >>= f >>= \case
         Just output -> do repjson <- returnJson output
                           sendResponse repjson
         Nothing -> notFound


addCtrl :: Text -- ^ route pattern
        -> Text -- ^ template name
        -> Q Exp
addCtrl route name = do
    let name' = T.filter isAlpha name
    -- TODO: Allow empty files
    [|addCtrlRaw $(liftT name')
                 $(liftT route)
                 $(autoHamlet "" name)
                 $(fromMaybe [| emptyFunction |] $ autoJulius "" name)
                 $(listE $ catMaybes [autoLucius "" name, autoCassius "" name])
                 |]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

addStateJ :: Text -> Text -> Q Exp
addStateJ state url = addState state (Just url)

addStateJAuth :: Text -> Text -> Q Exp
addStateJAuth state url = addStateAuth state (Just url)

addStateJAuthE :: Text -> Text -> Q Exp
addStateJAuthE state url = addStateAuthE state (Just url)

-- | create state
addState :: Text -> Maybe Text ->  Q Exp
addState state url =
   [|addCtrlRawState
       $(liftT state)
       $(liftT (fromMaybe ("/" <> T.replace "." "/" state) url))
       $(autoHamlet state "")
       $(fromMaybe [| emptyFunction |] $ autoJulius state "")
       $(listE $ catMaybes [autoLucius state "", autoCassius state ""])
       |]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

addStateAuth :: Text -> Maybe Text -> Q Exp
addStateAuth state url =
   [|addCtrlRawStateAuth2
            $(liftT state)
            $(liftT (fromMaybe ("/" <> T.replace "." "/" state) url))
            $(autoHamlet state "")
            $(fromMaybe [| emptyFunction |] $ autoJulius state "")
            $(listE $ catMaybes [autoLucius state "", autoCassius state ""])
            |]

  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

addStateAuthE :: Text -> Maybe Text -> Q Exp
addStateAuthE state url =
   [|addCtrlRawStateAuth2E
      $(liftT state)
      $(liftT (fromMaybe ("/" <> T.replace "." "/" state) url))
      $(autoHamlet state "") 
      $(fromMaybe [| emptyFunction |] $ autoJulius state "")
      $(listE $ catMaybes [autoLucius state "", autoCassius state ""])
      |]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t


addStateV :: Text -> Text -> Text ->  Q Exp
addStateV state vi url =
   [|addCtrlRawStateV1
           $(liftT state)
           $(liftT vi)
           $(liftT url)
           $(autoHamlet state vi)
           $(fromMaybe [| emptyFunction |] $ autoJulius state vi)
           $(listE $ catMaybes [autoLucius state vi, autoCassius state vi])
           |]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

addStateVAuth ::  Text -> Text -> Text ->  Q Exp
addStateVAuth state vi url =
   [|addCtrlRawStateV1Auth
         $(liftT state)
         $(liftT vi)
         $(liftT url)
         $(autoHamlet state vi)
         $(fromMaybe [| emptyFunction |] $ autoJulius state vi)
         $(listE $ catMaybes [autoLucius state vi, autoCassius state vi])
         |]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

emptyFunction = [julius| function(){} |]

addStateJC :: Text -> Maybe Text -> Q Exp
addStateJC state url =
   [|addCtrlRawStateJC
       $(liftT state)
       $(liftT (fromMaybe ("/" <>  T.replace "." "/" state) url))
       $(autoHamlet state "") 
       $(fromMaybe [| emptyFunction |] $ autoJulius state "")
       $(listE $ catMaybes [autoLucius state "", autoCassius state "" ])
       |]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t

addCtrlRaw :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text                         -- ^ user-friendly name
                -> Text                         -- ^ route pattern
                -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> GAngular master m ()
addCtrlRaw name' route template controller xcss = do
    name <- mappend ( mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials    = Map.singleton name (nullAuth, template)
        , combined      = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
        , awRoutes      = [julius|.when("#{rawJS route}", {controller:#{rawJS name}, templateUrl:"?partial=#{rawJS name}"})|]
        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        , awLook = xcss
        }

addWhen :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text -- ^ one route
                -> Text -- ^ the other one
                -> GAngular master m ()
addWhen fro to  = tell mempty {awRoutes = [julius|.when("#{rawJS fro}","#{rawJS to}")|] }

addWhenAuth :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text
                -> Text
                -> (v -> HandlerT master m Bool)
                -> v
                -> GAngular master m ()
addWhenAuth fro to a v =  do
   a' <- lift (a v)
   when a' $ tell mempty {awRoutes = [julius|.when("#{rawJS fro}","#{rawJS to}")|] }

addCtrlRawState :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text                         -- ^ user-friendly name
                -> Text                         -- ^ route pattern
                -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> GAngular master m ()
addCtrlRawState name'' route template controller xcss = do
    let name' = T.filter isAlpha name''
    name <- mappend (mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials    = Map.singleton name (nullAuth, template)
        , combined      = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
        , awStateName   = [name'']
        , awStates      = [julius|.state("#{rawJS name''}", { url:"#{rawJS route}", controller:#{rawJS name}, templateUrl:"?partial=#{rawJS name}"})|]
        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        , awLook        = xcss
        }

{-
  three types of templates:
     template :: Text
     templateUrl :: Url
     templateProvider :: Javascriot

  controlers
    controlers = js
                 string | ctrl name or controller: 'ContactsCtrl as contact'
    controllerProvider
  resolve:
    views: {
        "viewA": { template: "index.viewA" },
        "viewB": { template: "index.viewB" }
     }
-}

addCtrlRawStateAuth :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => HandlerT master m Bool       -- ^ autenticator function
                -> Text                         -- ^ user-friendly name
                -> Text                         -- ^ route pattern
                -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> GAngular master m ()

addCtrlRawStateAuth a name'' route template controller xcss = do
    a' <- lift a
    when a' $ do
     let name' = T.filter isAlpha name''
     name <- mappend ( mappend name' "__" ) <$> lift newIdent
     tell mempty
         { awPartials = Map.singleton name (a, template)
         , combined = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
         , awStateName   = [name'']
         , awStates = [julius|
     .state("#{rawJS name''}"
           , {url:"#{rawJS route}"
           , controller:#{rawJS name}
           , templateUrl:"?partial=#{rawJS name}"})|]
           , awControllers = [julius|var #{rawJS name} = ^{controller};|]
           , awLook = xcss
         }


addCtrlRawStateAuth2 :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              , ToJSON v
              ) => Text                         -- ^ user-friendly name
                -> Text                         -- ^ route pattern
                -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> (v -> HandlerT master m Bool)-> v -> GAngular master m()
addCtrlRawStateAuth2 name'' route template controller xcss a v = do
   a' <- lift (a v)
   when a' $ do
    name <- saneName name''
    tell mempty
        { awPartials = Map.singleton name (a v, template)
        , combined = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
        , awStateName   = [name'']
        , awStates = [julius|
  .state("#{rawJS name''}"
          , { url:"#{rawJS route}"
          , data: { auth: #{toJSON v} }
          , templateUrl:"?partial=#{rawJS name}"
          , controller:#{rawJS name}
          })|]

        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        , awLook = xcss
        }

addCtrlRawStateAuth2E :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              , ToJSON v
              ) => Text                         -- ^ user-friendly name
                -> Text                         -- ^ route pattern
                -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> (v -> HandlerT master m Bool)-> v -> GAngular master m()
addCtrlRawStateAuth2E name'' route template controller xcss a v = do
   a' <- lift (a v)
   name <- saneName name''
   when a' $
     tell mempty
        { awPartials = Map.singleton name (a v, template)
        , combined = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
        , awStateName   = [name'']
        , awStates = [julius|
  .state("#{rawJS name''}"
          , { url:"#{rawJS route}"
          , data: { auth: #{toJSON v} }
          , templateUrl:"?partial=#{rawJS name}"
          , controller:#{rawJS name}
          })|]

        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        , awLook = xcss
        }
   unless a' $
      tell mempty
        { awStateName   = [name'']
        , awStates = [julius|.state("#{rawJS name''}", {url:"#{rawJS route}", template: '<ui-view/>' })|]
        }



addCtrlRawStateJC :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text                         -- ^ user-friendly name
                -> Text                         -- ^ route patter
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> GAngular master m ()
addCtrlRawStateJC name'' route {- !template -} controller xcss = do
    name <- saneName name''
    tell mempty
        { awStateName   = [name'']
        , awStates = [julius|.state("#{rawJS name''}", {url:"#{rawJS route}", controller:#{rawJS name} })|]
        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        , awLook = xcss
        }

addCtrlRawStateV1 :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text                         -- ^ user-friendly name
                -> Text                         -- ^ view-friendly name
                -> Text                         -- ^ route pattern
                -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> GAngular master m()
addCtrlRawStateV1 name'' vi route template controller xcss = do
    name <- saneName name''
    tell mempty
        { awPartials = Map.singleton name (nullAuth, template)
        , combined = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
        , awStateName   = [name'']
        , awStates = [julius|
   .state("#{rawJS name''}"
         , { url:"#{rawJS route}"
         , views: { "#{rawJS vi}" : { controller:#{rawJS name}
                                    , templateUrl:"?partial=#{rawJS name}"
                                    }
                    }
           }
         )|]
        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        , awLook = xcss
        }

saneName name'' = mappend (mappend (T.filter isAlpha name'') "__") <$> lift newIdent


addCtrlRawStateV1Auth :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              , ToJSON v
              ) => Text                         -- ^ user-friendly name
                -> Text                         -- ^ view-friendly name
                -> Text                         -- ^ route pattern
                -> HtmlUrlI18n (SomeMessage master) (Route master)       -- ^ template
                -> JavascriptUrl (Route master) -- ^ controller
                -> [CssUrl (Route master)]
                -> (v -> HandlerT master m Bool) -> v -> GAngular master m()
addCtrlRawStateV1Auth name'' vi route template controller xcss a v = do
   a' <- lift (a v)
   when a' $ do
    name <- saneName name''
    tell mempty
        { awPartials = Map.singleton name (a v, template)
        , combined = [ihamlet|<script type="text/ng-template" id="?partial=#{name}">^{template} |]
        , awStateName   = [name'']
        , awStates = [julius|
   .state("#{rawJS name''}"
         , { url:"#{rawJS route}"
           , data: { auth: #{toJSON v} }
           , views: { "#{rawJS vi}" : { controller:#{rawJS name}
                                      , templateUrl:"?partial=#{rawJS name}" 
                                      } 
                    } 
           }
         )|]
        , awControllers = [julius|var #{rawJS name} = ^{controller};|]
        , awLook = xcss
        }

addStateA :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              ) => Text       -- ^ user-friendly name
                -> Text            -- ^ route pattern
                -> GAngular master m()
     -- ^ empty state ?
addStateA name'' route =
    tell mempty
        { awStateName   = [name'']
        , awStates = [julius|.state("#{rawJS name''}", {url:"#{rawJS route}", template: '<ui-view/>' })|]
        }

addStateAAuth :: ( Monad m
              , MonadThrow m
              , MonadBaseControl IO m
              , MonadIO m
              , ToJSON v
              ) => Text        -- ^ user-friendly name
                -> Text        -- ^ route pattern
                -> (v -> HandlerT master m Bool)-> v
                -> GAngular master m()
addStateAAuth name'' route a v = do
   a' <- lift (a v)
   when a' $
    tell mempty
        { awStateName   = [name'']
        , awStates = [julius|
 .state("#{rawJS name''}"
       , {url:"#{rawJS route}"
       , data: { auth: #{toJSON v} }
       , template: '<ui-view/>' }
       )|]
        }

setDefaultRoute :: (Monad m) => Text -> GAngular master m()
setDefaultRoute x = tell mempty { awDefaultRoute = [x] }

addFactoryStore :: (Monad m) => Text -> GAngular master m ()
addFactoryStore name = addFactory (name <> "Store") [julius| function(){
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
