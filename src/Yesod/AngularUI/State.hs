module Yesod.AngularUI.State where

import           Data.Monoid                (First (..), Monoid (..), (<>))
import           Data.Text                  (Text)

data UiState master m  = UiState
  { uisName     :: First Text
  , uiTC        :: UiTC master m
  }

data UiTC master m = UiTC
  { templ  :: HtmlUrlI18n (SomeMessage master) (Route master))
  , ctrl   :: JavascriptUrl (Route master)
  , ctrlAs :: First Text
  }

instance Monoid (UiState master m) where
    mempty = UiState mempty mempty
    (UiState a1 a2) `mappend` (UiState b1 b2) = UiState
       (mappend a1 b1)
       (mappend a2 b2)

instance Monoid (UiTC master m) where
    mempty = UiTC mempty mempty mempty
    (UiTC a1 a2 a3) `mappend` (UiTC b1 b2 b3) = UiTC
       (mappend a1 b1)
       (mappend a2 b2)
       (mappend a3 b3)
{-

 template vs templateURL <- both collapse now as with the current setup


     .state('route2', {
       url: "/route2",
       views: {
         "viewA": { template: "route2.viewA" },
         "viewB": { template: "route2.viewB" }
       }

       .state('contacts', {
         template: '<h1>My Contacts</h1>'
       })

-- inline inplace
$stateProvider.state('contacts', {
  template: '<h1>My Contacts</h1>'
})

-- inline in url
$stateProvider.state('contacts', {
  templateUrl: 'contacts.html'
})

template | templateUrl | templateProvider

-- generate url
       $stateProvider.state('contacts', {
         templateUrl: function ($stateParams){
           return '/partials/contacts.' + $stateParams.filterBy + '.html';
         }
       })

-- generate template
 $stateProvider.state('contacts', {
   templateProvider: function ($timeout, $stateParams) {
     return $timeout(function () {
      return '<h1>' + $stateParams.contactId + '</h1>'
     }, 100);
  }
})


controllerAs

 controller: function($scope){}
 controller: 'ContactsCtrl'

 controllerAs: 'contact'
 controller: 'ContactsCtrl as contact'

 controllerProvider: function($stateParams) {
       var ctrlName = $stateParams.type + "Controller";
       return ctrlName;
   }


views: { 'view@name' : t+c }


resolve:{}
controller: function()
data: {}
abstract: true,

onEnter
onExit

 -}