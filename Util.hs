module Util where

import Import

unMaybe :: Maybe a -> Handler a
unMaybe mba = 
  case mba of 
    Just mba -> return mba
    Nothing -> error "nothing"

unMaybeMsg :: Maybe a -> String -> Handler a
unMaybeMsg mba err = 
  case mba of 
    Just a -> return a
    Nothing -> error err
