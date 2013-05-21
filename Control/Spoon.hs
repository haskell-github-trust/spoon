{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Spoon
-- Copyright   :  (c) Matt Morrow & Dan Peebles
-- License     :  see LICENSE
-- 
-- Maintainer  :  pumpkingod@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (Scoped Type Variables)
--
-- Two functions for catching pureish exceptions in pure values. This library
-- considers pureish to be any error call or undefined, failed pattern matches, 
-- arithmetic exceptions, and array bounds exceptions.
--
-----------------------------------------------------------------------------


module Control.Spoon (spoon, teaspoon) where

import Control.Exception
import Control.DeepSeq
import System.IO.Unsafe

handlers :: [Handler (Maybe a)]
handlers = [ Handler $ \(_ :: ArithException)   -> return Nothing
           , Handler $ \(_ :: ArrayException)   -> return Nothing
           , Handler $ \(_ :: ErrorCall)        -> return Nothing
           , Handler $ \(_ :: PatternMatchFail) -> return Nothing
           , Handler $ \(x :: SomeException)    -> throwIO x
           ]

-- | Evaluate a value to normal form and return Nothing if any exceptions are thrown during evaluation. For any error-free value, @spoon = Just@.
spoon :: NFData a => a -> Maybe a
spoon a = unsafePerformIO $ deepseq a (Just `fmap` return a) `catches` handlers

-- | Like 'spoon', but only evaluates to WHNF.
teaspoon :: a -> Maybe a
teaspoon a = unsafePerformIO $ (Just `fmap` evaluate a) `catches` handlers

