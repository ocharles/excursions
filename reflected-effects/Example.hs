{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

{-# options_ghc -ddump-simpl -dsuppress-all #-}

module Example ( test ) where

import Data.Functor.Identity
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT )
import Reflected


data Error e m a where
  Throw :: e -> Error e m a
  TryCatch :: m a -> ( e -> m a ) -> Error e m a


instance AnEffect ( Error e ) where
  handle2 ctx eta = \case
    Throw e ->
      Coyoneda ( <$ ctx ) ( Throw e )

    TryCatch m f ->
      Coyoneda id ( TryCatch ( eta ( m <$ ctx ) ) ( eta . ( <$ ctx ) . f ) )


runError
  :: forall e a m g.
     ( Effect g, Carrier g m )
  => ( Interpreted ( Error e + g ) ( ExceptT e m ) a
     )
  -> m ( Either e a )
runError =
  interpretT runExceptT ( either ( return . Left ) runExceptT ) \case
    Throw e ->
      ExceptT ( return ( Left e ) )

    TryCatch m f ->
      ExceptT $ runExceptT m >>= \case
        Left e -> runExceptT ( f e )
        Right ok -> return ( Right ok )



throw :: ( Carrier sig m, Member ( Error e ) sig ) => e -> m a
throw e =
  eff ( inj ( Throw e ) )


program :: ( Carrier sig m, Member ( Error Bool ) sig ) => m Bool
program = do
  _ <- throw False
  return True


test :: Interpreted ( Error Bool + Pure ) ( ExceptT Bool Identity ) Bool -> Either Bool Bool
test =
  run . runError


test2 :: Either Bool ( Either Bool Bool )
test2 =
  run ( runError @Bool ( runError @Bool program ) )
