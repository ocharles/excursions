module Quake3.Shader.ShaderRepository where

import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import Data.Traversable
import qualified Quake3.Shader.Parser as ParsedShader
import qualified Quake3.Shader.TypeCheck as TCShader
import System.Directory
import System.FilePath
import Text.Megaparsec (parse)

-- SHADER REPOSITORY


{-| A ShaderRepository is a map of parsed and type-checked shaders.
-}
newtype ShaderRepository =
  ShaderRepository (Map String TCShader.Shader)


loadAllShadersInDirectory
  :: MonadIO m
  => FilePath -> m ShaderRepository
loadAllShadersInDirectory directory =
  do
    m <-
      liftIO $
      do
        files <-
          listDirectory directory

        fmap mconcat $
          for [f | f <- files, takeExtension f == ".shader"] $ \p ->
            do
              src <-
                T.readFile ("../../hs-quake-3/resources/scripts" </> p)

              case parse ParsedShader.parseShaderFile p src of
                Left e ->
                  do
                    putStrLn $ "Failed to parse " ++ p ++ ": " ++ show e

                    return mempty

                Right shaders ->
                  fmap mconcat $
                    for shaders $ \s ->
                      do
                        let
                          (typeChecked, warnings) =
                            TCShader.tcShader s

                        traverse_
                          (putStrLn . ((ParsedShader.shaderName s ++ ": ") ++))
                          warnings

                        return (Map.singleton (ParsedShader.shaderName s) typeChecked)

    return (ShaderRepository m)


lookupShader :: ShaderRepository -> String -> Maybe TCShader.Shader
lookupShader (ShaderRepository m) name =
  Map.lookup name m
