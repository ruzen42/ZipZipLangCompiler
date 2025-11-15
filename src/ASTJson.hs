{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module ASTJson where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Aeson.Types (Pair)

import Foreign
import Foreign.C.Types
import Foreign.C.String

import AST

instance ToJSON Program where
  toJSON (Program name decls) =
    object ["name" .= name, "decls" .= decls]

instance ToJSON Decl where
  toJSON = \case
    FunDecl n args ret body ->
      object ["kind" .= ("FunDecl" :: String), "name" .= n, "args" .= args, "ret" .= ret, "body" .= body]
    RecordDecl n fields ->
      object ["kind" .= ("RecordDecl" :: String), "name" .= n, "fields" .= fields]
    EnumDecl n variants ->
      object ["kind" .= ("EnumDecl" :: String), "name" .= n, "variants" .= variants]

instance ToJSON Field where
  toJSON (Field name ty) = object ["name" .= name, "ty" .= ty]

instance ToJSON Type where
  toJSON = \case
    TyNum -> object ["tag" .= ("TyNum" :: String)]
    TyLogic -> object ["tag" .= ("TyLogic" :: String)]
    TyText -> object ["tag" .= ("TyText" :: String)]
    TyAction mt -> object ["tag" .= ("TyAction" :: String), "value" .= mt]
    TyCustom s -> object ["tag" .= ("TyCustom" :: String), "value" .= s]
    TyFun args ret -> object ["tag" .= ("TyFun" :: String), "args" .= args, "ret" .= ret]

instance ToJSON Expr where
  toJSON = \case
    EVar v -> object ["tag" .= ("EVar" :: String), "name" .= v]
    ENum n -> object ["tag" .= ("ENum" :: String), "value" .= n]
    EStr s -> object ["tag" .= ("EStr" :: String), "value" .= s]
    EBool b -> object ["tag" .= ("EBool" :: String), "value" .= b]
    EBinOp op l r -> object ["tag" .= ("EBinOp" :: String), "op" .= show op, "left" .= l, "right" .= r]
    EUnOp op e -> object ["tag" .= ("EUnOp" :: String), "op" .= show op, "expr" .= e]
    ECall n as -> object ["tag" .= ("ECall" :: String), "name" .= n, "args" .= as]
    EIf c t e -> object ["tag" .= ("EIf" :: String), "cond" .= c, "then_" .= t, "else_" .= e]
    ELet n e1 e2 -> object ["tag" .= ("ELet" :: String), "name" .= n, "expr" .= e1, "in_" .= e2]
    ELambda args body -> object ["tag" .= ("ELambda" :: String), "args" .= args, "body" .= body]

-- FFI import
foreign import ccall unsafe "compile_from_json"
  c_compile_from_json :: CString -> CString -> IO CInt

compileProgram :: Program -> FilePath -> IO (Either String ())
compileProgram prog outPath = do
  let json = encode prog
  BS.useAsCStringLen (BL.toStrict json) $ \(cptr, len) ->
    withCString (BL.toStrict json >>= \bs -> pure (map (toEnum . fromEnum) (BS.unpack bs))) $ \_ -> do
      let s = show json
      withCString (BL.unpack json) $ \cjson -> do
        withCString outPath $ \cpath -> do
          rc <- c_compile_from_json cjson cpath
          if rc == 0 then pure (Right ()) else pure (Left ("compile error code: " ++ show rc))
