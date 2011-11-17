module Clang where

data Stage = Preprocess | Compile | Run
  deriving (Show, Eq)
