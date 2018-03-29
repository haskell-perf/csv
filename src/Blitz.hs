{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |

module Blitz where

import           Control.DeepSeq
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           GHC.Generics

data Values
  = Cons {-# UNPACK #-}!ByteString Values
  | Nil
  deriving (Generic, Show)
instance NFData Values


parseFile :: FilePath -> IO [[ByteString]]
parseFile fp = do
  bytes0 <- S.readFile fp
  pure (parseByteString bytes0)

parseByteString :: ByteString -> [[ByteString]]
parseByteString bytes0 = dispatch bytes0 [] []
  where
    dispatch bytes columns rows =
      case S8.uncons bytes of
        Nothing -> columns : rows
        Just ('"', bytes') ->
          let string extended dropped =
                case S8.elemIndex '"' (S.drop dropped bytes') of
                  Nothing -> error "Unterminated quoted field."
                  Just idx ->
                    case S8.uncons (S.drop (dropped + idx + 1) bytes') of
                      Just ('"', _) -> string True (dropped + idx + 2)
                      Just (',', _) ->
                        let prefinalStr = (S.take (dropped + idx + 1) bytes')
                            finalStr =
                              if extended
                                then S.intercalate
                                       "\""
                                       (filter
                                          (not . S.null)
                                          (S8.split '"' prefinalStr))
                                else prefinalStr
                        in dispatch
                             (S.drop (dropped + idx + 2) bytes')
                             ((:) finalStr columns)
                             rows
                      Just ('\n', _) ->
                        let prefinalStr = (S.take (dropped + idx + 1) bytes')
                            finalStr =
                              if extended
                                then S.intercalate
                                       "\""
                                       (filter
                                          (not . S.null)
                                          (S8.split '"' prefinalStr))
                                else prefinalStr
                        in dispatch
                             (S.drop (dropped + idx + 2) bytes')
                             []
                             (((:) finalStr columns) : rows)
                      Just {} -> error "Expected end of file."
                      Nothing -> error "End of file?"
          in string False 0
        Just _ ->
          case (S8.elemIndex ',' bytes, S8.elemIndex '\n' bytes) of
            (Just comma, Just n)
              | comma < n ->
                let finalStr = S.take comma bytes
                in dispatch
                     (S.drop (comma + 1) bytes)
                     ((:) finalStr columns)
                     rows
              | otherwise ->
                let finalStr = S.take comma bytes
                in dispatch
                     (S.drop (n + 1) bytes)
                     []
                     ((:) finalStr columns : rows)
            (Nothing, Just n) -> ((:) (S.take n bytes) columns) : rows
            x -> error ("Inexhaustive case: " ++ show x)
