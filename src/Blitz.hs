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
import           Data.ByteString.Internal
import           Data.ByteString.Unsafe
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
-- import           Debug.Trace
import           GHC.Generics

data Rows
  = Cons {-# UNPACK #-}!ByteString Rows
  | Nil
  deriving (Generic)
instance NFData Rows

parseFile :: FilePath -> IO Rows
parseFile fp = do
  bytes0 <- S.readFile fp
  let dispatch bytes acc =
        case S8.uncons bytes of
          Nothing -> acc
          Just ('"', bytes') ->
            let string extended dropped =
                  case S8.elemIndex '"' (S.drop dropped bytes') of
                    Nothing -> error "Unterminated quoted field."
                    Just idx ->
                      case S8.uncons (S.drop (dropped + idx + 1) bytes') of
                        Just ('"', _) ->
                          trace
                            ("DOUBLE + EXTEND" :: String)
                            (string True (dropped + idx + 2))
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
                          in trace
                               ("QUOTED + COMMA " ++ show finalStr)
                               (dispatch
                                  (S.drop (dropped + idx + 2) bytes')
                                  (Cons finalStr acc))
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
                          in trace
                               ("QUOTED + CRLF " ++ show finalStr)
                               (dispatch
                                  (S.drop (dropped + idx + 2) bytes')
                                  (Cons finalStr acc))
                        Just {} -> error "Expected end of file."
                        Nothing -> error "End of file?"
            in string False 0
          Just _ ->
            case (S8.elemIndex ',' bytes, S8.elemIndex '\n' bytes) of
              (Just comma, Just n)
                | comma < n ->
                  let finalStr = S.take comma bytes
                  in trace
                       ("COMMA " ++ show finalStr)
                       (dispatch (S.drop (comma + 1) bytes) (Cons finalStr acc))
                | otherwise ->
                  let finalStr = S.take comma bytes
                  in trace
                       ("CRLF " ++ show (S.take n bytes))
                       (dispatch (S.drop (n + 1) bytes) (Cons finalStr acc))
              (Nothing, Just n) -> trace ("EOF " ++ show (S.take n bytes)) acc
              x -> error ("Inexhaustive case: " ++ show x)
  pure (dispatch bytes0 Nil)

trace :: p1 -> p2 -> p2
trace !_ x = x
{-# INLINE trace #-}
