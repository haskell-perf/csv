{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | A fast CSV parser.

module Blitz where

import           Control.Monad.Catch
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.DList as DL
import           Data.Typeable

data BlitzException =
  UnterminatedQuotedField | ExpectedEndOfFile
  deriving (Show, Typeable)
instance Exception BlitzException

parseFile :: FilePath -> IO [[ByteString]]
parseFile fp = do
  bytes0 <- S.readFile fp
  parseByteString bytes0

{-# SPECIALIZE parseByteString :: ByteString -> IO [[ByteString]] #-}
{-# SPECIALIZE parseByteString :: ByteString -> Either SomeException [[ByteString]] #-}
parseByteString :: MonadThrow m => ByteString -> m [[ByteString]]
parseByteString bytes0 = fmap DL.toList (dispatch bytes0 DL.empty DL.empty)
  where
    dispatch bytes columns rows =
      case S8.uncons bytes of
        Nothing -> pure (DL.snoc rows (DL.toList columns))
        Just ('"', bytes') -> quoted columns rows bytes' False 0
        Just _ -> unquoted bytes columns rows
    unquoted bytes columns rows =
      case (S8.elemIndex ',' bytes, S8.elemIndex '\n' bytes) of
        (Just comma, Just n)
          | comma < n ->
            let finalStr = S.take comma bytes
            in dispatch
                 (S.drop (comma + 1) bytes)
                 (DL.snoc columns finalStr)
                 rows
          | otherwise ->
            let finalStr = S.take n bytes
            in dispatch
                 (S.drop (n + 1) bytes)
                 DL.empty
                 (DL.snoc rows (DL.toList (DL.snoc columns finalStr)))
        (Nothing, Just n) ->
          pure (DL.snoc rows (DL.toList (DL.snoc columns (S.take n bytes))))
        (Nothing, Nothing) ->
          pure
            (DL.snoc
               rows
               (DL.toList (DL.snoc columns (S.take (S.length bytes) bytes))))
        (Just comma, Nothing) ->
          let finalStr = S.take comma bytes
          in dispatch (S.drop (comma + 1) bytes) (DL.snoc columns finalStr) rows
    quoted columns rows bytes' extended dropped =
      case S8.elemIndex '"' (S.drop dropped bytes') of
        Nothing -> throwM UnterminatedQuotedField
        Just idx ->
          case S8.uncons (S.drop (nextQuoteIdx + 1) bytes') of
            Just ('"', _) -> quoted columns rows bytes' True (nextQuoteIdx + 2)
            Just (',', _) -> finishStringBeforeDelim
            Just ('\n', _) -> finishStringBeforeDelim
            Just {} -> throwM ExpectedEndOfFile
            _ -> finishStringBeforeDelim
          where nextQuoteIdx = dropped + idx
                finishStringBeforeDelim =
                  let prefinalStr = S.take nextQuoteIdx bytes'
                      finalStr =
                        if extended
                          then S.intercalate
                                 "\""
                                 (filter
                                    (not . S.null)
                                    (S8.split '"' prefinalStr))
                          else prefinalStr
                  in dispatch
                       (S.drop (nextQuoteIdx + 2) bytes')
                       (DL.snoc columns finalStr)
                       rows
