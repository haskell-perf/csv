{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | A fast CSV parser.

module Blitz where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

parseFile :: FilePath -> IO (Either String [[ByteString]])
parseFile fp = do
  bytes0 <- S.readFile fp
  pure (parseByteString bytes0)

parseByteString :: ByteString -> Either String [[ByteString]]
parseByteString bytes0 = fmap ($ []) (dispatch bytes0 id id)
  where
    dispatch bytes columns rows =
      case S8.uncons bytes of
        Nothing -> Right (rows . (columns [] :))
        Just ('"', bytes') -> quoted columns rows bytes' False 0
        Just _ -> unquoted bytes columns rows
    unquoted bytes columns rows =
      case (S8.elemIndex ',' bytes, S8.elemIndex '\n' bytes) of
        (Just comma, Just n)
          | comma < n ->
            let finalStr = S.take comma bytes
            in dispatch (S.drop (comma + 1) bytes) (columns . (finalStr :)) rows
          | otherwise ->
            let finalStr = S.take n bytes
            in dispatch
                 (S.drop (n + 1) bytes)
                 id
                 (rows . ((columns . (finalStr :)) [] :))
        (Nothing, Just n) ->
          Right (rows . ((columns . (S.take n bytes :)) [] :))
        (Nothing, Nothing) ->
          Right (rows . ((columns . (S.take (S.length bytes) bytes :)) [] :))
        (Just comma, Nothing) ->
          let finalStr = S.take comma bytes
          in dispatch (S.drop (comma + 1) bytes) (columns . (finalStr :)) rows
    quoted columns rows bytes' extended dropped =
      case S8.elemIndex '"' (S.drop dropped bytes') of
        Nothing -> error "Unterminated quoted field."
        Just idx ->
          case S8.uncons (S.drop (nextQuoteIdx + 1) bytes') of
            Just ('"', _) -> quoted columns rows bytes' True (nextQuoteIdx + 2)
            Just (',', _) -> finishStringBeforeDelim
            Just ('\n', _) -> finishStringBeforeDelim
            Just {} -> Left "Expected end of file."
            Nothing -> Left "End of file?"
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
                       (columns . (finalStr :))
                       rows
