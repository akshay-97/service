{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Serve where

import Servant
import Data.Text as Text
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as Hm
import Data.Bool (bool)
import Data.Scientific (fromFloatDigits)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson.TH (deriveJSON)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)

type ServiceResponse = Hm.HashMap Text A.Value

data ServiceRequest = ServiceRequest{
    req :: Text
}

deriveJSON A.defaultOptions ''ServiceRequest

type ServiceApis = 
    "convert" :> ReqBody '[JSON] ServiceRequest :> Post '[JSON] ServiceResponse

debug :: Text -> ()
debug t = unsafePerformIO (putStrLn $ unpack t)

serviceCall :: ServiceRequest -> Handler ServiceResponse
serviceCall ServiceRequest{req} = do
    liftIO $ putStrLn $ unpack req
    let r = mapOnEither $ splitOnInt <$> (Text.split ((==) '#') req)
    case r of
        Right arrRes -> pure $ Hm.fromList arrRes
        Left err ->
            throwError err400 { errBody = B.pack err}
    where
        splitOnInt :: Text -> Either String (Text, A.Value)
        splitOnInt str = do
            let (dType, inp) = Text.splitAt 2 str
            case (unpack dType) of
                (isArr:dt:[]) ->
                    maybe (Left "Error in reading type info") (processKeyValue inp) (getNumber isArr dt)
                _ -> (Left "No data")
            where
                getNumber isArr dt =
                    case (readMaybe [isArr], readMaybe [dt]) of
                        (Just isArr, Just dType) -> Just (isArr, dType)
                        _ -> Nothing

        mapOnEither :: [Either String a] -> Either String [a]
        mapOnEither = Prelude.foldr processEither (Right [])
            where
                processEither (Left err) b = if err == "No data" then b else Left err
                processEither _ (Left err) = Left err
                processEither (Right a) (Right acc) = Right (a:acc)
        
        processKeyValue :: Text -> (Int, Int) -> Either String (Text, A.Value)
        processKeyValue str (isArray, dType) =
            case (Text.split ((==) '|') str) of
                (key:value:_) ->
                    (\v -> (key,v)) <$> decodeToJSON value (isArrayData isArray) (parseFn dType)
                _ -> Left "Invalid Key Value Pair"
        
        decodeToJSON :: Text -> Bool -> (Text -> Either String A.Value) -> Either String A.Value
        decodeToJSON inp True parser  =
            let res :: (Either String [A.Value]) = Prelude.foldr convertToResult (Right []) $ parser <$> Text.split ((==) ',') inp 
              in (A.toJSON) <$> res
            where
                convertToResult (Left err) _ = Left err
                convertToResult _ (Left err) = Left err
                convertToResult (Right v) (Right acc) = Right (v:acc)

        decodeToJSON inp False parser = parser inp
        

        isArrayData :: Int -> Bool
        isArrayData 0 = False
        isArrayData 1 = True
        isArrayData _ = False

        parseFn :: Int -> (Text -> Either String A.Value)
        parseFn 0 = \inp -> (A.String) <$> validateDate inp
        parseFn 1 = \inp -> 
            maybe (Left "Invalid Number") (Right . A.Number)
                $ fromFloatDigits <$> readMaybe (unpack inp)
        parseFn 2 = Right . A.String
        parseFn 3 =
            \inp -> 
                if (unpack inp) `Prelude.elem` ["y", "Y", "T", "t"]
                    then Right $  A.Bool True
                else if (unpack inp) `Prelude.elem` ["n", "N", "f", "F"]
                    then Right $ A.Bool False
                else
                    Left "Invalid Bool"

        parseFn _ = const $ Left "Unsupported Type"

        validateDate :: Text -> Either String Text
        validateDate st =
            case (Text.split ((==) '-') st) of
                (y:m:d:[]) ->
                    maybe (Left "Invalid Date") (bool (Left "Invalid Date") (Right st))
                        $ isDateValid <$> readMaybe (unpack y) <*> readMaybe (unpack m) <*> readMaybe (unpack d)
                _ -> Left "Invalid Date"
            

        isDateValid :: Int -> Int -> Int -> Bool
        isDateValid y m d
            | y > 1900 && y < 2100 = True
            | m `Prelude.elem` [1,3,5,7,8,10,12] = d <= 31
            | m `Prelude.elem` [4,6,9,11]        = d <= 30
            | m == 2  = d <= 29
            | otherwise = False

server :: Server ServiceApis
server = serviceCall

{-
#00date|1997-02-06
#02name|bob
#01age|20
#03hasPassport|Y
#12access|read_db,write_db,view_logs;
-}