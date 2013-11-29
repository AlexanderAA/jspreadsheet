module WSAction (
    processChangeset
) where


import qualified Data.Aeson                  as AE
import qualified Data.Text                   as T


import qualified SpreadsheetTypes            as ST
import qualified WSCommon                    as WSC


processChangeset :: WSC.Message -> IO WSC.Message
processChangeset m@(WSC.Message Nothing control) = do
    return (WSC.Message Nothing Nothing)
processChangeset m@(WSC.Message (Just msg) control) = do
    let lazyMsg = WSC.textToLazyBS msg
    let hsobj = (AE.decode lazyMsg :: Maybe ST.SUpdate)
    case hsobj of 
        Nothing -> return (WSC.Message Nothing Nothing)
        Just hsobj -> do
            let encoded = (AE.encode $ ST.updateToCells hsobj)
            return (WSC.Message (Just $ T.pack $ show encoded) Nothing)