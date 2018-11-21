{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import           Graphics.UI.Threepenny as UI

import           Cardano.BM.Controller (setMinSeverity, setNamedSeverity)
import           Cardano.BM.Data (LogSelection (..), OutputKind (..),
                     Severity (..), TraceConfiguration (..), TraceContext (..),
                     TraceTransformer (..))
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (Trace, logInfo, traceNamedItem)

main :: IO ()
main = do
    trace <- setupTrace $ TraceConfiguration StdOut "test" Neutral Debug

    (evFillList, doFillList) <- newEvent
    initialList <- valuesSupply trace Debug
    behFillList <- stepper initialList evFillList

    (evFillList', doFillList') <- newEvent
    initialList' <- valuesSupply trace Debug
    behFillList' <- stepper initialList' evFillList'

    (evFillListMinSev, doFillListMinSev) <- newEvent
    initialListMinSev <- valuesSupply trace Debug
    behFillListMinSev <- stepper initialListMinSev evFillListMinSev

    startGUI defaultConfig $ \win -> do
        areaSendTitle <- UI.tr # set UI.text "Send Hello choosing severity:"
        areaSend      <- UI.tr # set UI.text ""
        selSend       <- listBox
                            behFillList'
                            (pure Nothing)
                            (pure $ \it -> UI.span # set text (show it))

        areaGT <- UI.tr # set UI.text "Minimum severity:"
        areaGS <- UI.tr # set UI.text (show Debug)
        selMinSev <- listBox
                        behFillListMinSev
                        (pure Nothing)
                        (pure $ \it -> UI.span # set text (show it))

        areaT <- UI.tr # set UI.text "Trace-specific severity:"
        areaS <- UI.tr # set UI.text (show Debug)
        selTraceSpecific <- listBox
                                behFillList
                                (pure Nothing)
                                (pure $ \it -> UI.span # set text (show it))

        let glue = string " "
        getBody win #+ [grid [[element areaSendTitle, element areaGT, element areaT], [element areaSend, element areaGS, element areaS], [element selSend, element selMinSev, element selTraceSpecific]]]
        setFocus $ getElement selTraceSpecific
        setFocus $ getElement selSend
        setFocus $ getElement selMinSev

        on selectionChange (getElement selSend) $ \case
            Nothing -> return ()
            Just ix -> do
                items <- currentValue behFillList
                let it = items !! ix
                -- selection
                liftIO $ valuesSend trace it >>= doFillList
                element areaSend # set UI.text (show it)
                setFocus $ getElement selSend

        on selectionChange (getElement selMinSev) $ \case
            Nothing -> return ()
            Just ix -> do
                items <- currentValue behFillListMinSev
                let it = items !! ix
                -- selection
                liftIO $ changedMinSev trace it >>= doFillListMinSev
                element areaGS # set UI.text (show it)
                setFocus $ getElement selMinSev

        on selectionChange (getElement selTraceSpecific) $ \case
            Nothing -> return ()
            Just ix -> do
                items <- currentValue behFillList
                let it = items !! ix
                -- selection
                liftIO $ valuesSupply trace it >>= doFillList
                element areaS # set UI.text (show it)
                setFocus $ getElement selTraceSpecific
  where
    valuesSupply :: Trace IO -> Severity -> IO [Severity]
    valuesSupply trace@(ctx, _) selectedSev = do
        putStrLn $ "Selected trace specific severity: " ++ show selectedSev
        setNamedSeverity ctx (loggerName ctx) selectedSev
        return [Debug, Info, Warning, Notice, Error]
    changedMinSev :: Trace IO -> Severity -> IO [Severity]
    changedMinSev trace selectedSev = do
        putStrLn $ "Selected minimum severity: " ++ show selectedSev
        setMinSeverity trace selectedSev
        return [Debug, Info, Warning, Notice, Error]
    valuesSend :: Trace IO -> Severity -> IO [Severity]
    valuesSend trace selectedSev = do
        traceNamedItem trace Both selectedSev $ "Hello in " <> T.pack (show selectedSev)
        return [Debug, Info, Warning, Notice, Error]
