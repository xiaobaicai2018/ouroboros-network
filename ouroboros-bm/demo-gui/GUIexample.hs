{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Graphics.UI.Threepenny as UI

import           Cardano.BM.Controller (setNamedSeverity)
import           Cardano.BM.Data (OutputKind (..), Severity (..),
                     TraceConfiguration (..), TraceContext (..),
                     TraceTransformer (..))
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (Trace, logInfo)

main :: IO ()
main = do
    trace <- setupTrace $ TraceConfiguration StdOut "test" Neutral Debug

    (evFillList, doFillList) <- newEvent
    initialList <- valuesSupply trace Debug
    behFillList <- stepper initialList evFillList

    startGUI defaultConfig $ \win -> do
        list <- ul
        sel <- listBox
            behFillList
            (pure Nothing)
            (pure $ \it -> UI.span # set text (show it))

        getBody win #+ [grid [[element sel, element list]]]
        setFocus $ getElement sel

        on selectionChange (getElement sel) $ \case
            Nothing -> return ()
            Just ix -> do
                items <- currentValue behFillList
                let it = items !! ix
                -- selection
                liftIO $ valuesSupply trace it >>= doFillList
                element list #+ [li # set html (show it)]
                setFocus $ getElement sel
  where
    valuesSupply :: Trace IO -> Severity -> IO [Severity]
    valuesSupply trace@(ctx, _) selectedSev = do
        putStrLn $ "SELECTED severity: " ++ show selectedSev
        setNamedSeverity ctx (loggerName ctx) selectedSev
        logInfo trace "Hello in Info!"
        return [Debug, Info, Warning, Notice, Error]
