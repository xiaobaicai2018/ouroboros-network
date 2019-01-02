# How to microbenchmark.

Needed:

1. A `trace` which will have been configured to observe besides logging.

        trace@(ctx, _) <- setupTrace (Left configFile) "demo-playground"

1. `configFile` is the configuration file of `trace`. An example of configuration can be found in [demo-playground/log-config-0.yaml](../../../../demo-playground/log-config-0.yaml). In order to enable the collection and processing of measurements (min, max mean, std-dev) `AggregationBK` is needed.

        defaultBackends:
          - KatipBK
          - AggregationBK

1. We set the measurements that we want to take by changing the configuration of the `trace` using `setSubTrace`, the namespace where we want to enable the particular measurements, and the list with the kind of measurements.

        CM.setSubTrace (configuration ctx) "demo-playground.submit-tx" $ Just $ ObservableTrace observablesSet
          where
            observablesSet = [MonotonicClock, MemoryStats]

1. Rename the trace that will be used:

        appendName "submit-tx" trace

1. Find an action to measure. E.g.:

        runProtocolWithPipe x hdl proto `catch` (\ProtocolStopped -> return ())

    and use
    * `bracketObserveIO` from [iohk-monitoring-framework](https://github.com/input-output-hk/iohk-monitoring-framework/blob/develop/src/Cardano/BM/Observer/Monadic.lhs#L39); if it is an `IO t` action .
    * `bracketObserve` from [ouroboros-network](./STM.hs) (temporarily there); if it is an `Tr n t` action .

    e.g.:

        bracketObserveIO trace "" $
            runProtocolWithPipe x hdl proto `catch` (\ProtocolStopped -> return ())
