# Revision history for reflex-gadt-api

## 0.4.0.0

* Add `Reflex.Dom.GadtApi.Codec`, exposing the pure `encodeApiRequest` and `serveApiRequest` codecs used by the transports; `serveApiRequest` gives servers a reusable untagged decode/interpret/encode step
* Add a test suite covering request tagging and correlation (in order, out of order, and multiple requests per frame), duplicate and unknown tags, malformed payloads and frames, and the codec helpers
* *Breaking*: `performWebSocketRequests` takes a `WebSocketClientConfig` (URL, subprotocols, close, and reconnect) and additionally returns a `WebSocketClientStatus` exposing connection open/error/close events, unparseable incoming messages, and responses that don't match any outstanding request. Note that outstanding requests still survive disconnects and reconnects
* *Breaking*: `tagRequests` additionally returns an event of responses whose tag matches no outstanding request; previously such responses were dropped silently
* *Breaking*: `performXhrRequests` and `apiRequestXhr` take an `XhrClientConfig` whose `configureRequest` hook can adjust the outgoing `XhrRequest` (e.g. to add headers or credentials)
* *Breaking*: transport-level failures are now reported instead of hanging: `XhrError` carries `Either XhrException XhrResponse`, so aborted requests and requests that could not be sent surface as errors; consequently `XhrError` no longer derives `Generic`

## 0.3.0.0

* *Breaking*: switch to a more useful xhr error response type. To retain the current behavior, you must convert `Either XhrResponse` to `Either Text`. See `Readme.lhs` for an example using `xhrErrorToText`.

## 0.2.2.3

* Support GHC 9.12

## 0.2.2.2

* Make Readme part of the library
* Support GHC 9.10
* Loosen version bounds

## 0.2.2.1-r1

* Loosen version bounds

## 0.2.2.1

* Support constraints-extras 0.4

## 0.2.2.0

* Support GHC 8.10
* Make xhr requests non-blocking

## 0.2.1.1

* Expose `Tagged`* constructors

## 0.2.1.0

* Use "some" instead of "dependent-sum" package

## 0.2.0.1

* Fix haddocks of `tagRequests`

## 0.2.0.0

* Add WebSocket interface
* Breaking Change: `performXhrRequests` now returns an `Event` instead of a `Dynamic` of an `Event`.

## 0.1.0.0

* XML HTTP Request FRP interface for interacting with an API defined as a GADT
