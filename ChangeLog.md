# Revision history for reflex-gadt-api

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
