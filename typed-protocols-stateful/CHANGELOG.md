# Revision history of typed-protocols-stateful

## 0.3.0.0

* when encoding a message the local state associated to the initial message
  state, rather than final state, is passed to the codec. For that reason:
  * `Yield` requires local state associated to both the initial and final protocol state
  *  `Codec` and `Driver` have changed accordingly

  This change eliminates the need to have add extra fields in messages which
  are not send over the wire, see the `Network.TypedProtocol.Stateful.ReqResp`
  example.
* `AnyMessage` takes only the local state associated to the initial protocol state of the `Message`.
* Removed `Show` instance of `AnyMessage`, provided instead `showAnyMessage`.
* `AnyMessageWithAgency` pattern synonym is exported as a constructor of `AnyMessage`.
* constraints of `prop_*` APIs where changed.

## 0.2.0.0

* Initial version
