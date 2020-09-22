# js8call-lisp
### _Jeff Francis <jeff@gritch.org>_

Note that this is very much a work in progress. While it does work in
it's current state, it is not yet complete, nor is it 100%
tested. There's a great deal of work yet to be done on both the code
and the documentation. But if somebody wants to play with it, it's
available.

This is a Common Lisp library to talk to JS8Call (the ham radio
communications package). Historically, JS8Call had a UDP API. Earlier
versions of this code supported that API. While it did work, it was
completely undocumented, and horrifically painful to use. JS8Call now
has a TCP API. While it's not REST (we all wish it was), it's far
better than the old API, though my understanding is that the UDP API
is still supported. The documentation of the API, however, has not
improved. The following is the full documentation of the API: "JS8Call
uses a JSON API offered over UDP and TCP. More detailed documentation
will be available in the future." Yeah, have fun with that.

If you want to dig in, the only viable solution is to download the
JS8Call source and dig in. The majority of API code that you'll need
to grok is in mainwindow.cpp.

There are still some bugs in JS8Call with respect to the API. For
example, if you change modem speeds using the API instead of the GUI,
the speed does change as requested and works, but the GUI does not
reflect that change. It continues to appear as though it was in the
last mode set in the GUI, indicating the wrong mode, and with the
waterfall interval remaining at the old setting. There are others, as
well, though that's one of the most obvious.

Note that in it's current form, when an API call is made using this
library, that call is pushed to a queue, which is sent as quickly as
possible. In the mean time, the API call returns immediately, even
though the function may or may not have completed. For example, if you
change the radio frequency, that change is queued up and likely sent
immediately. And the frequency on your radio would change pretty much
immediately. But if you send a long block of text to be transmitted,
that call will also return immediately, even though the radio might be
busy sending that message for the next three minutes. Another example
is fetching the radio frequency. The result comes back as a chunk of
JSON that gets decoded and added to *rx-q*, you do not actually get a
frequency returned from your call. This is an inherent limitation of
the JS8Call API implementation. I'm looking at methods of working
around this, which will probably involve some ugly kludges involving
threads that watch for responding JSON blobs, processing the returned
data to the user, and of course, timing out if/when data is not
returned. For now, you have to find and process your own return
messages.

## How To

First, load the package and start the connection. (start-server) takes
an optional host and port (defaults to "localhost" and 2442):

```
CL-USER> (ql:quickload :js8call)
To load "js8call":
  Load 1 ASDF system:
    js8call
; Loading "js8call"
.
(:JS8CALL)
CL-USER> (in-package :js8call)
JS8CALL> JS8CALL> (start-server)
#<SB-THREAD:THREAD "js8call" RUNNING {10159D9BD3}>
JS8CALL>
```

Once the server has been started, data slowly starts to accumulate in
a receive queue in the library. The data from JS8Call is all JSON
encoded, so the library decodes these into Lisp structures. To pull
the accumulated data from the queue, use (dump-rx):

```
JS8CALL> (dump-rx)
(((:PARAMS (:+NAME+ . "JS8Call") (:+UTC+ . 1545493329718)
   (:+VERSION+ . "0.11.0-devel"))
  (:TYPE . "PING") (:VALUE . ""))
 ((:PARAMS (:+NAME+ . "JS8Call") (:+UTC+ . 1545493344717)
   (:+VERSION+ . "0.11.0-devel"))
  (:TYPE . "PING") (:VALUE . "")))
JS8CALL>
```

If you just want to watch data as it comes in, run (watch-traffic t).

Jeff/N0GQ
