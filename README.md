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
completely undocumented, and rather painful to use. JS8Call now has a
TCP API. While it's not REST (we all wish it was), it's noticably than
the old API, though my understanding is that the UDP API is still
supported. The documentation of the API, however, has not
improved. The following is the full documentation of the API: "JS8Call
uses a JSON API offered over UDP and TCP. More detailed documentation
will be available in the future." Yeah, have fun with that.

If you want to dig in, the only viable solution is to download the
JS8Call source and dig in. The majority of API code that you'll need
to grok is in mainwindow.cpp.

There are still some bugs in JS8Call 2.2.0 with respect to the
API. For example, if you change modem speeds using the API instead of
the GUI, the speed does change as requested and works, but the GUI
does not reflect that change, and it gets a little wonky visually. It
continues to appear as though it was in the last mode set in the GUI,
indicating the wrong mode, and with the waterfall interval remaining
at the old setting. There are other bugs, as well, though that's one
of the most obvious.

Note that in it's current form, when an API call is made using this
library, that call is pushed to a queue, which is sent as quickly as
possible. In the mean time, the API call you made returns immediately,
even though what you asked the API to do may not have net
completed. For example, if you change the radio frequency, that change
is queued up and likely sent immediately. And the frequency on your
radio would change pretty much immediately. But not necessarily before
your function call to the API returns. On the other hand, if you send
a long block of text to be transmitted, that call will also return
immediately, even though the radio might be busy sending that message
for the next three minutes. Another example is fetching the radio
frequency. The result comes back as a chunk of JSON that gets decoded
and added to *rx-q*, you do not actually get a frequency returned from
your call. This is an inherent limitation of the JS8Call API
implementation. I'll probably end up adding callbacks to return the
data that was requested, but that's for a future version. For now, you
have to find and process your own return messages.

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
a receive queue. You can view the stream in real time with
(watch-traffic t):

```
JS8CALL> (watch-traffic t)
Type: RX.DIRECTED
Time: 2021-02-25T10:44:12.000000-07:00
Value: KF0AER: WA6HZT HEARTBEAT SNR -10 ♢
ID: -1
Dial Freq: 7078.000 khz
Offset: 959 hz
TX Freq: 7078.959 khz
SNR: -11
Speed: NORMAL
Drift: 200 ms
Cmd: HEARTBEAT SNR
Extra: -10
From: KF0AER
To: WA6HZT
Text: KF0AER: WA6HZT HEARTBEAT SNR -10 ♢

Type: RX.SPOT
Time: 2021-02-25T10:44:12.000000-07:00
ID: -1
Dial Freq: 7078.000 khz
Offset: 959 hz
TX Freq: 7078.959 khz
Call: KF0AER
Grid: DM79
SNR: -11

...
```

Jeff/N0GQ
