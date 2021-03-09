+++
title = "Sound on BEAM: Music in the Land of Distributed Lisp"
outputs = ["Reveal"]

[logo]
src = "logo-v6.svg"
+++

# Sound on BEAM: Music in the Land of Distributed Lisp

[//]: Speaker-Notes:
{{% note %}}

Hello, and welcome to my talk!

My name is Duncan McGreggor -- today I'm giving the second half of the talk
that I gave at Lambda Days, just a few weeks ago.

{{% /note %}}

---

# ( Part II )

[//]: Speaker-Notes:
{{% note %}}

As with that one, I'm going to share with you a little Lisp Flavoured Erlang,
Extempore, and generative music. 

However, in this talk I will be focusing much more upon the my needs as an
amatuer musician ...

{{% /note %}}

---

## Overview

* Introduction
* undertone and Its Use Cases
* Architecture
* Supervision Trees
* Clients and Servers
* External Processes, Ports, and `exec`
* Languages
* A Custom REPL
* Demo / Performance / Walkthrough
* What's Next for undertone
* Q & A

[//]: Speaker-Notes:
{{% note %}}

... in particular, my need for a reliable soft real-time
system capable of speaking multiple protocols, of creating servers or 
connecting to them and of being able to continue operations, even
as parts of the system encounter unrecoverable errors and must be restarted.

a smattering of OTP, some Extempore, and generative
music -- and how all these come together in the undertone project.

{{% /note %}}

---

## Who am I?

* Prinicpal software engineer
* Life-long hacker (started at age 9 in '81; never stopped)
* Habbitual explorer
* Core contributor to Robert Virding's Lisp Flavoured Erlang (LFE)

[//]: Speaker-Notes:
{{% note %}}

Here's are some superficial bullet points about me ...

{{% /note %}}

---

## Who am I?

* 4 yo - Messing about with Piano
* 9 or 10 yo - Formal piano lessons, casual guitar
* 14 yo - First synthesizer (analog Korg PolySix)
* 16 yo - Performed in a chamber music group
* 25 yo - Stopped music; replaced with physics/maths ... then career
* 48 yo - Started playing again

[//]: Speaker-Notes:
{{% note %}}

... and some more, relating to music.

{{% /note %}}

---


## Re-entering the Musical World

<img src="dm_as_1.jpg" />

{{% note %}}

About 6 1/2 years ago at OSCON 2014, I met Andrew Sorensen -- pictured here -- after his Extempore live-coding performance at OSCON.

(Yeah, that's me -- the one neaest him).

Andrew and I talked about live coding, my interest in using LFE/OTP along with Extempore, and if I remember correctly, he mentioned conversations that he and Joe Armstrong had about Extempore and Erlang and some potential collaboration.

I started using Extempore immediately after Andrew's performnace, when he shared his keynote code with me. I picked it up again last year ...
{{% /note %}}

---

## Re-entering the Musical World

<img src="sound_of_erlang.jpg" /> 

{{% note %}}

... after Aleksander Lisiecki's blog post which covered sound generation in Erlang.

I ported his code to LFE, and that same day, created the undertone project.

---

## Wherefore undertone?

<img src="undertone-logo-v1.svg" width="60%" style="border:none; background: none; box-shadow: none;"/> 

{{% note %}}

undertone came about due to my need to control synthsizers -- both hardware and software -- and run services. I started by reading everything I could on Erlang and music, retracing Joe Armstrong's steps from the mid-2000s up until a few short years ago. 

One of the first features that landed in undertone was support for Open Sound Control (or OSC). This was done so that I could run several of Joe's code samples from within a structured project using a custom backend for SuperCollider.

Ultimately, though, I became frustrated by my admittedly subjective perception that SuperCollider seems to lack an element of musicality -- a feeling I _did_ get from Extempore.

Within a few days I'd added a new backend for undertone, one that allowed me to run Extempore code from LFE. In so doing, I feel like I finished a conversation started between Joe and Andrew years ago.

{{% /note %}}

---

## Wherefore undertone?

<img src="undertone-logo-v1.svg" width="60%" style="border:none; background: none; box-shadow: none;"/> 

{{% note %}}

The real "why" behind undertone isn't a battle of backends ...

the heart of the matter is what it would be used for.

In my practice sessions with guitar and synthesizers, I wanted to be able to quickly write a few lines of code for some ambient backing sounds, or chord progressions against which I could practice scales, or experiment with intervals and counterpoint.

After so long in the software industry, I'm just much, much faster at writing code than sitting
down with a sheaf of blank staves and writing notes.

There is an implicit answer here, to the question of "Why LFE"? : if I'm going to be writing code in my spare time, I need it to be in a language that I love and have fun using.

{{% /note %}}

---

## Wherefore undertone?

* Create music in my preferred language
* Monitoring and automatically restarting OS processes
* Speak to Open Sound Control servers (i.e., controlling faders on software consoles/mixers)
* Potentially host my own OSC servers
* Send TCP packages to the Extempore compiler service
* Automtically reconnect (with backoff support) to required services
* Be able to restart any of these components in the event of partial or complete system failure

{{% note %}}

Above and beyond that, thgough, I needed to be able to control external processes running on the operating system. restarting them as necessary. There are a lot of features listed here, but that one is biggie for me. I did a lot of experimentation with different MIDI drivers, software synthesizers, VST plugin hosts, and the like, and ended up having various applications or their supporting processes, crash.

Sometimes days of work -- where I'd invested my time in a long chain of trial and error -- were lost. All because just one component I'd been experimenting with was unstable or wasn't designed to do the crazy things I was asking of it.

And to that point ...

{{% /note %}}

---

## Wherefore undertone?

Also:

* Maintain multiple, separate state contexts
* Support a familiar workflow (a REPL!)
* Provide basic session management (ETS + a handful of functions)

{{% note %}}

I also needed to manage state in a sane manner. As you are all assuradly well aware, the points above and those in the previous slide are features readily available in OTP or in the BEAM languages which offer Erlang interoperability.

And it is for all these reasons that undertone was born, created in LFE, and built upon the foundation of Erlang and OTP.

{{% /note %}}

---

{{< slide transition="none" >}}

## What is undertone?

<img src="arch/erlang-node-diagram2.jpg" width="80%" style="border:none; background: none;"/> 


{{% note %}}



{{% /note %}}

---

## Architecture

<img src="arch/system-context.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}

So, here's a view of the system you just saw demoed, represented with the C4 style of architectural diagrams. Don't worry though: we'll just look at two of the four ;-)

Lest you get the wrong idea, only a small portion of this diagram is undertone. 

And I should point out that the hard work of sending MIDI messages (or playing its own sounds, as the case may be), is all done by Extempore -- Andrew Sorensen's open source live-coding software. 

{{% /note %}}

---

## Architecture

System context for Extempore:

* talks to the OS / routes MIDI
* signals routed to external devices
* also routed to MIDI in the DAW (e.g., software synthesizers)

[//]: Speaker-Notes:
{{% note %}}
Extempore runs a TCP server to which we can connect, and then from there, gain access to the operating system's audio layer, MIDI devices, and the like.
{{% /note %}}

---

## Architecture

System context for undertone:

* uses Erlang (starts up supervision tree, clients, servers)
* talks to Extempore (bitstrings over TCP)
* controls OSC servers (e.g., DAWs)

[//]: Speaker-Notes:
{{% note %}}
undertone spawns Extmpore as a managed OS process, starts a TCP client, and optionally starts Open Sound Control clients and servers.

All of this gets stuffed into a supervision tree that will restart these components, should you push them beyond their limits -- without crashing the Erlang VM.

{{% /note %}}

---

## Architecture

<img src="arch/system-context.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
Now, if we zoom in ...
{{% /note %}}

---

## Architecture

<img src="arch/system-context-bevin.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
Now, if we zoom in ...
{{% /note %}}

---

## Architecture

<img src="arch/system-context.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
Now, if we zoom in ...
{{% /note %}}

---

{{< slide transition="none" >}}

## Architecture

<img src="arch/system-context-zoom.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
... on undertone's purple box, or "container" ...
{{% /note %}}

 
---
 
## Architecture

<img src="arch/containers2.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
We can see a little more of what's going on under the bonnet.

The "container" view of undertone shows which components are connected to each other and how.
{{% /note %}}

---

{{< slide transition="none" >}}

## Architecture

<img src="arch/containers2.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
The grey box in the upper left is where the first demo took place. All the commands entered there were sent via the undertone-managed TCP client to the Extempore compiler server.
{{% /note %}}

---

{{< slide transition="none" >}}

## Architecture

<img src="arch/containers2.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
The grey box in the middle-right is what you saw when looking at the terminal in the second demo:

* where, after pasting all the Extempore code
* we viewed the indexed history of the entered Scheme forms
* and where all the logging output was

That REPL -- while written in LFE -- has its own commands separate from the LFE REPL.

{{% /note %}}

---

{{< slide transition="none" >}}

## Architecture

<img src="arch/containers2.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
Extempore proper, doesn't actually have a REPL of its own -- the standard way of interacting with it is by means of a text editor such as Emacs that's capable of sending Scheme forms to the Extempore TCP server for on-the-fly compilation.

In short: everything within the dashed purple border here has been written in LFE. 
{{% /note %}}

---

{{< slide transition="none" >}}

## Architecture

<img src="arch/containers2.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}
Also, note that the blue "Extempore" box actually represents LFE spawning the Extempore processes and manaing the spawned port and associated state in a gen_server.
{{% /note %}}

---

## Architecture

The undertone "container":

* OTP app with supervisor and state server
* State server for mananging session commands and system config
* OSC clients for any OSC-enabled software running a UDP server
* TCP client for long-running connections to Extempore
* LFE REPL
* Extempore REPL

[//]: Speaker-Notes:
{{% note %}}
And here's a quick textual overview of what we just covered ...
{{% /note %}}

---

## Progress Check

* ✅ Introduction
* ✅ undertone and Its Use Cases
* ✅ Architecture
* Supervision Trees
* Clients and Servers
* External Processes, Ports, and exec
* A Custom REPL
* Demo / Performance / Walkthrough
* What's Next for undertone
* Q & A

{{% note %}}

Quick update on what we've covered so far ...

{{% /note %}}


---

## Supervision Trees

<img src="treebeard-talbot-jenkins.jpg" style="width: 40%" />

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Supervision Trees

<img src="undertone-sup.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Supervision Trees

<img src="undertone-sup-xt.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Supervision Trees

<img src="undertone-sup-bevin.jpg" style="width: 100%" />

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Clients and Servers

* Extempore client
  * TCP client for sending messages to the compiler server
  * Scheme syntax as bitstrings
  * Started by the release

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Clients and Servers

* Open Sound Control clients
  * potentially many
  * connecting to both software and hardware
  * e.g., digital audio workstations (DAWs) to control console faders

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Clients and Servers

* Open Sound Control servers
  * none right now
  * could create an OSC/MIDI bridge in LFE/Erlang 
  * create a custom Raspberry Pi sound device and export OSC methods
* erlsci/osc is UDP only

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Clients and Servers

* `gen_servers` for state management
* The Extempore REPL is a simple looping server
* The undertone backends each have their own `gen_server` that's responsible for managing the backend

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## External Processes & Erlang Ports

* Used to use `erlang:open_port` (`spawn_executable`)
* Switched to `exec` library (which still uses Erlang ports)
* Extempore backend:
  * capturing output from Extempore
* "Bevin" backend:
  * sending MIDI (OS process)
  * receiving MIDI (separate OS process)
  * stdout is captured for both and logged / parsed

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Dependencies

* https://github.com/erlsci/osc
  * Forked from https://github.com/marianoguerra/erlang-osc
  * 5-10 year old code, updated per rebar3 project best practices

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Dependencies

* https://github.com/lfex/tcp-client
  * Originally based upon https://github.com/cabol/tcp_client
  * Rewritten in LFE around `gen_statem` using https://andrealeopardi.com/posts/connection-managers-with-gen_statem/
  * Exponential backoff from  https://github.com/ferd/backoff

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Dependencies

* https://github.com/saleyn/erlexec
  * Addresses issues with terminating OS processes
  * Keeps to the spirit of Erlang's clean Port API
  * Used to manage 2 of 3 backends in undertone

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Progress Check

* ✅ Introduction
* ✅ undertone and Its Use Cases
* ✅ Architecture
* ✅ Supervision Trees
* ✅ Clients and Servers
* ✅ External Processes, Ports, and exec
* Languages
* A Custom REPL
* Demo / Performance / Walkthrough
* What's Next for undertone
* Q & A

{{% note %}}

Quick update on what we've covered so far ...

{{% /note %}}

---

{{< slide background-image="LFE-logo-darker-greys-0.05trans-6-square-x3000.png" >}}

## Erlang & LFE

Basics: a recursive function using pattern-matching in the function heads.

#### Erlang

```erlang
ackermann(0, N) ->
  N+1;
ackermann(M, 0) ->
  ackermann(M-1, 1);
ackermann(M, N) when M > 0 andalso N > 0 ->
  ackermann(M-1, ackermann(M, N-1)).
```

#### LFE

```clj
(defun ackermann
  ((0 n) (+ n 1))
  ((m 0) (ackermann (- m 1) 1))
  ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))
```

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Erlang & LFE

OTP: Erlang `supervisor`

```erlang
-module('undertone.sup').
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
init([]) ->
    {ok, {sup_flags(),
          [child('undertone.server', start_link, [])]}}.
    
sup_flags() ->
    #{strategy => one_for_one,
      intensity => 3,
      period => 60}.
```

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Erlang & LFE

OTP: LFE `supervisor`

```clj
(defmodule undertone.sup
  (behaviour supervisor)
  (export
   (start_link 0)
   (init 1)))
   
(defun start_link ()
  (supervisor:start_link `#(local ,(MODULE)) (MODULE) '()))

(defun init (_args)
  `#(ok #(,(sup-flags)
          (,(child 'undertone.server 'start_link '())))))
  
(defun sup-flags ()
  `#m(strategy one_for_one
      intensity 3
```

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Extempore

xtlang:

``` scheme
(bind-func AudioBuffer_data_b64
  (lambda (ab:AudioBuffer*)
    (let ((b64size:i64 0)
          (datsize:i64 (* (AudioBuffer_frames ab)
                          (AudioBuffer_channels ab) 4)))
      (String (base64_encode (cast (tref ab 4) i8*)
                             datsize
                             (ref b64size))))))
```

[//]: Speaker-Notes:
{{% note %}}
If that wasn't complicated enough, there are two more languages invloved here as well. This is Extempore's xtlang, which provides low-level access to all aspects of the Extempore system.

{{% /note %}}

---

## Extempore

Scheme:

``` scheme
(sys:load "libs/external/portmidi.xtm")
(pm_initialize)
(define *midi-out* (pm_create_output_stream 3))

(define midi-loop
  (lambda (beat dur)
    (mplay *midi-out*
           (random (list 36 43 48 51 60 60 60 67 70 74 75))
           (random 60 80)
           dur 0)
    (callback (*metro* (+ beat (* .5 dur)))
              'midi-loop
              (+ beat dur)
              dur)))

(midi-loop (*metro* 'get-beat 4) 1/4)
```

[//]: Speaker-Notes:
{{% note %}}
And this is Extempore's higher-level language, a derivative of Tiny Scheme.

This is what most Extempore performers use -- and, in fact, is what you saw me pasting into the REPL at the beginning of the demo.
{{% /note %}}

---

## A Custom REPL

<img src="xt-repl-start.jpg" width="80%" /> 

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## A Custom REPL

<img src="xt-repl-help.jpg" width="80%" /> 

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## A Custom REPL

<img src="xt-repl-sess.jpg" width="80%" /> 

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## A Custom REPL

<img src="xt-repl-defun.jpg" width="100%" /> 

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## Demo!

[//]: Speaker-Notes:
{{% note %}}

TBD

{{% /note %}}

---

## What's Next for undertone?

<img src="undertone-github.jpg" width="80%" /> 

[//]: Speaker-Notes:
{{% note %}}
So what's next for undertone? I have created tickets for efforts such as:

* Deeper support of Extempore in native LFE
* Live capture of music, recorded as LFE data structures
* A Hybrid LFE/Extempore REPL
* Digital Signal Processing from LFE
* More support for open source synthesizers
* Explore the collaboration possibilities with the distributed platform that LFE offers
* Lots of practical use in generating music, exploring music theory ... and just _listening!_
{{% /note %}}

---

## Progress Check

* ✅ Introduction
* ✅ undertone and Its Use Cases
* ✅ Architecture
* ✅ Supervision Trees
* ✅ Clients and Servers
* ✅ External Processes, Ports, and exec
* ✅ Languages
* ✅ A Custom REPL
* ✅ Demo / Performance / Walkthrough
* ✅ What's Next for undertone
* Q & A

{{% note %}}

Quick update on what we've covered so far ...

{{% /note %}}

---

## Q & A

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

#### Contact

* oubiwann@gmail.com
* [@oubiwann]()
* https://soundcloud.com/oubiwann/tracks
* [linkedin.com/in/oubiwann]()
* [@forgottentones]()
* https://soundcloud.com/forgotten-tones/tracks

[//]: Speaker-Notes:
{{% note %}}
Here's how you can reach me ...
{{% /note %}}

---

#### undertone Resources

* https://github.com/ut-proj/undertone
* https://undertone.lfe.io/presentations
* https://undertone.lfe.io/book
* [lfe.slack.com]() #algo-sound
* http://groups.google.com/group/lfe-undertone
* [@lfeundertone]()
* https://www.instagram.com/lfeundertone/

---

#### LFE Resources

* https://lfe.io/
* https://github.com/rvirding/lfe
* [lfe.slack.com]()
* http://groups.google.com/group/lisp-flavoured-erlang
* [@ErlangLisp]()

[//]: Speaker-Notes:
{{% note %}}
Here's where stuff is ...
{{% /note %}}
