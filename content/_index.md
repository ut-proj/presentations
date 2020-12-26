+++
title = "Example LFE Presentation"
outputs = ["Reveal"]

[logo]
src = "logo-v6.svg"
+++

# Sound on BEAM: Music in the Land of Distributed Lisp

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Overview

* Background, sound in the digital world, etc.
* Homage: Joe Armstrong's explorations
* Extempore vs. SuperCollider
* Lisp Flavoured Erlang 2.0
* Extempore in OTP
* Making Music

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Background (me)

<ul>
<li class="fragment">4 yo - Messing about with Piano</li>
<li class="fragment">8 yo - Started formal piano lessons, casual guitar </li>
<li class="fragment">16 yo - Performed in a chamber music group</li>
<li class="fragment">22 yo - Assembled a home recording studio</li>
<li class="fragment">25 yo - Stopped playing music, replaced with physics / maths studies</li>
<li class="fragment">48 yo - Started playing again</li>
</ul>

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Background (digital music)

<ul>
<li class="fragment">Early 1980s - Musical Interface Digital Interface (MIDI) standard created</li>
<li class="fragment">1990s - Digital music recording took off</li>
<li class="fragment">2000s - Recording on regular PCs</li>
<li class="fragment">Early 2010s - Open Sound Control (OSC) standard created </li>
<li class="fragment">2010s - Digital Audio Workstation (DAW) software, tube/analog emulation, live-coding</li>
</ul>

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Erlang & Sound

<ul>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
</ul>

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Extempore vs. SuperCollider

<ul>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
<li class="fragment">XXX</li>
</ul>

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## undertone

What is it good for?

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Architecture

[diagram of connected pieces]

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Intermission

<img src="Let's_All_Go_to_the_Lobby.jpg" style="width: 80%" />

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---


---

## Review

* {{< fa check-square >}} Background, sound in the digital world, etc.
* {{< fa check-square >}} Homage: Joe Armstrong's explorations
* {{< fa check-square >}} Extempore vs. SuperCollider
* Lisp Flavoured Erlang 2.0
* Extempore in OTP
* Making Music

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

{{< slide background-image="LFE-logo-darker-greys-0.05trans-6-square-x3000.png" >}}

## LFE

What is it?

```erlang
ackermann(0, N) ->
  N+1;
ackermann(M, 0) ->
  ackermann(M-1, 1);
ackermann(M, N) when M > 0 andalso N > 0 ->
  ackermann(M-1, ackermann(M, N-1)).
```

```clj
(defun ackermann
  ((0 n) (+ n 1))
  ((m 0) (ackermann (- m 1) 1))
  ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))
```

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

## Don't Panic

[//]: Speaker-Notes:
{{% note %}}
Don't forget your towel when you leave.
{{% /note %}}

---

## Q & A Time

[//]: Speaker-Notes:
{{% note %}}
You're a bunch of really loopy froods. What can I say?
{{% /note %}}

---

#### Contact

* {{< fa envelope >}} oubiwann@gmail.com
* {{< fa fab twitter-square >}} [@oubiwann]()
* {{< fa fab linkedin >}} [linkedin.com/in/oubiwann]()
* {{< fa fab twitter-square >}} [@forgottentones]()
* {{< fa fab soundcloud >}} https://soundcloud.com/forgotten-tones/tracks

[//]: Speaker-Notes:
{{% note %}}
Here's how you can reach me ...
{{% /note %}}

---

#### LFE Resources

* {{< fa globe >}} https://lfe.io/
* {{< fa fab github-square >}} https://github.com/rvirding/lfe
* {{< fa fab slack >}} [lfe.slack.com]()
* {{< fa users >}} http://groups.google.com/group/lisp-flavoured-erlang
* {{< fa fab twitter-square >}} [@ErlangLisp]()

[//]: Speaker-Notes:
{{% note %}}
Here's where stuff is ...
{{% /note %}}

---

#### undertone Resources

* {{< fa fab github-square >}} https://github.com/ut-proj/undertone
* {{< fa fas book >}} https://undertone.lfe.io/book
* {{< fa fab slack >}} [lfe.slack.com]() #algo-sound
* {{< fa users >}} http://groups.google.com/group/lfe-undertone
* {{< fa fab twitter-square >}} [@lfeundertone]()
* {{< fa fab instagram >}} https://www.instagram.com/lfeundertone/


[//]: Speaker-Notes:
{{% note %}}
Here's where stuff is ...
{{% /note %}}
