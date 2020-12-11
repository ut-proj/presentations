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
  
(io:format "~p~n" `(,thing))
```

[//]: Speaker-Notes:
{{% note %}}
{{% /note %}}

---

{{< slide background-image="LFE-logo-darker-greys-0.05trans-6-square-x3000.png" >}}

## Slide 3ree.1

Let's do a deep dive, here ...


[//]: Speaker-Notes:
{{% note %}}
Deep dive notes.
{{% /note %}}

---

{{< slide background-image="LFE-logo-darker-greys-0.05trans-6-square-x3000.png" >}}

## Slide 3ree.2

So about this Ackermann thing ...

[//]: Speaker-Notes:
{{% note %}}
Lots more details.
{{% /note %}}

---

{{< slide background-image="LFE-logo-darker-greys-0.05trans-6-square-x3000.png" >}}

## Slide 3ree.3

Oh, and the lambda calculus!

Alight, I think we're done here.

[//]: Speaker-Notes:
{{% note %}}
Really starting to ramble, now ...
{{% /note %}}

---

## Slide 4our

Some maths:

$$e^{i \pi} + 1 = 0$$

[//]: Speaker-Notes:
{{% note %}}
Who doesn't like math?!
{{% /note %}}

---

{{< slide transition="convex" >}}

## Slide 5ive

That was a convex transition ...

[//]: Speaker-Notes:
{{% note %}}
Hey, a transition!
{{% /note %}}

---

{{< slide transition="concave" >}}

## Slide 6ix

That was a concave transition ...

[//]: Speaker-Notes:
{{% note %}}
Another transition!
{{% /note %}}

---

{{< slide transition="fade" >}}

## Slide 7even

That was a fade transition ...

[//]: Speaker-Notes:
{{% note %}}
Another transition!
{{% /note %}}

---

{{< slide transition="zoom" >}}

## Slide 8ight

That was a zoom transition ...

[//]: Speaker-Notes:
{{% note %}}
Another transition!
{{% /note %}}

---

## Slide 9ine

A quote:

> "A thing. About stuff."

-- by Someone

[//]: Speaker-Notes:
{{% note %}}
Another transition!
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

* email: name@domain
* Twits: @username
* Slack: lfe.slack.com
* LinkedIn: linkedin.com/in/NAME

[//]: Speaker-Notes:
{{% note %}}
Here's how you can reach me ...
{{% /note %}}

---

#### Resources

* LFE site: https://lfe.io/
* Mail list: http://groups.google.com/group/lisp-flavoured-erlang
* LFE Twits: @ErlangLisp
* Project site: https://github.com/ORG/NAME

[//]: Speaker-Notes:
{{% note %}}
Here's where stuff is ...
{{% /note %}}
