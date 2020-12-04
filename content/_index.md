+++
title = "Example LFE Presentation"
outputs = ["Reveal"]

[logo]
src = "LFE-logo-6-square.svg"

[reveal_hugo.templates.green]
background = "#424242"
transition = "convex"

+++

# Example LFE Presentation

[//]: Speaker-Notes:
{{% note %}}
Welcome to the demo presentation!
{{% /note %}}

---

## Slide Two

Animating Bullets:

<ul>
<li class="fragment">Point 1 (Fade in)</li>
<li class="fragment fade-out">Point 2 (Fade out)</li>
<li class="fragment highlight-red">Point 3 (Highlight red)</li>
<li class="fragment fade-in-then-out">Fade in, then out</li>
<li class="fragment fade-up">Slide up while fading in</li>
</ul>

(But can be used with more than just bullets ...)

[//]: Speaker-Notes:
{{% note %}}
I'd like to point out that these are really interesting points.
Pretty much on-point.
{{% /note %}}

---

[//]: Begin-Vertical-Slides

{{% section %}}

[//]: WARNING:-Speaker-notes-don't-work-in-vertical-slides-:-(

## Slide Three

Some highlighted LFE code:

```clj
(defun ackermann
  ((0 n) (+ n 1))
  ((m 0) (ackermann (- m 1) 1))
  ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))
  
(io:format "~p~n" `(,thing))
```

[//]: Speaker-Notes:
{{% note %}}
Who doesn't like code?!
{{% /note %}}

---

## Slide Three.1

Let's do a deep dive, here ...


[//]: Speaker-Notes:
{{% note %}}
Deep dive notes.
{{% /note %}}

---

## Slide Three.2

So about this Ackermann thing ...

[//]: Speaker-Notes:
{{% note %}}
Lots more details.
{{% /note %}}

---

## Slide Three.3

Oh, and the lambda calculus!

Alight, I think we're done here.

[//]: Speaker-Notes:
{{% note %}}
Really starting to ramble, now ...
{{% /note %}}

{{% /section %}}

[//]: End-Vertical-Slides

---

## Slide Four

Some maths:

$$e^{i \pi} + 1 = 0$$

[//]: Speaker-Notes:
{{% note %}}
Who doesn't like math?!
{{% /note %}}

---

{{< slide transition="convex" >}}

## Slide Five

That was a convex transition ...

[//]: Speaker-Notes:
{{% note %}}
Hey, a transition!
{{% /note %}}

---

{{< slide transition="concave" >}}

## Slide Six

That was a concave transition ...

[//]: Speaker-Notes:
{{% note %}}
Another transition!
{{% /note %}}

---

{{< slide transition="fade" >}}

## Slide Seven

That was a fade transition ...

[//]: Speaker-Notes:
{{% note %}}
Another transition!
{{% /note %}}

---

{{< slide transition="zoom" >}}

## Slide Eight

That was a zoom transition ...

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
