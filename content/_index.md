+++
title = "Example LFE Presentation"
outputs = ["Reveal"]

[logo]
src = "LFE-logo-6-square.svg"

[reveal_hugo]
theme = "night"
highlight_theme = "atom-one-dark"
margin = 0.2

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

* Point 1
* Point 2
* Point 3

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

{{< slide template="green" >}}

## Slide Five

[//]: Speaker-Notes:
{{% note %}}
Nothing to see here, folks.
{{% /note %}}

---

## Don't Panic

[//]: Speaker-Notes:
{{% note %}}
Don't forget your towel when you leave.
{{% /note %}}

---

## Qs?

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
