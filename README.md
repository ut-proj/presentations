# slide-templates

Use this when giving LFE presentations!

There is a live demo of this sample presentation here:

* [LFE Dark Theme](https://lfe.io/slide-template/dark)
* [LFE Light Theme](https://lfe.io/slide-template/light)
* [LFE Green Theme](https://lfe.io/slide-template/green) (medium)
* [LFE 'Classic' Theme](https://lfe.io/slide-template/classic)

## Step 0

Make sure you have `git`, GNU `make` and `docker` installed.

## Step 1

Clone this repo:

``` shell
$ git clone git@github.com:lfe/slide-templates.git my-preso
```

## Step 2 

Run the server:

``` shell
$ cd my-preso
$ make run
```

Then open up your browser to [localhost:1313](http://localhost:1313).

## Step 5 ... No, 3!

Edit the slides:

* Open up `./content/_index.md` and edit as needed for your LFE presentation.
* If you've still got `make run` running, changes in the Markdown file will auto-load in your browser.

## Themes

This template comes with three themes and may be set by editing `./docker/config.toml`
to comment out the undwanted themes and leave uncommented the desired theme:

``` toml
[params.reveal_hugo]
#custom_theme = "lfe-dark.scss"
custom_theme = "lfe-light.scss"
#custom_theme = "lfe-green.scss"
#custom_theme = "lfe-classic.scss"
```

To get the classic logo, you will need to edit `./content/_index.md` and change the top of the file from this:

``` toml
[logo]
src = "logo-v6.svg"
```

to this:

``` toml
[logo]
src = "logo-v1.svg"
```

The 'classic' theme looks best if the `markup.highlight` style is changed to
`monokailight`.

After editing this file, you will need to stop the local server and re-run
`make run`.

## Resources

To see what you can do with your Markdown slides, see
[these examples](https://github.com/dzello/reveal-hugo#demos).
