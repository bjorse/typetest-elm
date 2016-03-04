# Typing test

This is my first Elm project on GitHub. It's a simple typing test project. It's not finished yet though, so expect bugs if you actually try it out.

The (known) limitations so far:

  * 1000 word limit (and filtered on words above 2 characters, so even less than that)

  * No good handling when words run out (words just stop to appear)

  * ~~No actual ending on the contest, time keeps ticking~~ (fixed)

  * The time ticking is happening all the time in the background, so if you're unlucky you'll miss out of a big part of a second when you start typing. Some signal to the top to start the ticking should be implemented.

  * Backspace is not working. It seems some hijacking in JavaScript is needed to get it to work. So, if you mistype a word you'll have to skip it by pressing space.

  * Damn ugly GUI (not intended to improve though)

## The future

The idea is to finish most of the issues stated above as a good learning practice. Any suggestions would be appreciated!

When the above is finished, these things should be fixed:

  * ~~Show a summary when the game has ended (WPM and so on)~~ (fixed, updated continously throughout the game)