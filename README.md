# gamemaker-risk-of-rain

An unpacking and decompilation of the
[Risk of Rain 1](https://store.steampowered.com/app/248820/Risk_of_Rain/) files.

More specifically I'm referring to `data.win` (if you're using Windows) or
`data.unx` (if you're using Linux).

This package includes:

  * Extraction of every single texture/audio file/string/font/shader the game
    has to offer;

  * Decompilation of the game's interpreted code (stored inside the
    `CODE` chunk and available only on Windows because Hopoo stopped running
    YYC-compiled code which caused
    [crashes](https://riskofrain.fandom.com/wiki/Updates#Patch_v1.2.3)).

Big thanks go out to
[the Undertale datamining squad](https://pcy.ulyssis.be/undertale/),
as without them this would take a year longer than it already did.



## Questions

Q: Can this decompile games other than Risk of Rain 1?

A: I tried like five and all of them seemed to use bytecode 0xE, while
   Risk of Rain 1 uses 0xF (if I had to guess people just don't update because
   updating GameMaker
   [breaks](https://www.reddit.com/r/gamemaker/comments/89sukv/the_latest_update_broke_my_game/)
   the project). There are major structural differences between the two, therefore even the
   naming of the modules in the package is `GameMaker.RiskOfRain.*`. So imo if you wanna
   decompile a different game, handroll another setup, you're most certainly not
   using Haskell either way.

<br>

Q: Why is this in a separate package and not inside a game build?

A: Because I feel like it makes for a nice little unit by itself, so even though
   it is practically useless outside of a game build, it can still exist as a separate
   package. Seeing how I've been experimenting with making a game for the past two years
   and it has been a wild mess of me learning [reflex](http://hackage.haskell.org/package/reflex)
   and getting into the absolute bowels of Haskell with all the type-level stuff,
   I'm not even sure I'll make a working videogame ever, so perhaps someone can use this
   to figure out how to unpack a different game.



## Contact information

The only contact channel I have is Github, at least as of the time of writing this.
