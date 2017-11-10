# Tandy 64

A intro for the Tandy 1000 HX (and compatible) computers.

<img src="https://lh3.googleusercontent.com/6gGmjaQE3ZHaLAalGV1BsGPk8B-4j2vVImCuzMSSgRApg5lI9a5-hgmN2O2YCv2T35kOFsGOhVIl4MapYFjgvA5pavmqO6TRw54eXFlIO5-oBkzfzHFeNroG_xyQPqjxGBrBaPQ3tBY">


## Requirements

* Tandy 1000 HX (or compatible)
* 256Kb RAM (it could fit in 128Kb with some minor changes)


## How to compile it

* Install [nasm][0]
* Install [alink][1]
* (Optional) Install [Python 3][2]
* (Optional) Install [DosDox-x][3]
* (Optional) Install [upx][4]


And do:

    $ make


To run it in the DosBox-x emulator, do:

    $ make run


## Resources

The `/res` folder has the music, graphics and fonts in their native formats.
And in the `/tools` folder are the scripts to convert them to a more optimized
format for the Tandy.

To convert the resources to native do:

    $ make res

## Source code structure

The `/src` folder contains the source code and all converted resources.
There are 4 `.asm` files:

*   `main.asm`: setups the stack and calls the pre_intro, intro and post_intro in order
*   `pre_intro.asm`: shows the C64 loading screen
*   `intro.asm`: Everything happens here. The code is somewhat messy. There a few state machines to control and sync the different actions. Some pointers:
    * `plasma_tex_*`: refers to "plasma textures", the ones that appear at the very beginning
    * `plasma_*`: (without the `_tex` suffix): refers to the plasma animation.
    * `new_i08_*`: there are like 3 different interrupt handlers. They are used in at different parts
    * `boy_anim_*`: code related to the guy walking/dancing
    * `text_writter_*`: code related to the "cursor writting text"
    * `letter_state_*`: code related to the PVM letters animation
    * `state_*` / `main_state_*`: "main" state machine code
    * `scroll_*`: scroller code
    * `raster_bars_*`: raster bar effect code
*   `post_intro.asm`: the final C64 screen


[0]: http://nasm.us/
[1]: https://github.com/ricardoquesada/alink
[2]: https://www.python.org/downloads/
[3]: http://dosbox-x.com/
[4]: https://upx.github.io/
