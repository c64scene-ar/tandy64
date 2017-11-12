# Tandy 64

A intro for the Tandy 1000 HX (and compatible) computers.

[![Tandy 64 Intro](https://img.youtube.com/vi/M2X7e9KnoLk/0.jpg)](https://www.youtube.com/watch?v=M2X7e9KnoLk)


## Requirements

* Tandy 1000 HX (or compatible)
* Tested with 256Kb RAM, but could fit in 128Kb with some minor changes


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

## Tools

In `tools` folder you will find:

*   `convert_vgm_to_pvm.py`: Converts VGM music format to PVM (Plays VGM Music) music format
    *   Almost the same as VGM but uses Run-leght encoding. As an example, the music that we use
        takes ~78K in VGM format, and around ~48K in PVM format. It is good to have it in less than 64k (1 segment)
*   `convert_gfx_to_bios_format.py`: Converts graphics to Tandy format.
*   `convert_anim_to_frames.py`: Quick hack to dump animation frames in tandy format

We also used:

*   [plasma_generator.py][5]: to visually build the plasma effects
*   [Deflemask][6] for the music
*   [VChar64][7] for the charset


# Lessons learned

*   Next time, we should support composite instead of RGBI


[0]: http://nasm.us/
[1]: https://github.com/ricardoquesada/alink
[2]: https://www.python.org/downloads/
[3]: http://dosbox-x.com/
[4]: https://upx.github.io/
[5]: https://github.com/ricardoquesada/c64-misc/blob/master/tools/plasma_generator.py
[6]: http://deflemask.com/
[7]: https://github.com/ricardoquesada/vchar64
