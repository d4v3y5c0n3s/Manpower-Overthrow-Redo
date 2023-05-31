# Manpower Overthrow Redo
I'm working on a remake of a game that I made with a friend for a game jam.
### Playability/Status:
WIP, super early development
### Screenshots:
todo
### Build Instructions:
todo, but I'm using Alire & GNAT if you want to take a stab at building it yourself
### Goals:
I plan to work on this game to completion.  I am committed to making this game a reality.  In the process, I'll be working on fixing the auto-generated Allegro library bindings.  The code I work on here I plan on building off of for future projects.
### Notes:
I'm using GCC to auto-generate the bindings to Allegro.  This works fine most of the time, but the generation does have hiccups that require manual fixing.  Usually if there is a problem it can be fixed by removing the "limited" keyword and/or adding a "use" declaration to fix visibility issues.