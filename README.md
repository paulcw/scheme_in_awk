# scheme_in_awk
A Scheme interpreter written in Awk.

## Is this complete/done?

No.

This is a work in progress, it's nowhere near complete yet.


## Is it a shining example of perfect code for a working professional?

No, it's a thing I'm working on, for fun.  Boring things will be skipped
over, which is bad for professional code that others may work on.

## What is it?

It's a Scheme interpreter, written in Awk.

## Why is it?

- It seemed like an interesting, ridiculous exercise.
- It's been a while since college, so thought it would be interesting
	to get back in the headspace of language implementation and such.
	Mostly I've worked backend application-level web development
	over the years, wanted to play with some other stuff.
- More fun time with LISP-family languages.
- It seems interesting to see what you can do to get around the limitations of awk.
- I was inspired after reading about LISP Machines, something that I
	completely missed out on.  Wondered if a silly implemenation would hit
	similar issues.  It would be nice if I could learn enough about Scheme
	implementation to -- say -- be able to implement an interpreter in Z80
	assembly or something, later, for the next set of laughs.

## How to use it

	$ ./scheme.awk < schemeprogram.scm

or

	$ ./scheme.awk

to use it interactively, but keep in mind that currently it doesn't evaluate
anything until all input is read, so "interactively" just means you don't
type your expressions in until runtime.  Remember to set it executable,
otherwise you can do `awk -f scheme.awk` instead.

## Approach

Did a first pass based on basically just what I remembered from college and
whatever I've done with Scheme over the years, which isn't much.
Got a minimal implementation from that.

Then, going through the book "The Little Schemer", to get use cases to test
this with, and add new functionality, occasionally checking the language spec.
I saw classmates reading this book back in college, although we never used it
for any of my classes.

Later may go through the language spec more formally to round off any sharp
edges, but at that point it would probably be well past the realm of Fun,
which is the whole point of doing this.

## Scheme versions

I had used a version of Scheme in the past which is several versions out
of date, apparently.  I think I used Scheme5.
Catching up with Scheme7, turns out the report is
for the "small" language, which is apparently small enough to be embeddedable.
It's not clear to me whether this is a refactoring of previous versions
such that Scheme5 would be a superset of Scheme7, or if they're
just trying to isolate extensions to that into something "big", or what.
But anyway, maybe for funsies we can try implementing Scheme7small.
Or maybe I'll just go back to Scheme5 because that's the manual I happen
to have printed out.

## Notes

- There is no garbage collection.
	It's not even clear to me whether garbage collection means anything
	considering that the memory is Awk arrays.  How does one reclaim
	memory in Awk?
- This reads the whole input then parses/evaluates it.
	it would be damn nice if could (appropriately) evaluate it as it goes,
	seeing if there's a complete expression after tokenizing each line.
	Maybe.
- I'm using vertical bar to indicate memory references.
- I notice reviewing the code that I don't exploit the pattern-matching
	capacities of awk much, which seems weird.

## Prior work


I googled to find prior work.  Turns out someone wrote LISP in Awk.
I decided not to get spoilers by looking at it; that would spoil the fun.
No idea if it works or is better or what; maybe I'll compare the two when
this is done.

## Roll your own stacks

So this is fun!  The Awk I'm using (mawk that came with Ubuntu Linux)
has a flag compiled that limits the stack to 1024 frames!
For debugging purposes, apparently.
I discovered this via a (presumably) broken recursive function, but
any working non-trivial program will probably hit that limit pretty quick,
assuming I delegate the Scheme stack to the Awk stack.
So recursion basically won't work once I get to nontrivial program sizes!

Now, I *could*, and in any practical project *would*, just use a better
Awk.  There are other awks available for Linux, and I could get a newer
compilation of mawk (or just compile my own) to get past that limitation.

But if I were being practical I wouldn't do the whole project this way,
or at all.  So, when this raises its ugly head, I'll see about converting
recursion to iteration, rolling my own stack.  I did this once for a simple
function, no idea how hard it will get for more complex ones.
The difficulty is the point here.  Wish me luck.


## Open questions
- It is a more interesting to do things the hard way, or seeing
	how easy it could be to implement if I delegated as much as possible
	to Awk?  Maybe above issue is backwards.
- I don't recall how strongly typed Scheme is (as I recall, not much)
	but more to the point, at what point do we need to identify the
	types of values?  Should I let the user redefine 42 as a function?

## TODO

- The tests aren't much -- they're just Scheme code that the user can
	pipe to the interpreter and see if the output is right.  Arguably
	I should make them proper tests that don't have to be run by hand
	and inspected by eye.  Maybe there's a shell script unit test
	framework?
- Garbage collection, maybe.
- Anything more useful than this.
