#!/usr/bin/awk -f


#############
# MAIN LOOP #
#############


# BEGIN and line blocks just create an array of tokens
BEGIN {
	tokens_idx = 1
}
{
	tokenize_around_special_chars($0);
	# could just use the built-in splitting around whitespace,
	# but this destroys our ability to have strings with embedded spaces
}
END {
	max_tokens = tokens_idx
	#print("tokens:")
	#dump_tokens()

	init_memory()
	commands = parse()
	# TODO "commands" is a terrible name; they're not commands at all


	print("heap:")
	dump_heap()
	print("start:", commands)

	# note that we're being destructive with "commands" here...
	while(commands != NULL) {
		command = car(commands)
		print("COMMAND:")
		display(command)
		print("RESULT IS:")
		display(eval(command))
		print("\n----")
		commands = cdr(commands)
	}
}



##############
# TOKENIZING #
##############

# this part is supposed to turn "(cons 1 2)" into ["(", "cons", "1", "2", ")"]

# this adds "plain tokens", i.e., not metachars, to the token buffer.
# plain tokens are just tokens split with the standard separator
# in FS, and arguably i shouldn't rely on that, but basically whitespace.
function tokenize_whitespace_delim(s,		tmp, i, n) {
	n = split(s, tmp)
	for (i = 1; i <= n; i++) {
		tokens[tokens_idx] = tmp[i]
		tokens[tokens_idx, "lineno"] = NR
		tokens_idx++
	}
}

function tokenize_around_special_chars(line,	metachar, metachar_pos) {
	# tokenize each full line
	# it should produce a series of tokens, which are (, ), ', string
	# literals, and everything else (which should be whitespace delimited)
	# comments are removed 
	# TODO r5rs: "[ ] { } | are reserved for future extension"
	#      - arguably I should throw an error if those are detected in input
	#		 at least outside of strings.  Note that I use | in this code.

	# look for "metachars", characters with special importance to
	# tokenizing, and react accordingly.
	# the regexp is of course hard to read, it looks for ( ) ' " ; |
	# not using split() because there's apparently no way to capture the matches
	while (match(line, /([\(\)\'\"\;\|])/)) {
		metachar_pos = RSTART
		metachar = substr(line, metachar_pos, 1)

		# deal with whatever's before the metachar
		tokenize_whitespace_delim(substr(line, 1, metachar_pos - 1))

		# now deal with the metachar and whatever's after it
		if (metachar == "(" || metachar == ")" || metachar == "'") {
			# just push onto token queue and proceed
			# we'll deal with these specially later, during parsing.
			tokens[tokens_idx, "lineno"] = NR
			tokens[tokens_idx++] = metachar
			line = substr(line, metachar_pos + 1)
		} else if (metachar == ";") {
			# it's a comment, ignore to end of line
			line = ""
		} else if (metachar == "\"") {
			# look for closing quote
			if (match(substr(line, metachar_pos + 1), /\"/)) {
			tokens[tokens_idx, "lineno"] = NR
				tokens[tokens_idx++] = substr(line, metachar_pos, RSTART + 1)
				line = substr(line, metachar_pos + RSTART + 1)
			} else {
				ERROR = "we don't support multi-line strings yet" # TODO add
				exit(1) # this only breaks the per-line loop...?
			}
		} else if (metachar == "|") {
			print("You can't use vertical bar outside of strings; line", NR)
			exit(1) # TODO this doesn't seem to actually exit, check docs
		} else {
			print("shouldn't happen but here we are")
			exit(1)
		}
	}
	tokenize_whitespace_delim(line)
}

# to show tokens, for debugging
function dump_tokens() {
	for (i = 1; i < max_tokens ; i++) {
		print(i, ": ", tokens[i])
	}
# to debug the debugging:
#  for (x in tokens) {
#    print(x, ":", tokens[x])
#  }
}


function next_token() {
	LINENO = tokens[token_idx, "lineno"]
	return tokens[token_idx++]
}
function end_of_tokens() {
	return token_idx > max_tokens
}
function token_error() {
	print("Error at line",  tokens[tokens_idx, "lineno"])
}



#######################
# "MEMORY MANAGEMENT" #
#######################

# The goal here is to have functions to deal with "memory management",
# although calling it that is awfully flattering for what it'll probably
# end up being.  It's more about just isolating the behavior in question,
# to make it easier to control and understand.
#
# Because the memory management here is stupid, there is a global reference
# to the next available spot in memory.
# The "memory" is just a big map; it's called "HEAP" out of a sense of
# tradition.  There is no garbage collection; I'm not sure that's even
# meaningful on an awk map.  There is no allocation optimization.
#
# For now I'm going to make a convention/assumption: if you need memory,
# you need a SINGLE POSITION (not sure how true this will be), so you
# take it from this global then immediately increment it.

function init_memory() {
	# sets global variables we need for memory
	NULL = "|0"
	# I created NULL to point to the zeroth memory location,
	# but there was really no need to do it like this, just force
	# of habit really. TODO maybe change the value for clarity?

	HEAP_IDX = 1 # first available memory location.
}

function get_memory_cell() {
	return sprintf("|%d", HEAP_IDX++)
}

function store_cdr(loc, val) {
	HEAP[loc, "cdr"] = val
}

function store_car(loc, val) {
	HEAP[loc, "car"] = val
}

# ^^ these are basically like the bang funcs (set-cdr! or whatever it is)
# and arguably they can be repurposed as such.  TODO

function car(loc) {
	return HEAP[loc, "car"]
}

function cdr(loc) {
	return HEAP[loc, "cdr"]
}

# the first two args are normal scheme, the 3rd is only meant to be
# used in parse functions, to store the line number.
# TODO review, maybe i should have better names?
function cons(first, rest, debuginfo,		loc) {
	loc = get_memory_cell()
	store_car(loc, first)
	store_cdr(loc, rest)
	if (debuginfo) {
		DEBUG[loc] = debuginfo
	}
	return loc
}

function is_pair(thing,		sigil) {
 	if (thing == NULL) {
		return 0
	}
	sigil = substr(thing, 1, 1)
	#print("DEBUG: thing is:", thing, "; sigil is:", sigil)
	if (sigil == "|") {
		return 1
	}
	return 0
}

# to show heap, for debugging
function dump_heap(		loc) {
	for (i = 1; i < HEAP_IDX ; i++) {
	  	loc = sprintf("|%d", i)
	  	printf("%d\t:\t%s\t:\t%s\n", i, HEAP[loc, "car"], HEAP[loc, "cdr"])
	}
}




###########
# PARSING #
###########

# take the token list and turn it into some complete list expressions
# that can then be evaluted.

# parse takes the global token list (which is a global awk list)
# and parses it, creating scheme objects in memory (which is also
# a global awk list).  This means that it probably creates a list
# in memory, since mostly you do lists in Scheme, but it's moving
# this list from awk to Scheme, if that makes sense.

# The main functions to do this go through the token list, consuming
# them, which I refer to as "eating."  The salient point there is that
# you can't go backwards.

# I try to preserve line number information, but it's not always
# clear where a meaningful line number is.  Maybe that's always the
# case.  Anyway, I think this would be a lot easier if I could create
# an object with the token and the line number built into it, but
# barring that, just trying to grab the line number meaningfully and store
# it when I can.

function parse(		commands) {
	# TODO as above, "commands" is a terrible name, that I should change
	# for some reason i was imagining a command line

	#dump_tokens() # for debugging

	# to make this easier we'll imagine the commands as basically
	# in an invisible top-level list.
	# to parse them, we're going to stick a ) at the end then just use eat_list.
	# eat_list should consume the ).
	tokens[tokens_idx++] = ")"

	token_idx = 1 # reset to start of list

	commands = eat_list_expression()

	# if there's anything left over, it's an error
	if (!end_of_tokens()) {
		print("Extra stuff at end of command sequence")
		print("token index:", token_idx, "max index:", max_tokens)
		dump_tokens()
		token_error()
		exit(1)
	}

	return commands
}



# TODO the names 'eat expression' and "eat list expression"
# are kind of misleading.  We don't know if they're expressions yet.
# it's that we *expect* them to be a list of expressions, and sometimes,
# the expression is a list expression.
# maybe "eat item" and "eat pair" or even "eat pair" and "eat nonpair".
# or "complete pair" and "single"?  wtf


# grabs one expression's-worth of tokens, and returns a Scheme object
# of some kind or another.  it may return a ref to a parse tree of more
# objects, if it parses a list
function eat_expression(	token, lineno) {

	token = next_token();
	lineno = LINENO # save this before future parsing increments

	#print ("DEBUG: token is:", token, "; tokexidx is now:", token_idx)
	if (token == "(") {
		# it's a list expression
		return eat_list_expression()
	}

	if (token == "'") {
		return cons("quote", cons(eat_expression(), NULL), lineno)
	}

	# if it's not a pair or a quote, the token is a single scalar thing
	# that operates as a single expression.  Like, for example: 3
	# i have faith in scheme that this statement is true, now watch me be wrong.
	# just return it.
	return token
}


# it's not really a list, it's a pair, which may or may not
# be part of the construction of a list, so this is misnamed
function eat_list_expression(	val, cdrval, lineno) {

	if (end_of_tokens()) {
		print("We should never be at the end of the token list when this is called")
		exit(1)
	}

	lineno = LINENO

	val = eat_expression()

	if (val == ")") {
		return NULL
	}

	if (val == ".") {
		# the following thing should be the cdr of the parent thing
		cdrval = eat_expression()
		lineno = LINENO
		val = eat_expression()
		if (val != ")") {
			print("Malformed expression!  Not a close paren after val after dot operator, line", lineno)
			exit(1)
		}
		return cdrval
	}

	return cons(val, eat_list_expression(), lineno)
}





##############
# EVALUATION #
##############


# eval will almost certainly need to do the same no-recursion thing
# that display() does.  Parsing too probably.

function eval(thing,	op, args) {

	# My sample repl will do this when trying to eval an empty list
	# TODO confirm that this is correct behavior.
	# note that (eval 3) returns 3, but (eval '()) is an error.
	if (thing == NULL) {
		print("Ill-formed expression: trying to exec empty list")
		exit(1)
	}

	if (!is_pair(thing)) {
		# for now, anything that's not a string, evaluates to itself
		return thing
		# TODO later, I'll have to determine the type, and if it's a
		# primitive (like a number or boolean) (and I don't know if "primitive"
		# is even Scheme terminology) it can evaluate as that type, otherwise
		# we'll have to do a symbol lookup, to see if it's bound to anything
		# (and it's an error if it isn't).
		# but today is not that day
	}

	# if it's a non-empty list, get the first element as an operator
	op = car(thing)
	# re: ^^^ at some point, i'd do:
	#	- the value must be an atom
	#	- look up the atom as a symbol, see if a func is bound to it
	# but it's all hardcoded for now

	args = cdr(thing)
	# next, if not a quote we DESTRUCTIVELY go through the list of args,
	# evaluating them (replacing expressions with their values). TODO
	# in the future, I expect to find that this won't work and we'll
	# need to make copies.
	if (op != "quote") {
		# we don't recursively evaluate the args inside a quoted thing.
		# that misses the point of quoting
		eval_args(args)
	}

	# for now, we only execute builtins.  in the future i expect the
	# next line will only be a fallback.
	return builtins(op, args)
}


# this evaluates the individual arguments, before passing what
# will hopefully be a list of primitives to the operator above.
# It's destructive, destroying the list!  It will probably eventually
# need to be a copy.
function eval_args(list) {
	if (list != NULL) {
		store_car(list, eval(car(list)))
		eval_args(cdr(list))
	}
}


function builtins(op, list) {

	if (op == "print") {
		display(list) # TODO this will necessarily print a list (of the args)
						# so for ex "(print 3)" will print "(3)" not "3".
						# it may be more intuitive to iterate through the list
						# and print each.

	} else if (op == "+") {
		if (list == NULL) {
			return 0
		}
		return car(list) + builtins(op, cdr(list))
		# TODO this feel inelegant, having to go through the operator
		# lookup again.  can i clean this up?  like having a separate builtin
		# add func that can loop and/or recurse itself?

	} else if (op == "quote") {
		# quote takes a single argument, which is the thing being
		# quoted (which means in particular, not executed or expanded)
		if (!one_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		return car(list)

	} else if (op == "cons") {
		if (!two_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		return cons(car(list), car(cdr(list)))

	} else if (op == "car") {
		if (!one_elt_list(list) || car(list) == NULL || !is_pair(car(list))) {
			print("wrong number of /bad arguments line", DEBUG[list])
			exit(1)
		}
		return car(car(list))

	} else if (op == "cdr") {
		if (!one_elt_list(list) || car(list) == NULL || !is_pair(car(list))) {
			print("wrong number of /bad arguments line", DEBUG[list])
			exit(1)
		}
		return cdr(car(list))

	} else if (op == "eval") {
		# apparently in real scheme (?), there can be another argument
		# which is an environment specifier... but I'm not going to support
		# that for now TODO add this?
		if (!one_elt_list(list)) {
			print("wrong number of /bad arguments line", DEBUG[list])
			exit(1)
		}
		return eval(car(list))

	} else if (op == "null?") {
		if (!one_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		if (car(list) == NULL) {
			return "#t"
		}
		return "#f"
		# TODO for the benefit of legibility of these predicates,
		# should i define globals TRUE and FALSE?

	} else if (op == "pair?") {
		if (!one_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		if (is_pair(car(list))) {
			return "#t"
		}
		return "#f"

	} else if (op == "atom?") {
		if (!one_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		# I defined this like a builtin but just because it appeared
		# in "The Little Schemer", not sure if it's actually a standard func.
		# TODO I should compare to the language spec.
		if (is_pair(car(list))) {
			return "#f"
		}
		return "#t"

	} else if (op == "eq?") {
		if (!two_elt_list(list)) {
			print("wrong number of arguments, line", DEBUG[list])
			exit(1)
		}
		if (car(list) == car(cdr(list))) {
			return "#t"
		}
		return "#f"

	} else {
		print("unknown op:", op)
		exit(1)
	}
}


function one_elt_list(list) {
	return list != NULL && cdr(list) == NULL
}


function two_elt_list(list) {
	return list != NULL && cdr(list) != NULL && cdr(cdr(list)) == NULL
}



# For display, I originally wrote a sane set of mutually recursive functions 
# they seemed correct, but (the version of) awk (that I'm using) only
# allows 1024 stack frames, which broke things, probably because i had
# a bug that caused a stack overflow error -- it's just that it overflowed
# at a relatively low value.
# it's likely that with real practical scheme code i may have
# deeper stacks in the future, so even without a bug, i may
# run out of stack with this version of mawk.
# i could get a better awk, but for fun, i'm just going to un-recurse display.

# Display throws expressions to stdout.
# TODO maybe rename "thing"
# TODO maybe try pretty-printing, but I don't think it would be easy
# I mean adding better line breaks and indentation.
function display(thing,		stack, stackptr, mode) {

	# push initial place we want to go, to stack
	stackptr = 1
	stack[stackptr, "thing"] = thing
	stack[stackptr, "mode"] = "start"

	do {
		# pop stack to get current values and step
		thing = stack[stackptr, "thing"]
		mode = stack[stackptr, "mode"]
		stackptr--

		if (mode == "start") {
			if (thing == NULL) {
				print("()")
			} else if (!is_pair(thing)) {
				printf("%s", thing)
			} else {
				printf("(")
				# because we're dealing with a stack, we have to
				# give next steps in reverse order
				# we need to push return point, ie close to list
				stackptr++
				stack[stackptr, "thing"] = thing # we won't actually use this...
				stack[stackptr, "mode"] = "closelist"
				# and now, say we need to start processing list
				stackptr++
				stack[stackptr, "thing"] = thing
				stack[stackptr, "mode"] = "startlist"
			}
		} else if (mode == "closelist") {
			print(")")
		} else if (mode == "startlist") {
			# first we do the second step, processing the cdr of the list
			stackptr++
			stack[stackptr, "thing"] = cdr(thing)
			stack[stackptr, "mode"] = "restoflist"
			# then we say what we do next, handle the start of the list
			stackptr++
			stack[stackptr, "thing"] = car(thing)
			stack[stackptr, "mode"] = "start"
		} else if (mode == "restoflist") {
			if (thing == NULL) {
				# do nothing but "return", return to caller,
				# which in this case means "loop again", taking the
				# next step from the stack (we don't add anything)
			} else if (!is_pair(thing)) {
				printf(" . ")
				stackptr++
				stack[stackptr, "thing"] = thing
				stack[stackptr, "mode"] = "start"
			} else {
				printf(" ")
				stackptr++
				stack[stackptr, "thing"] = thing
				stack[stackptr, "mode"] = "startlist"
			}
		} else {
			print("mode got garbled")
			break
		}
	} while (stackptr > 0)
}

