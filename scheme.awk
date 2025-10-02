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
	expressions = parse()

	print("heap:")
	dump_heap()
	print("start:", expressions)

	# note that we're being destructive with "expressions" here...
	while(expressions != NULL) {
		expr = car(expressions)
		print("EXPRESSION:")
		display(expr)
		print("RESULT IS:")
		display(eval(NULL, expr)) # for now, null env means top env
		print("\n----")
		expressions = cdr(expressions)
	}
	# TODO: replace with eval_list, maybe, if it still exists later.
	# decided to keep it separate here so i can wrap that debugging
	# output around it.  later, may do that by wrapping with lambda statements,
	# or more enhanced tracing if such exists.

	# I feel like this main structure is too heavily weighted to the END loop.
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
		metachar_pos = RSTART # TODO why didn't i want to use index() here?
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

# TODO I've really hand-waved the nesting of environments.
# basically I just make a big linked lists of k/v pairs.
# each of which end by pointing to the next highest level.
# there's no sense of a clear delination of levels, which can
# be important in things like "define" or "set!".  As long as I don't
# use these mutatorish crap it should be safe, I think, but I'm going
# to have to clean this up later.


function init_memory() {
	# sets global variables we need for memory
	NULL = "|0"
	# I created NULL to point to the zeroth memory location,
	# but there was really no need to do it like this, just force
	# of habit really. TODO maybe change the value for clarity?

	HEAP_IDX = 1 # first available memory location.
	# the heap itself is just "HEAP", nothing to initialize

	# not sure about this, but here's the first pass
	# global vars will go into a list called GLOBALS.
	# this will be directly modified when things are added to it.
	# i may have to change this later, it feels like a horrible hack.
	# Note that it isn't used as the tail end of a series of environments,
	# as it was in an previous version.
	# it's a kind of additional environment that can always be searched.
	# So we use it in set/define and lookups, and nowhere else.
	# TODO think about this...
	# Ideally it may be someday initialized with standard library stuff.
	GLOBALS = NULL
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

# TODO I added support for line numbers in input, but I'm really
# loose about actually printing it out on errors.
# I should (a) review what exit() actually does, and (b) get
# consistent about printing out line numbers whenever I call that.



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

function dump_globals(		ptr, kv) {
	ptr = GLOBALS
	print("GLOBALS:")
	while(ptr != NULL) {
		kv = car(ptr)
		printf("name: %s : value: %s\n", car(kv), car(cdr(kv)))
		ptr = cdr(ptr)
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

function parse(		expressions) {
	#dump_tokens() # for debugging

	# to make this easier we'll imagine the expressions as basically
	# in an invisible top-level list.
	# to parse them, we're going to stick a ) at the end then just use eat_list.
	# eat_list should consume the ).
	tokens[tokens_idx++] = ")"

	token_idx = 1 # reset to start of list

	expressions = eat_list_expression()

	# if there's anything left over, it's an error
	if (!end_of_tokens()) {
		print("Extra stuff at end of expression sequence")
		print("token index:", token_idx, "max index:", max_tokens)
		dump_tokens()
		token_error()
		exit(1)
	}

	return expressions
}



# TODO the names 'eat expression' and "eat list expression"
# are kind of misleading.  We don't know if they're expressions yet.
# it's that we *expect* them to be a list of expressions, and sometimes,
# the expression is a list expression.
# maybe "eat item" and "eat pair" or even "eat pair" and "eat nonpair".
# or "complete pair" and "single"?  wtf
# otoh, what the hell else are they?  how strict is the definition
# of "expression"?

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


# TODO eval will almost certainly need to do the same no-recursion thing
# that display() does.  Parsing too probably.

function eval(env, expr,		op, args, ref) {

	# My sample repl will do this when trying to eval an empty list
	# TODO confirm that this is correct behavior.
	# note that (eval 3) returns 3, but (eval '()) is an error.
	if (expr == NULL) {
		print("Ill-formed expression: trying to exec empty list")
		exit(1)
	}

	# booleans and numbers and strings evaluate to themselves
	if (is_boolean(expr) || is_number(expr) || is_string(expr)) {
		return expr
	}

	if (!is_pair(expr)) {
		# must be a variable, look it up to see if it's bound
		ref = find_in_binding_list(expr, env)
		if (ref == NULL) {
			print("Unbound variable:", expr)
			exit(1) # TODO I'm pretty sure all these calls to exit are wrong...
		}
		return car(cdr(ref))
	}

	# at this point, it must be a non-empty list, so get the first element
	# as an operator
	op = car(expr)
	# TODO think of a more generic name,
	# it's not necessarily an operator, it could be syntax, etc.

	args = cdr(expr)

	# handling syntax, this is such a hack.
	# TODO i hate these lists of keywords
	#	but i can't think of a more elegant solution within pure awk,
	#	and using c or gawk extensions feels like a cheat.
	if (op == "define" || op == "set!" || op == "let" || op == "let*" || op == "lambda" || op == "quote" || op == "cond") {
		return syntax(op, args, env)
		# TODO I notice that the repl I'm using as a comparison,
		# calls some of this stuff "macro".  are they actually using
		# macros?  am i thinking about this wrong?
	}
	# interesting note, one thing that differentiates the above
	# and the below (builtins and stored procs) is that the below
	# doesn't take the environment (when handling the op) because
	# they'd be lexically scoped. whereas above, in the 'syntax' stuff,
	# we do need the environment
	# or, maybe the distinction is that we shouldn't evaluate
	# all the arguments before applying function, in the above cases?
	# TODO think about the semantics here

	# assuming whatever else is a normal procedure application,
	# so we evaluate all the args then apply the operator

	# next, go through the list of args, evaluating them in a new list

	args = eval_args(env, args)
	# OK, what I want to do in the future is inline these functions
	# as builtins, executed in a similar way to user-defined functions.
	# but in the meantime, here's another hack:
	# TODO i really hate this (see above), but I don't see a good alternative
	# without leaving awk and getting into a more powerful language, which
	# largely misses the point of this exercise (see slightly less above).
	# i could probably put these keywords in a map to make the test more
	# elegant but that's not much of a solution.
	if (op == "print" || op == "+" || op == "cons" || op == "car" || op == "cdr" || op == "eval" || op == "null?" || op == "pair?" || op == "and" || op == "or" || op == "not" || op == "boolean?" || op == "number?" || op == "string?" || op == "eq?" || op == "dump_globals" || op == ">" || op == "<" || op == "=") {
		return builtins(op, args)
		# note interesting thing: nothing in there needs the env
	}

	# look up the operator to see if it's bound
	ref = eval(env, op)
	# note that the above will end the program if it's unbound.
	# TODO this may be wrong...
	if (ref != NULL) {
		return execute_stored_procedure(ref, args)
	}

	# i think what i really want is for what's now in builtins, to be
	# part of what's "syntax", somehow. the tricky part is knowing
	# how to identify whether something was found.  and then we'll
	# try to look up a binding, and then we'll thow up a "not found"
	# message as final version TODO
	# but note that some things can just go into memory as default
	# funcs, like I'm doing with + now, hopefully.
}


function eval_list(env, list,		expr, result) {
	while(list != NULL) {
		expr = car(list)
		result = eval(env, expr)
		list = cdr(list)
	}
	return result # the return value of a list is the value of the last elt
}


# execute user-defined procedure TODO i should probably rename this
function execute_stored_procedure(proc, args,	formals, env) {
	# note that in a stored procedure, we use the environment as
	# the proc was defined, i.e., it's lexically scoped.
	# we don't take the env as an argument

	if (!is_pair(proc)) {
		print("Not a procedure!")
		exit(1)
	}

	# OK, let's assume a stored command is a list with these parts:
	# - a ptr to the env
	# - an arg list
	# - a list of commands
	# this is not unlike a let expression

	if (proc == NULL || one_elt_list(proc) || two_elt_list(proc)) {
		print("error: mis-defined proc")
		exit(1)
	}

	env = car(proc)
	formals = car(cdr(proc))

	# construct a new env with the arguments in front
	while(formals != NULL) {
		if (is_pair(formals)) {
			if (args == NULL) {
				print("not enough arguments")
				exit(1)
			}
			env = cons(cons(car(formals), cons(car(args), NULL)), env)
			formals = cdr(formals)
			args = cdr(args)
		} else if (is_number(formals) || is_string(formals) || is_boolean(formals)) {
			print("bad parameter: ", formals)
			exit(1)
		} else {
			env = cons(cons(formals, cons(copy_list(args), NULL)), env)
			formals = NULL
			args = NULL
		}
	}
	if (args != NULL) {
		print("too many arguments")
		exit(1)
	}

	# now execute the body of the proc
	return eval_list(env, cdr(cdr(proc)))
}


# i call this "syntax" because I expect it will be used
# to handle syntactical things.
# one interesting thing is that "let" returns values, it's like
# an expression, but "define" and "set!" don't.  maybe these
# should be handled by different funcs? TODO
function syntax(op, args, env,		id, expr, ref, val, bindingdefs, b, newbindings, clause) {
	# only allowing define on the top level
	# for future versions, defines at the start of a block could maybe
	# be turned into an implicit LET* TODO later maybe (i think it will be
	# fine to just skip that, actually, basically it's just syntactic sugar)

	if (op == "define" || op == "set!") {
		if (!two_elt_list(args)) {
			print("wrong number of arguments line", DEBUG[args])
			# I don't think exit does what I expect it to, but I keep calling it TODO fix
			exit(1)
		}
		if (op == "define" && env != NULL) {
			print("Can only use define on top level")
			# TODO using NULL as a flag for the env being at the top
			# env level, sucks.  It can't differentiate between top level,
			# and lower levels that just don't happen to define new vals.
			exit(1)
		}
		# ^^^^ TODO i can get rid of this, i think, if i just make
		# define always add at the end
		# oh, but "at the end" doesn't make sense if the list starts empty.
		# maybe i can hack around that.  but anyway the problem is
		# that redefining the env to start with a new value, doesn't work,
		# if it's not global, because there'd be no way to tell the caller
		# the new env value (if it's not GLOBAL, which is itself global)
		# this is part of the whole env mess that I mention above.
		# I need a cleaner, more formal delineation of these... think about
		# this more when I get to impl'ing letrec.

		id = car(args)
		expr = car(cdr(args))
		ref = find_in_binding_list(id, env)
		if (ref == NULL) {
			if (op == "set!") {
				print(id, "not bound for set!")
				# TODO wait, will find die before we even get here?
				exit(1)
			} else if (op == "define") {
				val = eval(env, expr)
				GLOBALS = cons(cons(id, cons(val, NULL)), GLOBALS)
			}
		} else {
			val = eval(env, expr)
			store_car(cdr(ref), val)
		}

	# r7rs: "In a let expression, the initial values are computed
	# before any of the variables become bound; in a let* expression,
	# the bindings and evaluations are performed sequentially;
	# while in letrec and letrec* expressions, all the bindings are
	# in effect while their initial values are being computed,
	# thus allowing mutually recursive definitions."
	# so for following, the thing that changes is really just the manner
	# of figuring the bindings.  which come to think of it, isn't really
	# all that helpful, because everything else is almost nothing.
	} else if (op == "let") {
		if (args == NULL || one_elt_list(args)) {
			print("error: let but no bindings and/or no body")
			exit(1)
		}
		bindingdefs = car(args)
		newbindings = env
		while (bindingdefs != NULL) {
			b = car(bindingdefs)
			if (!two_elt_list(b)) {
				print("error: bindings need 2 elts")
				exit(1)
			}
			# create a new binding, which is a list, and add it to the
			# front of new list of bindings (which ends by connecting to
			# existing env), but don't execute any of the evaluations
			# in the binding list, using the new binding list
			newbindings = cons(cons(car(b), cons(eval(env, car(cdr(b))), NULL)), newbindings)
			bindingdefs = cdr(bindingdefs)
		}
		return eval_list(newbindings, cdr(args))
	} else if (op == "let*") {
		if (args == NULL || one_elt_list(args)) {
			print("error: let* but no bindings and/or no body")
			exit(1)
		}
		bindingdefs = car(args)
		newbindings = env
		while (bindingdefs != NULL) {
			b = car(bindingdefs)
			if (!two_elt_list(b)) {
				print("error: bindings need 2 elts")
				exit(1)
			}
			# create a new binding, which is a list, and add it to the
			# front of new list of bindings (which ends by connecting to
			# existing env), and use the new binding list in subsequent
			# binding evals
			newbindings = cons(cons(car(b), cons(eval(newbindings, car(cdr(b))), NULL)), newbindings)
			bindingdefs = cdr(bindingdefs)
		}
		return eval_list(newbindings, cdr(args))

	} else if (op == "lambda") {
		# for now, only doing the kind where there's a list of formals
		if (args == NULL) {
			print("error: empty lambda")
			# otherwise i'm going to allow empty bodies in case you want
			# to define no-op funcs; TODO see if this is allowed in spec
			exit(0)
		}

		# a stored command is a list with these parts:
		# - a ptr to the env
		# - an arg list
		# - a list of commands
		# and I think we get all that just by doing the following??
		return cons(env, args)

	} else if (op == "quote") {
		# quote takes a single argument, which is the thing being
		# quoted (which means in particular, not executed or expanded)
		if (!one_elt_list(args)) {
			print("wrong number of arguments line", DEBUG[args])
			exit(1)
		}
		return car(args)

	} else if (op == "cond") {
		if (args == NULL || one_elt_list(args)) {
			print("wrong number of arguments line", DEBUG[args])
			exit(1)
		}
		while(args != NULL) {
			clause = car(args)
			if (!is_pair(clause)) {
				print("not a clause!", DEBUG[args])
				exit(1)
			}
			# Note: I'm not doing that whole "=>" syntax for now TODO
			if (car(clause) == "else") {
				if (cdr(args) != NULL) {
					print("can't have an else that's not last")
					exit(1)
				}
				return eval_list(env, cdr(clause))
			} else if (eval(env, car(clause)) != "#f") {
				return eval_list(env, cdr(clause))
			}
			args = cdr(args)
		}

	} else {
		print("somehow you called this with an unsupported syntax:", op)
	}
}

# TODO i could probably find an is_empty and/or not_empty useful...
# for all those loops through lists...although all it's doing now
# is comparing to NULL, it won't be that big an improvement, just a little
# more self-documenting...


# It occurs to me that in a way all of these ^^ are modifications
# of the environment, and then a succession of statements being performed.
# which I guess isn't exactly a revelation, but may be useful if I need
# to squash awk call trees later...



function find_in_binding_list(id, list,		item) {
	while(list != NULL) {
		item = car(list)
		if (item == NULL) {
			print("There shouldn't be a null item here, but there is:", list)
		}
		if (car(item) == id) {
			return item
		}
		list = cdr(list)
	}
	# didn't find in env, try globals
	list = GLOBALS
	while(list != NULL) {
		item = car(list)
		if (item == NULL) {
			print("There shouldn't be a null item here (globals), but there is:", list)
		}
		if (car(item) == id) {
			return item
		}
		list = cdr(list)
	}
	return NULL
}


# two functions that copy lists, but one evalutes each member,
# and the other doesn't, it just copies.
# TODO I feel like they should be combined, simplified somehow
# but can't think of a clear way of doing that now.
# I mean the obvious way is to always call eval, but in the copy-only
# version quote every element so it ends up the same, but i'm not
# convinced that'll be any nicer.


# this evaluates the individual arguments sent to a procedure
# being invoked in eval(), ultimately passing what
# will hopefully be a list of primitives to the operator.
function eval_args(env, list) {
	if (list == NULL) {
		return NULL
	}
	return cons(eval(env, car(list)), eval_args(env, cdr(list)))
}

function copy_list(lst,		out) {
	if (lst == NULL) {
		return NULL
	}
	return cons(car(lst), copy_list(cdr(lst)))
}
# TODO note that these will use up stack to the size of the list,
# and we're limited to 1024 frames given that artificial restriction.
# it's not impossible that list length + depth in stack so far would
# be greater than 1024.
# could build the stack into these funcs explicitly, though it's
# less clear how in the case of eval_args.  should be trivial in copy_list


function builtins(op, list,		val) {

	if (op == "print") {
		display(list) # TODO this will necessarily print a list (of the args)
						# so for ex "(print 3)" will print "(3)" not "3".
						# it may be more intuitive to iterate through the list
						# and print each.

	} else if (op == "+") {
		val = 0
		while (list != NULL) {
			val = val + car(list)
			list = cdr(list)
		}
		return val

	} else if (op == "<") {
		if (list == NULL || one_elt_list(list)) {
			return "#t"
		}
		do {
			val = car(list)
			list = cdr(list)
			if (car(list) <= val) {
				return "#f"
			}
		} while (cdr(list) != NULL)
		return "#t"

	} else if (op == ">") {
		if (list == NULL || one_elt_list(list)) {
			return "#t"
		}
		do {
			val = car(list)
			list = cdr(list)
			if (car(list) >= val) {
				return "#f"
			}
		} while (cdr(list) != NULL)
		return "#t"

	} else if (op == "=") {
		if (list == NULL || one_elt_list(list)) {
			return "#t"
		}
		do {
			val = car(list)
			list = cdr(list)
			if (car(list) != val) {
				return "#f"
			}
		} while (cdr(list) != NULL)
		return "#t"

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
		# apparently in real scheme (?), there needs to be another argument
		# which is an environment specifier... but I'm not going to support
		# that for now TODO add this?
		if (!one_elt_list(list)) {
			print("wrong number of /bad arguments line", DEBUG[list])
			exit(1)
		}
		return eval(GLOBALS, car(list)) # TODO really we need that env spec, just defaulting to globals for now

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

	} else if (op == "and") {
		while (list != NULL) {
			if (car(list) == "#f") {
				return "#f"
			}
			list = cdr(list)
		}
		return "#t"

	} else if (op == "or") {
		while (list != NULL) {
			if (car(list) != "#f") {
				return "#t"
			}
			list = cdr(list)
		}
		return "#f"

	} else if (op == "not") {
		if (!one_elt_list(list)) {
			print("wrong number of /bad arguments line", DEBUG[list])
			exit(1)
		}
		if (car(list) == "#f") {
				return "#t"
		}
		return "#f"

	} else if (op == "boolean?") {
		if (!one_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		if (is_boolean(car(list))) {
			return "#t"
		}
		return "#f"

	} else if (op == "number?") {
		if (!one_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		if (is_number(car(list))) {
			return "#t"
		}
		return "#f"

	} else if (op == "string?") {
		if (!one_elt_list(list)) {
			print("wrong number of arguments line", DEBUG[list])
			exit(1)
		}
		if (is_string(car(list))) {
			return "#t"
		}
		return "#f"

	} else if (op == "eq?") {
		if (!two_elt_list(list)) {
			print("wrong number of arguments, line", DEBUG[list])
			exit(1)
		}
		if (car(list) == car(cdr(list))) {
			return "#t"
		}
		return "#f"

	} else if (op == "dump_globals") {
		dump_globals()
	}

# TODO can i use a switch, this is getting out of hand
# though even that isn't particularly elegant
# -- well, not commonly.  gawk has it but gawk feels like cheating
# at this point.  continue thinking about this...
# currently i'm leaning towards grouping builtins into similar functions
# (so they can have a smaller set of shared tmp vars)
# then this func would just be the glue but that's still just a more
# elegantly implemented hack
}


function one_elt_list(list) {
	return list != NULL && cdr(list) == NULL
}


function two_elt_list(list) {
	return list != NULL && cdr(list) != NULL && cdr(cdr(list)) == NULL
}


# here are predicates the spec gives
#boolean?
#char?
#null?
#pair?
#procedure?
#symbol?
#bytevector?
#eof-object?
#number?
#port?
#string?
#vector?
# the spec says, no object satisfies more than one of those

# Re: numbers, the spec defines:
# number?  complex?  real?  rational?  integer?
# but doesn't require that all be implemented.
# The spec also has a whole section on exactness, which
# i'm calling out of bounds for this project.

function is_boolean(v) {
	return v == "#f" || v == "#t" || v == "#true" || v == "#false"
}

function is_number(v) {
	# gnu awk has a "typeof" func, but mawk does not
	# i could possibly just use the "if (x + 0) == x" trick
	# but hell let's try regexps
	return v ~ /^[+-]?[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?$/
}

# note that I'm not handling embedded quoted chars...TODO
function is_string(v) {
	return v ~ /^".*"$/
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

