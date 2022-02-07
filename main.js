(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$AuthMsg = function (a) {
	return {$: 'AuthMsg', a: a};
};
var $author$project$Main$Loading = {$: 'Loading'};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Auth$checkForRefreshToken = _Platform_outgoingPort(
	'checkForRefreshToken',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Ports$getSheetSettingsFromStorage = _Platform_outgoingPort(
	'getSheetSettingsFromStorage',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$AccountsPage$Loading = {$: 'Loading'};
var $author$project$AccountsPage$Model = function (accounts) {
	return {accounts: accounts};
};
var $author$project$AccountsPage$init = $author$project$AccountsPage$Model($author$project$AccountsPage$Loading);
var $author$project$Auth$Model = F2(
	function (token, authState) {
		return {authState: authState, token: token};
	});
var $author$project$Auth$NotLoggedIn = {$: 'NotLoggedIn'};
var $author$project$Auth$init = A2($author$project$Auth$Model, '', $author$project$Auth$NotLoggedIn);
var $author$project$ExpenseTracker$Loading = {$: 'Loading'};
var $author$project$ExpenseTracker$init = {account: '', allAccounts: _List_Nil, amount: '', category: '', date: '', info: '', notes: '', pageState: $author$project$ExpenseTracker$Loading};
var $author$project$HomePage$init = '1';
var $author$project$IncomePage$Loaded = {$: 'Loaded'};
var $author$project$IncomePage$init = {amount: '', date: '', info: '', intoAccount: '', notes: '', pageState: $author$project$IncomePage$Loaded};
var $author$project$RecentsPage$Loading = {$: 'Loading'};
var $author$project$RecentsPage$init = {transactions: $author$project$RecentsPage$Loading};
var $author$project$SettingsPage$Model = F3(
	function (sheetId, accountSheet, expenseSheet) {
		return {accountSheet: accountSheet, expenseSheet: expenseSheet, sheetId: sheetId};
	});
var $author$project$SettingsPage$init = A3($author$project$SettingsPage$Model, '', '', '');
var $author$project$TransferPage$Loading = {$: 'Loading'};
var $author$project$TransferPage$Model = F7(
	function (amount, fromAccount, toAccount, date, info, notes, pageState) {
		return {amount: amount, date: date, fromAccount: fromAccount, info: info, notes: notes, pageState: pageState, toAccount: toAccount};
	});
var $author$project$TransferPage$init = A7($author$project$TransferPage$Model, '', '', '', '', '', '', $author$project$TransferPage$Loading);
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Main$init = function (_v0) {
	return A2(
		$elm$core$Tuple$pair,
		{accounts: _List_Nil, accountsPage: $author$project$AccountsPage$init, auth: $author$project$Auth$init, currentPage: $author$project$Main$Loading, expenseTracker: $author$project$ExpenseTracker$init, homePage: $author$project$HomePage$init, incomePage: $author$project$IncomePage$init, recentsPage: $author$project$RecentsPage$init, sheetError: '', sheetSettings: $author$project$SettingsPage$init, toastMsg: $elm$core$Maybe$Nothing, transferPage: $author$project$TransferPage$init},
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2(
					$elm$core$Platform$Cmd$map,
					$author$project$Main$AuthMsg,
					$author$project$Auth$checkForRefreshToken(_Utils_Tuple0)),
					$author$project$Ports$getSheetSettingsFromStorage(_Utils_Tuple0)
				])));
};
var $author$project$Main$GoBack = {$: 'GoBack'};
var $author$project$Auth$ReceiveAuthCode = function (a) {
	return {$: 'ReceiveAuthCode', a: a};
};
var $author$project$Auth$ReceiveRefreshToken = function (a) {
	return {$: 'ReceiveRefreshToken', a: a};
};
var $author$project$Main$ReceiveSheetSettingsFromStorage = function (a) {
	return {$: 'ReceiveSheetSettingsFromStorage', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$goBack = _Platform_incomingPort('goBack', $elm$json$Json$Decode$string);
var $elm$core$Platform$Sub$map = _Platform_map;
var $author$project$Auth$receiveAuthCode = _Platform_incomingPort('receiveAuthCode', $elm$json$Json$Decode$string);
var $author$project$Auth$receiveRefreshToken = _Platform_incomingPort('receiveRefreshToken', $elm$json$Json$Decode$string);
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $author$project$Main$receiveSheetSettingsFromStorage = _Platform_incomingPort(
	'receiveSheetSettingsFromStorage',
	A2(
		$elm$json$Json$Decode$andThen,
		function (_v0) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (_v1) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (_v2) {
							return $elm$json$Json$Decode$succeed(
								_Utils_Tuple3(_v0, _v1, _v2));
						},
						A2($elm$json$Json$Decode$index, 2, $elm$json$Json$Decode$string));
				},
				A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string)));
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				$elm$core$Platform$Sub$map,
				$author$project$Main$AuthMsg,
				$author$project$Auth$receiveRefreshToken($author$project$Auth$ReceiveRefreshToken)),
				A2(
				$elm$core$Platform$Sub$map,
				$author$project$Main$AuthMsg,
				$author$project$Auth$receiveAuthCode($author$project$Auth$ReceiveAuthCode)),
				$author$project$Main$receiveSheetSettingsFromStorage($author$project$Main$ReceiveSheetSettingsFromStorage),
				$author$project$Main$goBack(
				function (_v1) {
					return $author$project$Main$GoBack;
				})
			]));
};
var $author$project$Main$AccountsPage = {$: 'AccountsPage'};
var $author$project$Main$AccountsPageMsg = function (a) {
	return {$: 'AccountsPageMsg', a: a};
};
var $author$project$Main$ExpenseTrackerPage = {$: 'ExpenseTrackerPage'};
var $author$project$Main$ExpenseTrackerPageMsg = function (a) {
	return {$: 'ExpenseTrackerPageMsg', a: a};
};
var $author$project$Main$GlobalError = {$: 'GlobalError'};
var $author$project$Main$GoTo = function (a) {
	return {$: 'GoTo', a: a};
};
var $author$project$Main$HomePage = {$: 'HomePage'};
var $author$project$Main$IncomePage = {$: 'IncomePage'};
var $author$project$Main$IncomePageMsg = function (a) {
	return {$: 'IncomePageMsg', a: a};
};
var $author$project$AccountsPage$LoadAccounts = {$: 'LoadAccounts'};
var $author$project$RecentsPage$LoadTransactions = {$: 'LoadTransactions'};
var $author$project$Main$Login = {$: 'Login'};
var $author$project$Main$Logout = {$: 'Logout'};
var $author$project$Main$RecentsPage = {$: 'RecentsPage'};
var $author$project$Main$RecentsPageMsg = function (a) {
	return {$: 'RecentsPageMsg', a: a};
};
var $author$project$Main$SettingsPage = {$: 'SettingsPage'};
var $author$project$Main$SettingsPageMsg = function (a) {
	return {$: 'SettingsPageMsg', a: a};
};
var $author$project$Main$TransferPage = {$: 'TransferPage'};
var $author$project$Main$TransferPageMsg = function (a) {
	return {$: 'TransferPageMsg', a: a};
};
var $author$project$ExpenseTracker$Loaded = {$: 'Loaded'};
var $author$project$ExpenseTracker$Model = F8(
	function (amount, account, info, notes, date, category, pageState, allAccounts) {
		return {account: account, allAccounts: allAccounts, amount: amount, category: category, date: date, info: info, notes: notes, pageState: pageState};
	});
var $author$project$ExpenseTracker$clearExpenseTrackerData = function (expense) {
	return A8($author$project$ExpenseTracker$Model, '', expense.account, '', '', '', '', $author$project$ExpenseTracker$Loaded, expense.allAccounts);
};
var $author$project$TransferPage$Loaded = {$: 'Loaded'};
var $author$project$TransferPage$clearForm = function (_v0) {
	return {amount: '', date: '', fromAccount: '', info: '', notes: '', pageState: $author$project$TransferPage$Loaded, toAccount: ''};
};
var $author$project$Main$exitApp = _Platform_outgoingPort(
	'exitApp',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$Main$GotAccounts = function (a) {
	return {$: 'GotAccounts', a: a};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Header = F2(
	function (a, b) {
		return {$: 'Header', a: a, b: b};
	});
var $elm$http$Http$header = $elm$http$Http$Header;
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $author$project$API$get = F4(
	function (token, baseURL, queryParams, expect) {
		var queryParamsAsString = $elm$core$String$concat(
			A2(
				$elm$core$List$intersperse,
				'&',
				A2(
					$elm$core$List$map,
					function (_v0) {
						var f = _v0.a;
						var s = _v0.b;
						return f + ('=' + s);
					},
					queryParams)));
		var constructedURL = baseURL + ('?' + queryParamsAsString);
		return $elm$http$Http$request(
			{
				body: $elm$http$Http$emptyBody,
				expect: expect,
				headers: _List_fromArray(
					[
						A2($elm$http$Http$header, 'Authorization', 'Bearer ' + token),
						A2($elm$http$Http$header, 'Content-type', 'application/json')
					]),
				method: 'GET',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: constructedURL
			});
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$getAccounts = function (_v0) {
	var token = _v0.token;
	var sheetId = _v0.sheetId;
	var accountSheet = _v0.accountSheet;
	var queryParams = _List_fromArray(
		[
			_Utils_Tuple2('majorDimension', 'COLUMNS')
		]);
	var decoder = A2(
		$elm$json$Json$Decode$map,
		$elm$core$List$drop(1),
		A2(
			$elm$json$Json$Decode$map,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$List$head,
				$elm$core$Maybe$withDefault(_List_Nil)),
			A2(
				$elm$json$Json$Decode$field,
				'values',
				$elm$json$Json$Decode$list(
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string)))));
	var expect = A2($elm$http$Http$expectJson, $author$project$Main$GotAccounts, decoder);
	var baseURL = 'https://sheets.googleapis.com/v4/spreadsheets/' + (sheetId + ('/values/' + (accountSheet + '!A:A')));
	return A4($author$project$API$get, token, baseURL, queryParams, expect);
};
var $author$project$Main$getGlobals = function (model) {
	return {accountSheet: model.sheetSettings.accountSheet, accounts: model.accounts, expenseSheet: model.sheetSettings.expenseSheet, sheetError: model.sheetError, sheetId: model.sheetSettings.sheetId, token: model.auth.token};
};
var $author$project$Main$SetToastMsg = function (a) {
	return {$: 'SetToastMsg', a: a};
};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Main$hideToastMessage = function (seconds) {
	var sleeper = A2(
		$elm$core$Task$map,
		function (_v0) {
			return $elm$core$Maybe$Nothing;
		},
		$elm$core$Process$sleep(seconds * 1000));
	return A2($elm$core$Task$perform, $author$project$Main$SetToastMsg, sleeper);
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Auth$doLogin = _Platform_outgoingPort('doLogin', $elm$json$Json$Encode$string);
var $author$project$Auth$clientId = '34596233405-fmkeqrgsajsuhd39bruih2jsh7ahoq8j.apps.googleusercontent.com';
var $author$project$Auth$redirectURI = 'com.druchan.xpnsadd:/';
var $author$project$Auth$scopes = 'https://www.googleapis.com/auth/spreadsheets';
var $author$project$Auth$url = 'https://accounts.google.com/o/oauth2/v2/auth?client_id=' + ($author$project$Auth$clientId + ('&redirect_uri=' + ($author$project$Auth$redirectURI + ('&response_type=code&scope=' + $author$project$Auth$scopes))));
var $author$project$Auth$initiateLogin = function (_v0) {
	return $author$project$Auth$doLogin($author$project$Auth$url);
};
var $author$project$Main$logOut = _Platform_outgoingPort(
	'logOut',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Page$msgToCmd = function (msg) {
	return A2(
		$elm$core$Task$perform,
		function (_v0) {
			return msg;
		},
		$elm$core$Task$succeed(1));
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$AccountsPage$Accounts = function (a) {
	return {$: 'Accounts', a: a};
};
var $author$project$AccountsPage$ErrorMsg = function (a) {
	return {$: 'ErrorMsg', a: a};
};
var $author$project$Page$errToString = function (e) {
	switch (e.$) {
		case 'BadUrl':
			var s = e.a;
			return s;
		case 'BadStatus':
			var _int = e.a;
			return 'Server responded with a bad status code: ' + $elm$core$String$fromInt(_int);
		case 'BadBody':
			var s = e.a;
			return s;
		default:
			return 'Weird. Inexplicable error occured!';
	}
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $author$project$AccountsPage$AccountsLoaded = function (a) {
	return {$: 'AccountsLoaded', a: a};
};
var $author$project$AccountsPage$loadAccounts = function (_v0) {
	var token = _v0.token;
	var sheetId = _v0.sheetId;
	var accountSheet = _v0.accountSheet;
	var queryParams = _List_Nil;
	var decoder = A2(
		$elm$json$Json$Decode$field,
		'values',
		$elm$json$Json$Decode$list(
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
	var expect = A2($elm$http$Http$expectJson, $author$project$AccountsPage$AccountsLoaded, decoder);
	var baseURL = 'https://sheets.googleapis.com/v4/spreadsheets/' + (sheetId + ('/values/' + (accountSheet + '!A2:Z')));
	return A4($author$project$API$get, token, baseURL, queryParams, expect);
};
var $author$project$AccountsPage$update = F3(
	function (globals, msg, model) {
		switch (msg.$) {
			case 'LoadAccounts':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{accounts: $author$project$AccountsPage$Loading}),
					$author$project$AccountsPage$loadAccounts(globals));
			case 'AccountsLoaded':
				var res = msg.a;
				if (res.$ === 'Err') {
					var e = res.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								accounts: $author$project$AccountsPage$ErrorMsg(
									$author$project$Page$errToString(e))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var accs = res.a;
					var withDefault = $elm$core$Maybe$withDefault('');
					var toAccount = function (list) {
						var dict = $elm$core$Dict$fromList(
							A2(
								$elm$core$List$indexedMap,
								F2(
									function (idx, val) {
										return _Utils_Tuple2(
											$elm$core$String$fromInt(idx),
											val);
									}),
								list));
						return {
							balance: withDefault(
								A2($elm$core$Dict$get, '2', dict)),
							id: withDefault(
								A2($elm$core$Dict$get, '0', dict)),
							name: withDefault(
								A2($elm$core$Dict$get, '1', dict)),
							type_: withDefault(
								A2($elm$core$Dict$get, '3', dict))
						};
					};
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								accounts: $author$project$AccountsPage$Accounts(
									A2($elm$core$List$map, toAccount, accs))
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Auth$AuthError = function (a) {
	return {$: 'AuthError', a: a};
};
var $author$project$Auth$GettingAccessToken = {$: 'GettingAccessToken'};
var $author$project$Auth$LoggedIn = {$: 'LoggedIn'};
var $author$project$Auth$GotToken = function (a) {
	return {$: 'GotToken', a: a};
};
var $author$project$Auth$authUrl = 'https://oauth2.googleapis.com/token';
var $author$project$Auth$AuthObject = F2(
	function (refreshToken, token) {
		return {refreshToken: refreshToken, token: token};
	});
var $author$project$Auth$decoder1 = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Auth$AuthObject,
	A2($elm$json$Json$Decode$field, 'refresh_token', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'access_token', $elm$json$Json$Decode$string));
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$http$Http$post = function (r) {
	return $elm$http$Http$request(
		{body: r.body, expect: r.expect, headers: _List_Nil, method: 'POST', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $author$project$Auth$getAccessTokenFromAuthCode = function (code) {
	return $elm$http$Http$post(
		{
			body: $elm$http$Http$jsonBody(
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'client_id',
							$elm$json$Json$Encode$string($author$project$Auth$clientId)),
							_Utils_Tuple2(
							'grant_type',
							$elm$json$Json$Encode$string('authorization_code')),
							_Utils_Tuple2(
							'redirect_uri',
							$elm$json$Json$Encode$string($author$project$Auth$redirectURI)),
							_Utils_Tuple2(
							'code',
							$elm$json$Json$Encode$string(code))
						]))),
			expect: A2($elm$http$Http$expectJson, $author$project$Auth$GotToken, $author$project$Auth$decoder1),
			url: $author$project$Auth$authUrl
		});
};
var $author$project$Auth$getNewAccessTokenFromRefreshToken = function (refreshToken) {
	var decoder = A2(
		$elm$json$Json$Decode$map,
		function (at) {
			return A2($author$project$Auth$AuthObject, refreshToken, at);
		},
		A2($elm$json$Json$Decode$field, 'access_token', $elm$json$Json$Decode$string));
	return $elm$http$Http$post(
		{
			body: $elm$http$Http$jsonBody(
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'client_id',
							$elm$json$Json$Encode$string($author$project$Auth$clientId)),
							_Utils_Tuple2(
							'grant_type',
							$elm$json$Json$Encode$string('refresh_token')),
							_Utils_Tuple2(
							'refresh_token',
							$elm$json$Json$Encode$string(refreshToken))
						]))),
			expect: A2($elm$http$Http$expectJson, $author$project$Auth$GotToken, decoder),
			url: $author$project$Auth$authUrl
		});
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Capacitor$saveToStorage = _Platform_outgoingPort(
	'saveToStorage',
	function ($) {
		var a = $.a;
		var b = $.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string(a),
					$elm$core$Basics$identity(b)
				]));
	});
var $author$project$Capacitor$showAlert = _Platform_outgoingPort(
	'showAlert',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'message',
					$elm$json$Json$Encode$string($.message)),
					_Utils_Tuple2(
					'title',
					$elm$json$Json$Encode$string($.title))
				]));
	});
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$Auth$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'GetToken':
				var rt = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{authState: $author$project$Auth$GettingAccessToken}),
					$author$project$Auth$getNewAccessTokenFromRefreshToken(rt));
			case 'ReceiveAuthCode':
				var code = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{authState: $author$project$Auth$GettingAccessToken}),
					$author$project$Auth$getAccessTokenFromAuthCode(code));
			case 'ReceiveRefreshToken':
				var token = msg.a;
				return (token === '') ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{authState: $author$project$Auth$GettingAccessToken}),
					$author$project$Auth$getNewAccessTokenFromRefreshToken(token));
			default:
				var res = msg.a;
				if (res.$ === 'Ok') {
					var authObj = res.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{authState: $author$project$Auth$LoggedIn, token: authObj.token}),
						$author$project$Capacitor$saveToStorage(
							_Utils_Tuple2(
								'refreshToken',
								$elm$json$Json$Encode$string(authObj.refreshToken))));
				} else {
					var e = res.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								authState: $author$project$Auth$AuthError(
									$elm$core$Debug$toString(e))
							}),
						$author$project$Capacitor$showAlert(
							{
								message: 'Something went wrong when trying to authorize: ' + $elm$core$Debug$toString(e),
								title: 'Error'
							}));
				}
		}
	});
var $author$project$ExpenseTracker$GoToHomePage = {$: 'GoToHomePage'};
var $author$project$ExpenseTracker$Saving = {$: 'Saving'};
var $author$project$ExpenseTracker$SaveExpenseResponded = function (a) {
	return {$: 'SaveExpenseResponded', a: a};
};
var $elm$http$Http$expectBytesResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'arraybuffer',
			_Http_toDataView,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$expectWhatever = function (toMsg) {
	return A2(
		$elm$http$Http$expectBytesResponse,
		toMsg,
		$elm$http$Http$resolve(
			function (_v0) {
				return $elm$core$Result$Ok(_Utils_Tuple0);
			}));
};
var $author$project$API$post = F5(
	function (token, baseURL, queryParams, body, expect) {
		var queryParamsAsString = $elm$core$String$concat(
			A2(
				$elm$core$List$intersperse,
				'&',
				A2(
					$elm$core$List$map,
					function (_v0) {
						var f = _v0.a;
						var s = _v0.b;
						return f + ('=' + s);
					},
					queryParams)));
		var constructedURL = baseURL + ('?' + queryParamsAsString);
		return $elm$http$Http$request(
			{
				body: body,
				expect: expect,
				headers: _List_fromArray(
					[
						A2($elm$http$Http$header, 'Authorization', 'Bearer ' + token),
						A2($elm$http$Http$header, 'Content-type', 'application/json')
					]),
				method: 'POST',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: constructedURL
			});
	});
var $author$project$ExpenseTracker$saveExpense = F2(
	function (model, _v0) {
		var sheetId = _v0.sheetId;
		var token = _v0.token;
		var expenseSheet = _v0.expenseSheet;
		var queryParams = _List_fromArray(
			[
				_Utils_Tuple2('valueInputOption', 'USER_ENTERED')
			]);
		var expect = $elm$http$Http$expectWhatever($author$project$ExpenseTracker$SaveExpenseResponded);
		var body = $elm$http$Http$jsonBody(
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'values',
						A2(
							$elm$json$Json$Encode$list,
							$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
							_List_fromArray(
								[
									_List_fromArray(
									[model.date, model.info, model.category, model.amount, model.account, model.notes])
								])))
					])));
		var baseURL = 'https://sheets.googleapis.com/v4/spreadsheets/' + (sheetId + ('/values/' + (expenseSheet + ('!A:Z' + ':append'))));
		return A5($author$project$API$post, token, baseURL, queryParams, body, expect);
	});
var $author$project$ExpenseTracker$update = F3(
	function (msg, model, _v0) {
		var token = _v0.token;
		var sheetId = _v0.sheetId;
		var expenseSheet = _v0.expenseSheet;
		switch (msg.$) {
			case 'UpdateAccount':
				var acc = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{account: acc}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAmount':
				var amt = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{amount: amt}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateCategory':
				var cat = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{category: cat}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateDate':
				var date = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{date: date}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInfo':
				var info = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{info: info}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateNotes':
				var n = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{notes: n}),
					$elm$core$Platform$Cmd$none);
			case 'SaveExpense':
				return (sheetId === '') ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{pageState: $author$project$ExpenseTracker$Saving}),
					A2(
						$author$project$ExpenseTracker$saveExpense,
						model,
						{expenseSheet: expenseSheet, sheetId: sheetId, token: token}));
			case 'SaveExpenseResponded':
				var response = msg.a;
				if (response.$ === 'Ok') {
					return _Utils_Tuple2(
						$author$project$ExpenseTracker$clearExpenseTrackerData(model),
						A2(
							$elm$core$Task$perform,
							function (_v3) {
								return $author$project$ExpenseTracker$GoToHomePage;
							},
							$elm$core$Task$succeed(_Utils_Tuple0)));
				} else {
					var e = response.a;
					return _Utils_Tuple2(
						model,
						$author$project$Capacitor$showAlert(
							{
								message: $elm$core$Debug$toString(e),
								title: ' Error '
							}));
				}
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$IncomePage$GoToHomePage = {$: 'GoToHomePage'};
var $author$project$IncomePage$SaveTransferDone = function (a) {
	return {$: 'SaveTransferDone', a: a};
};
var $author$project$IncomePage$saveIncome = F2(
	function (_v0, model) {
		var token = _v0.token;
		var sheetId = _v0.sheetId;
		var expenseSheet = _v0.expenseSheet;
		var queryParams = _List_fromArray(
			[
				_Utils_Tuple2('valueInputOption', 'USER_ENTERED')
			]);
		var expect = $elm$http$Http$expectWhatever($author$project$IncomePage$SaveTransferDone);
		var body = $elm$http$Http$jsonBody(
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'values',
						A2(
							$elm$json$Json$Encode$list,
							$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
							_List_fromArray(
								[
									_List_fromArray(
									[model.date, model.info, 'income', '-' + model.amount, model.intoAccount, model.notes])
								])))
					])));
		var baseURL = 'https://sheets.googleapis.com/v4/spreadsheets/' + (sheetId + ('/values/' + (expenseSheet + ('!A:Z' + ':append'))));
		return A5($author$project$API$post, token, baseURL, queryParams, body, expect);
	});
var $author$project$IncomePage$update = F3(
	function (globals, msg, model) {
		switch (msg.$) {
			case 'GoToHomePage':
				return _Utils_Tuple2($author$project$IncomePage$init, $elm$core$Platform$Cmd$none);
			case 'NoOp':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'UpdateDate':
				var date = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{date: date}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateIntoAccount':
				var acc = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{intoAccount: acc}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAmount':
				var amt = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{amount: amt}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateNotes':
				var notes = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{notes: notes}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInfo':
				var info = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{info: info}),
					$elm$core$Platform$Cmd$none);
			case 'SaveIncome':
				return _Utils_Tuple2(
					model,
					A2($author$project$IncomePage$saveIncome, globals, model));
			default:
				var res = msg.a;
				if (res.$ === 'Ok') {
					return _Utils_Tuple2(
						$author$project$IncomePage$init,
						A2(
							$elm$core$Task$perform,
							function (_v2) {
								return $author$project$IncomePage$GoToHomePage;
							},
							$elm$core$Task$succeed(_Utils_Tuple0)));
				} else {
					var e = res.a;
					return _Utils_Tuple2(
						model,
						$author$project$Capacitor$showAlert(
							{
								message: $elm$core$Debug$toString(e),
								title: ' Error '
							}));
				}
		}
	});
var $author$project$RecentsPage$ErrorMsg = function (a) {
	return {$: 'ErrorMsg', a: a};
};
var $author$project$RecentsPage$Transactions = function (a) {
	return {$: 'Transactions', a: a};
};
var $author$project$RecentsPage$TransactionsLoaded = function (a) {
	return {$: 'TransactionsLoaded', a: a};
};
var $author$project$RecentsPage$loadTransactions = function (_v0) {
	var token = _v0.token;
	var sheetId = _v0.sheetId;
	var expenseSheet = _v0.expenseSheet;
	var queryParams = _List_Nil;
	var decoder = A2(
		$elm$json$Json$Decode$field,
		'values',
		$elm$json$Json$Decode$list(
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
	var expect = A2($elm$http$Http$expectJson, $author$project$RecentsPage$TransactionsLoaded, decoder);
	var baseURL = 'https://sheets.googleapis.com/v4/spreadsheets/' + (sheetId + ('/values/' + (expenseSheet + '!A2:Z')));
	return A4($author$project$API$get, token, baseURL, queryParams, expect);
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$RecentsPage$update = F3(
	function (globals, msg, model) {
		switch (msg.$) {
			case 'LoadTransactions':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{transactions: $author$project$RecentsPage$Loading}),
					$author$project$RecentsPage$loadTransactions(globals));
			case 'GoToHomePage':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			default:
				var res = msg.a;
				if (res.$ === 'Err') {
					var e = res.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								transactions: $author$project$RecentsPage$ErrorMsg(
									$author$project$Page$errToString(e))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var list = res.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								transactions: $author$project$RecentsPage$Transactions(
									A2(
										$elm$core$List$take,
										10,
										$elm$core$List$reverse(list)))
							}),
						$elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$SettingsPage$saveSheetSettings = function (model) {
	return $elm$core$Platform$Cmd$batch(
		_List_fromArray(
			[
				$author$project$Capacitor$saveToStorage(
				_Utils_Tuple2(
					'sheetId',
					$elm$json$Json$Encode$string(model.sheetId))),
				$author$project$Capacitor$saveToStorage(
				_Utils_Tuple2(
					'accountSheet',
					$elm$json$Json$Encode$string(model.accountSheet))),
				$author$project$Capacitor$saveToStorage(
				_Utils_Tuple2(
					'expenseSheet',
					$elm$json$Json$Encode$string(model.expenseSheet)))
			]));
};
var $author$project$SettingsPage$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UpdateSheetId':
				var id = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sheetId: id}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAccountSheetName':
				var name = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{accountSheet: name}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateExpenseSheetName':
				var name = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{expenseSheet: name}),
					$elm$core$Platform$Cmd$none);
			case 'GoToHomePage':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'SaveSheetSettings':
				return _Utils_Tuple2(
					model,
					$author$project$SettingsPage$saveSheetSettings(model));
			default:
				return _Utils_Tuple2(
					model,
					$author$project$Ports$getSheetSettingsFromStorage(_Utils_Tuple0));
		}
	});
var $author$project$TransferPage$GoToHomePage = {$: 'GoToHomePage'};
var $author$project$TransferPage$Saving = {$: 'Saving'};
var $author$project$TransferPage$SaveTransferDone = function (a) {
	return {$: 'SaveTransferDone', a: a};
};
var $author$project$TransferPage$saveTransfer = F2(
	function (model, _v0) {
		var token = _v0.token;
		var expenseSheet = _v0.expenseSheet;
		var sheetId = _v0.sheetId;
		var queryParams = _List_fromArray(
			[
				_Utils_Tuple2('valueInputOption', 'USER_ENTERED')
			]);
		var expect = $elm$http$Http$expectWhatever($author$project$TransferPage$SaveTransferDone);
		var body = $elm$http$Http$jsonBody(
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'values',
						A2(
							$elm$json$Json$Encode$list,
							$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
							_List_fromArray(
								[
									_List_fromArray(
									[model.date, model.info, 'transfer', model.amount, model.fromAccount, model.notes]),
									_List_fromArray(
									[model.date, model.info, 'transfer', '-' + model.amount, model.toAccount, model.notes])
								])))
					])));
		var baseURL = 'https://sheets.googleapis.com/v4/spreadsheets/' + (sheetId + ('/values/' + (expenseSheet + ('!A:Z' + ':append'))));
		return A5($author$project$API$post, token, baseURL, queryParams, body, expect);
	});
var $author$project$TransferPage$update = F3(
	function (msg, model, globals) {
		switch (msg.$) {
			case 'GoToHomePage':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'UpdateFromAccount':
				var acc = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{fromAccount: acc}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateDate':
				var date = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{date: date}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateToAccount':
				var acc = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{toAccount: acc}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAmount':
				var amt = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{amount: amt}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateNotes':
				var notes = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{notes: notes}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInfo':
				var info = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{info: info}),
					$elm$core$Platform$Cmd$none);
			case 'SaveTransfer':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{pageState: $author$project$TransferPage$Saving}),
					A2($author$project$TransferPage$saveTransfer, model, globals));
			default:
				var res = msg.a;
				if (res.$ === 'Ok') {
					return _Utils_Tuple2(
						$author$project$TransferPage$clearForm(model),
						A2(
							$elm$core$Task$perform,
							function (_v2) {
								return $author$project$TransferPage$GoToHomePage;
							},
							$elm$core$Task$succeed(_Utils_Tuple0)));
				} else {
					var e = res.a;
					return _Utils_Tuple2(
						model,
						$author$project$Capacitor$showAlert(
							{
								message: $elm$core$Debug$toString(e),
								title: ' Error '
							}));
				}
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 'NoOp':
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				case 'DoLogin':
					return _Utils_Tuple2(
						model,
						$author$project$Auth$initiateLogin(_Utils_Tuple0));
				case 'GoTo':
					var page = msg.a;
					switch (page.$) {
						case 'HomePage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										currentPage: $author$project$Main$HomePage,
										expenseTracker: $author$project$ExpenseTracker$clearExpenseTrackerData(model.expenseTracker)
									}),
								$elm$core$Platform$Cmd$none);
						case 'ExpenseTrackerPage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										currentPage: $author$project$Main$ExpenseTrackerPage,
										expenseTracker: $author$project$ExpenseTracker$clearExpenseTrackerData(model.expenseTracker)
									}),
								$elm$core$Platform$Cmd$none);
						case 'SettingsPage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{currentPage: $author$project$Main$SettingsPage}),
								$elm$core$Platform$Cmd$none);
						case 'RecentsPage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{currentPage: $author$project$Main$RecentsPage, recentsPage: $author$project$RecentsPage$init}),
								A2(
									$elm$core$Platform$Cmd$map,
									$author$project$Main$RecentsPageMsg,
									$author$project$Page$msgToCmd($author$project$RecentsPage$LoadTransactions)));
						case 'IncomePage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{currentPage: $author$project$Main$IncomePage}),
								$elm$core$Platform$Cmd$none);
						case 'TransferPage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{currentPage: $author$project$Main$TransferPage}),
								$elm$core$Platform$Cmd$none);
						case 'Loading':
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						case 'Login':
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						case 'GlobalError':
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{accountsPage: $author$project$AccountsPage$init, currentPage: $author$project$Main$AccountsPage}),
								A2(
									$elm$core$Platform$Cmd$map,
									$author$project$Main$AccountsPageMsg,
									$author$project$Page$msgToCmd($author$project$AccountsPage$LoadAccounts)));
					}
				case 'AuthMsg':
					var authMsg = msg.a;
					var _v2 = A2($author$project$Auth$update, authMsg, model.auth);
					var m = _v2.a;
					var cmd = _v2.b;
					var cmd_ = A2($elm$core$Platform$Cmd$map, $author$project$Main$AuthMsg, cmd);
					var _v3 = m.authState;
					switch (_v3.$) {
						case 'AuthError':
							return A2(
								$elm$core$Tuple$pair,
								_Utils_update(
									model,
									{auth: m, currentPage: $author$project$Main$GlobalError}),
								cmd_);
						case 'NotLoggedIn':
							return A2(
								$elm$core$Tuple$pair,
								_Utils_update(
									model,
									{auth: m, currentPage: $author$project$Main$Login}),
								cmd_);
						case 'GettingAccessToken':
							return A2(
								$elm$core$Tuple$pair,
								_Utils_update(
									model,
									{auth: m, currentPage: $author$project$Main$Loading}),
								cmd_);
						default:
							return A2(
								$elm$core$Tuple$pair,
								_Utils_update(
									model,
									{auth: m, currentPage: $author$project$Main$HomePage}),
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											cmd_,
											$author$project$Main$getAccounts(
											{accountSheet: model.sheetSettings.accountSheet, sheetId: model.sheetSettings.sheetId, token: m.token})
										])));
					}
				case 'HomePageMsg':
					var homePageMsg = msg.a;
					switch (homePageMsg.$) {
						case 'GoToExpensePage':
							var $temp$msg = $author$project$Main$GoTo($author$project$Main$ExpenseTrackerPage),
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						case 'GoToTransferPage':
							var $temp$msg = $author$project$Main$GoTo($author$project$Main$TransferPage),
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						case 'GoToSettingsPage':
							var $temp$msg = $author$project$Main$GoTo($author$project$Main$SettingsPage),
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						case 'GoToIncomePage':
							var $temp$msg = $author$project$Main$GoTo($author$project$Main$IncomePage),
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						case 'GoToRecentsPage':
							var $temp$msg = $author$project$Main$GoTo($author$project$Main$RecentsPage),
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						case 'Logout':
							var $temp$msg = $author$project$Main$Logout,
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						default:
							var $temp$msg = $author$project$Main$GoTo($author$project$Main$AccountsPage),
								$temp$model = model;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
					}
				case 'ExpenseTrackerPageMsg':
					var expenseTrackerMsg = msg.a;
					var _v5 = A3(
						$author$project$ExpenseTracker$update,
						expenseTrackerMsg,
						model.expenseTracker,
						{expenseSheet: model.sheetSettings.expenseSheet, sheetId: model.sheetSettings.sheetId, token: model.auth.token});
					var m = _v5.a;
					var cmd = _v5.b;
					var cmd_ = A2($elm$core$Platform$Cmd$map, $author$project$Main$ExpenseTrackerPageMsg, cmd);
					switch (expenseTrackerMsg.$) {
						case 'GoToHomePage':
							return A2(
								$elm$core$Tuple$pair,
								_Utils_update(
									model,
									{
										currentPage: $author$project$Main$HomePage,
										expenseTracker: $author$project$ExpenseTracker$clearExpenseTrackerData(model.expenseTracker)
									}),
								cmd_);
						case 'SaveExpenseResponded':
							var response = expenseTrackerMsg.a;
							if (response.$ === 'Ok') {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											expenseTracker: m,
											toastMsg: $elm$core$Maybe$Just('Saved!')
										}),
									$elm$core$Platform$Cmd$batch(
										_List_fromArray(
											[
												cmd_,
												$author$project$Main$hideToastMessage(5)
											])));
							} else {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{expenseTracker: m}),
									cmd_);
							}
						default:
							return A2(
								$elm$core$Tuple$pair,
								_Utils_update(
									model,
									{expenseTracker: m}),
								cmd_);
					}
				case 'SettingsPageMsg':
					var settingsPageMsg = msg.a;
					var _v8 = A2($author$project$SettingsPage$update, settingsPageMsg, model.sheetSettings);
					var m = _v8.a;
					var cmd = _v8.b;
					var newCmd = A2($elm$core$Platform$Cmd$map, $author$project$Main$SettingsPageMsg, cmd);
					var newModel = _Utils_update(
						model,
						{sheetSettings: m});
					switch (settingsPageMsg.$) {
						case 'GoToHomePage':
							var $temp$msg = $author$project$Main$GoTo($author$project$Main$HomePage),
								$temp$model = newModel;
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						case 'SaveSheetSettings':
							return _Utils_Tuple2(
								_Utils_update(
									newModel,
									{
										toastMsg: $elm$core$Maybe$Just('Saved!')
									}),
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											newCmd,
											$author$project$Main$getAccounts(
											{accountSheet: m.accountSheet, sheetId: m.sheetId, token: model.auth.token}),
											$author$project$Main$hideToastMessage(5)
										])));
						default:
							return _Utils_Tuple2(newModel, newCmd);
					}
				case 'GotAccounts':
					var res = msg.a;
					if (res.$ === 'Ok') {
						var accs = res.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									accounts: $elm$core$Set$toList(
										$elm$core$Set$fromList(accs)),
									sheetError: ''
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{accounts: _List_Nil, sheetError: 'Could not get accounts for the given sheetID/account sheet name combination. Please check if the sheetID and the accounts sheet name are correct.'}),
							$elm$core$Platform$Cmd$batch(_List_Nil));
					}
				case 'SetToastMsg':
					var message = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{toastMsg: message}),
						$author$project$Main$hideToastMessage(5));
				case 'GoBack':
					var _v11 = model.currentPage;
					switch (_v11.$) {
						case 'HomePage':
							return _Utils_Tuple2(
								model,
								$author$project$Main$exitApp(_Utils_Tuple0));
						case 'ExpenseTrackerPage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										currentPage: $author$project$Main$HomePage,
										expenseTracker: $author$project$ExpenseTracker$clearExpenseTrackerData(model.expenseTracker)
									}),
								$elm$core$Platform$Cmd$none);
						case 'SettingsPage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{currentPage: $author$project$Main$HomePage}),
								$elm$core$Platform$Cmd$none);
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{currentPage: $author$project$Main$HomePage}),
								$elm$core$Platform$Cmd$none);
					}
				case 'TransferPageMsg':
					var tpMsg = msg.a;
					var _v12 = A3(
						$author$project$TransferPage$update,
						tpMsg,
						model.transferPage,
						$author$project$Main$getGlobals(model));
					var m = _v12.a;
					var cmd = _v12.b;
					var cmd_ = A2($elm$core$Platform$Cmd$map, $author$project$Main$TransferPageMsg, cmd);
					switch (tpMsg.$) {
						case 'GoToHomePage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										currentPage: $author$project$Main$HomePage,
										transferPage: $author$project$TransferPage$clearForm(model.transferPage)
									}),
								cmd_);
						case 'SaveTransferDone':
							var res = tpMsg.a;
							if (res.$ === 'Ok') {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											toastMsg: $elm$core$Maybe$Just('Saved!')
										}),
									$elm$core$Platform$Cmd$batch(
										_List_fromArray(
											[
												cmd_,
												$author$project$Main$hideToastMessage(5)
											])));
							} else {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{transferPage: m}),
									cmd_);
							}
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{transferPage: m}),
								cmd_);
					}
				case 'IncomePageMsg':
					var ipMsg = msg.a;
					var _v15 = A3(
						$author$project$IncomePage$update,
						$author$project$Main$getGlobals(model),
						ipMsg,
						model.incomePage);
					var m = _v15.a;
					var cmd = _v15.b;
					var cmd_ = A2($elm$core$Platform$Cmd$map, $author$project$Main$IncomePageMsg, cmd);
					switch (ipMsg.$) {
						case 'GoToHomePage':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{currentPage: $author$project$Main$HomePage, incomePage: m}),
								cmd_);
						case 'SaveTransferDone':
							var res = ipMsg.a;
							if (res.$ === 'Ok') {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											incomePage: m,
											toastMsg: $elm$core$Maybe$Just('Saved!')
										}),
									$elm$core$Platform$Cmd$batch(
										_List_fromArray(
											[
												cmd_,
												$author$project$Main$hideToastMessage(5)
											])));
							} else {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{incomePage: m}),
									cmd_);
							}
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{incomePage: m}),
								cmd_);
					}
				case 'Logout':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{accounts: _List_Nil, auth: $author$project$Auth$init, currentPage: $author$project$Main$Login}),
						$author$project$Main$logOut(_Utils_Tuple0));
				case 'ReceiveSheetSettingsFromStorage':
					var _v18 = msg.a;
					var sheetId = _v18.a;
					var accountSheet = _v18.b;
					var expenseSheet = _v18.c;
					var sheetSettings = A3($author$project$SettingsPage$Model, sheetId, accountSheet, expenseSheet);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{sheetSettings: sheetSettings}),
						$author$project$Main$getAccounts(
							{accountSheet: accountSheet, sheetId: sheetId, token: model.auth.token}));
				case 'RecentsPageMsg':
					var recentsPageMsg = msg.a;
					var _v19 = A3(
						$author$project$RecentsPage$update,
						$author$project$Main$getGlobals(model),
						recentsPageMsg,
						model.recentsPage);
					var m = _v19.a;
					var cmd = _v19.b;
					var cmd_ = A2($elm$core$Platform$Cmd$map, $author$project$Main$RecentsPageMsg, cmd);
					var m_ = _Utils_update(
						model,
						{recentsPage: m});
					if (recentsPageMsg.$ === 'GoToHomePage') {
						return _Utils_Tuple2(
							_Utils_update(
								m_,
								{currentPage: $author$project$Main$HomePage}),
							cmd_);
					} else {
						return _Utils_Tuple2(m_, cmd_);
					}
				default:
					var accPageMsg = msg.a;
					var _v21 = A3(
						$author$project$AccountsPage$update,
						$author$project$Main$getGlobals(model),
						accPageMsg,
						model.accountsPage);
					var m = _v21.a;
					var cmd = _v21.b;
					var cmd_ = A2($elm$core$Platform$Cmd$map, $author$project$Main$AccountsPageMsg, cmd);
					var m_ = _Utils_update(
						model,
						{accountsPage: m});
					if (accPageMsg.$ === 'GoToHomePage') {
						return _Utils_Tuple2(
							_Utils_update(
								m_,
								{currentPage: $author$project$Main$HomePage}),
							cmd_);
					} else {
						return _Utils_Tuple2(m_, cmd_);
					}
			}
		}
	});
var $author$project$Page$Global = F6(
	function (token, sheetId, accounts, sheetError, accountSheet, expenseSheet) {
		return {accountSheet: accountSheet, accounts: accounts, expenseSheet: expenseSheet, sheetError: sheetError, sheetId: sheetId, token: token};
	});
var $author$project$Main$HomePageMsg = function (a) {
	return {$: 'HomePageMsg', a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Page$pageLoaderDiv = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('h-screen flex items-center justify-center')
		]),
	_List_fromArray(
		[
			$elm$html$Html$text('Loading...')
		]));
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Page$renderBody = function (content) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$id('app-page-content')
			]),
		_List_fromArray(
			[content]));
};
var $author$project$AccountsPage$GoToHomePage = {$: 'GoToHomePage'};
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $feathericons$elm_feather$FeatherIcons$Icon = function (a) {
	return {$: 'Icon', a: a};
};
var $feathericons$elm_feather$FeatherIcons$defaultAttributes = function (name) {
	return {
		_class: $elm$core$Maybe$Just('feather feather-' + name),
		size: 24,
		sizeUnit: '',
		strokeWidth: 2,
		viewBox: '0 0 24 24'
	};
};
var $feathericons$elm_feather$FeatherIcons$makeBuilder = F2(
	function (name, src) {
		return $feathericons$elm_feather$FeatherIcons$Icon(
			{
				attrs: $feathericons$elm_feather$FeatherIcons$defaultAttributes(name),
				src: src
			});
	});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $feathericons$elm_feather$FeatherIcons$home = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'home',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 22 9 12 15 12 15 22')
				]),
			_List_Nil)
		]));
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$AccountsPage$accountCard = function (acc) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('flex flex-col')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('text-gray-400 text-xs')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('id : ' + acc.id)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('text-lg')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(acc.balance)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('text-xs')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(acc.name)
							])),
						($elm$core$String$length(acc.type_) > 0) ? A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('text-gray-500')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(' (type : ' + (acc.type_ + ')'))
							])) : A2($elm$html$Html$span, _List_Nil, _List_Nil)
					]))
			]));
};
var $author$project$AccountsPage$renderBody = function (_v0) {
	var accounts = _v0.accounts;
	switch (accounts.$) {
		case 'Loading':
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Loading...')
					]));
		case 'ErrorMsg':
			var msg = accounts.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(msg)
					]));
		default:
			var accs = accounts.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('flex flex-col gap-10')
					]),
				A2($elm$core$List$map, $author$project$AccountsPage$accountCard, accs));
	}
};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$map = $elm$virtual_dom$VirtualDom$map;
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var $elm$svg$Svg$Attributes$strokeLinejoin = _VirtualDom_attribute('stroke-linejoin');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $feathericons$elm_feather$FeatherIcons$toHtml = F2(
	function (attributes, _v0) {
		var src = _v0.a.src;
		var attrs = _v0.a.attrs;
		var strSize = $elm$core$String$fromFloat(attrs.size);
		var baseAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$fill('none'),
				$elm$svg$Svg$Attributes$height(
				_Utils_ap(strSize, attrs.sizeUnit)),
				$elm$svg$Svg$Attributes$width(
				_Utils_ap(strSize, attrs.sizeUnit)),
				$elm$svg$Svg$Attributes$stroke('currentColor'),
				$elm$svg$Svg$Attributes$strokeLinecap('round'),
				$elm$svg$Svg$Attributes$strokeLinejoin('round'),
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromFloat(attrs.strokeWidth)),
				$elm$svg$Svg$Attributes$viewBox(attrs.viewBox)
			]);
		var combinedAttributes = _Utils_ap(
			function () {
				var _v1 = attrs._class;
				if (_v1.$ === 'Just') {
					var c = _v1.a;
					return A2(
						$elm$core$List$cons,
						$elm$svg$Svg$Attributes$class(c),
						baseAttributes);
				} else {
					return baseAttributes;
				}
			}(),
			attributes);
		return A2(
			$elm$svg$Svg$svg,
			combinedAttributes,
			A2(
				$elm$core$List$map,
				$elm$svg$Svg$map($elm$core$Basics$never),
				src));
	});
var $feathericons$elm_feather$FeatherIcons$withSize = F2(
	function (size, _v0) {
		var attrs = _v0.a.attrs;
		var src = _v0.a.src;
		return $feathericons$elm_feather$FeatherIcons$Icon(
			{
				attrs: _Utils_update(
					attrs,
					{size: size}),
				src: src
			});
	});
var $feathericons$elm_feather$FeatherIcons$withStrokeWidth = F2(
	function (strokeWidth, _v0) {
		var attrs = _v0.a.attrs;
		var src = _v0.a.src;
		return $feathericons$elm_feather$FeatherIcons$Icon(
			{
				attrs: _Utils_update(
					attrs,
					{strokeWidth: strokeWidth}),
				src: src
			});
	});
var $author$project$Page$renderIcon = function (_v0) {
	var icon = _v0.icon;
	var size = _v0.size;
	return A2(
		$feathericons$elm_feather$FeatherIcons$toHtml,
		_List_Nil,
		A2(
			$feathericons$elm_feather$FeatherIcons$withStrokeWidth,
			2,
			A2($feathericons$elm_feather$FeatherIcons$withSize, size, icon)));
};
var $author$project$Page$renderHeader = function (_v0) {
	var text = _v0.text;
	var icon = _v0.icon;
	var msg = _v0.msg;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$id('app-page-header')
			]),
		_List_fromArray(
			[
				A2($elm$html$Html$div, _List_Nil, _List_Nil),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(text)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(msg)
					]),
				_List_fromArray(
					[
						$author$project$Page$renderIcon(
						{icon: icon, size: 16})
					]))
			]));
};
var $author$project$AccountsPage$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Page$renderHeader(
				{icon: $feathericons$elm_feather$FeatherIcons$home, msg: $author$project$AccountsPage$GoToHomePage, text: 'Account Balances'}),
				$author$project$Page$renderBody(
				$author$project$AccountsPage$renderBody(model))
			]));
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$ExpenseTracker$pageWrapper = function (body) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Page$renderHeader(
				{icon: $feathericons$elm_feather$FeatherIcons$home, msg: $author$project$ExpenseTracker$GoToHomePage, text: 'Expense'}),
				$author$project$Page$renderBody(body)
			]));
};
var $author$project$ExpenseTracker$showError = function (error) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('p-2 rounded bg-red-100 text-xs mb-5')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(error)
			]));
};
var $author$project$ExpenseTracker$SaveExpense = {$: 'SaveExpense'};
var $author$project$ExpenseTracker$UpdateAccount = function (a) {
	return {$: 'UpdateAccount', a: a};
};
var $author$project$ExpenseTracker$UpdateAmount = function (a) {
	return {$: 'UpdateAmount', a: a};
};
var $author$project$ExpenseTracker$UpdateCategory = function (a) {
	return {$: 'UpdateCategory', a: a};
};
var $author$project$ExpenseTracker$UpdateDate = function (a) {
	return {$: 'UpdateDate', a: a};
};
var $author$project$ExpenseTracker$UpdateInfo = function (a) {
	return {$: 'UpdateInfo', a: a};
};
var $author$project$ExpenseTracker$UpdateNotes = function (a) {
	return {$: 'UpdateNotes', a: a};
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$Page$formElement = F2(
	function (label, control) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('flex flex-col gap-2')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('text-xs')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[control]))
				]));
	});
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$ExpenseTracker$isExpenseFormValid = function (expense) {
	var nothingIsEmpty = A2(
		$elm$core$List$all,
		function (str) {
			return $elm$core$String$length(str) > 0;
		},
		_List_fromArray(
			[expense.amount, expense.date, expense.info, expense.account]));
	var amountIsValidNumber = function () {
		var _v0 = $elm$core$String$toFloat(expense.amount);
		if (_v0.$ === 'Nothing') {
			return false;
		} else {
			return true;
		}
	}();
	return nothingIsEmpty && amountIsValidNumber;
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$ExpenseTracker$viewForm = F2(
	function (accounts, model) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('flex flex-col gap-10')
						]),
					_List_fromArray(
						[
							A2(
							$author$project$Page$formElement,
							'How much?',
							A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('number'),
										$elm$html$Html$Attributes$value(model.amount),
										$elm$html$Html$Events$onInput($author$project$ExpenseTracker$UpdateAmount)
									]),
								_List_Nil)),
							A2(
							$author$project$Page$formElement,
							'What did you spend on?',
							A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$value(model.info),
										$elm$html$Html$Events$onInput($author$project$ExpenseTracker$UpdateInfo)
									]),
								_List_Nil)),
							A2(
							$author$project$Page$formElement,
							'When did you spend this?',
							A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('date'),
										$elm$html$Html$Attributes$value(model.date),
										$elm$html$Html$Events$onInput($author$project$ExpenseTracker$UpdateDate)
									]),
								_List_Nil)),
							A2(
							$author$project$Page$formElement,
							'Which account from?',
							A2(
								$elm$html$Html$select,
								_List_fromArray(
									[
										$elm$html$Html$Events$onInput($author$project$ExpenseTracker$UpdateAccount)
									]),
								A2(
									$elm$core$List$map,
									function (account) {
										return A2(
											$elm$html$Html$option,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$value(account),
													$elm$html$Html$Attributes$selected(
													_Utils_eq(account, model.account))
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(
													(account === '') ? 'Select account' : account)
												]));
									},
									$elm$core$List$concat(
										_List_fromArray(
											[
												_List_fromArray(
												['']),
												accounts
											]))))),
							A2(
							$author$project$Page$formElement,
							'Category? (optional)',
							A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$value(model.category),
										$elm$html$Html$Events$onInput($author$project$ExpenseTracker$UpdateCategory)
									]),
								_List_Nil)),
							A2(
							$author$project$Page$formElement,
							'Notes (optional)',
							A2(
								$elm$html$Html$textarea,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$value(model.notes),
										$elm$html$Html$Events$onInput($author$project$ExpenseTracker$UpdateNotes)
									]),
								_List_Nil)),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('flex justify-around gap-4')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Events$onClick($author$project$ExpenseTracker$GoToHomePage)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Back')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('primary'),
											$elm$html$Html$Attributes$disabled(
											(!$author$project$ExpenseTracker$isExpenseFormValid(model)) || _Utils_eq(model.pageState, $author$project$ExpenseTracker$Saving)),
											$elm$html$Html$Events$onClick($author$project$ExpenseTracker$SaveExpense)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Save')
										]))
								]))
						]))
				]));
	});
var $author$project$Page$viewToaster = function (string) {
	if (string.$ === 'Nothing') {
		return $elm$html$Html$text('');
	} else {
		var str = string.a;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('fixed bottom-5 left-5 right-5 bg-blue-700 text-white p-3 rounded text-xs')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(str)
				]));
	}
};
var $author$project$ExpenseTracker$view = F2(
	function (_v0, model) {
		var sheetError = _v0.sheetError;
		var accounts = _v0.accounts;
		var _v1 = model.pageState;
		switch (_v1.$) {
			case 'Loading':
				return $author$project$Page$pageLoaderDiv;
			case 'Loaded':
				return $author$project$ExpenseTracker$pageWrapper(
					(sheetError !== '') ? A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$ExpenseTracker$showError(sheetError),
								A2($author$project$ExpenseTracker$viewForm, accounts, model)
							])) : A2($author$project$ExpenseTracker$viewForm, accounts, model));
			default:
				return $author$project$ExpenseTracker$pageWrapper(
					A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$ExpenseTracker$viewForm, accounts, model),
								$author$project$Page$viewToaster(
								$elm$core$Maybe$Just('Saving...'))
							])));
		}
	});
var $author$project$HomePage$GoToAccountsPage = {$: 'GoToAccountsPage'};
var $author$project$HomePage$GoToExpensePage = {$: 'GoToExpensePage'};
var $author$project$HomePage$GoToIncomePage = {$: 'GoToIncomePage'};
var $author$project$HomePage$GoToRecentsPage = {$: 'GoToRecentsPage'};
var $author$project$HomePage$GoToSettingsPage = {$: 'GoToSettingsPage'};
var $author$project$HomePage$GoToTransferPage = {$: 'GoToTransferPage'};
var $author$project$HomePage$Logout = {$: 'Logout'};
var $feathericons$elm_feather$FeatherIcons$activity = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'activity',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 12 18 12 15 21 9 3 6 12 2 12')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $feathericons$elm_feather$FeatherIcons$arrowUpCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-up-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 12 12 8 8 12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$dollarSign = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'dollar-sign',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$fileText = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'file-text',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('14 2 14 8 20 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('10 9 9 9 8 9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$logOut = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'log-out',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 17 21 12 16 7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$refreshCw = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'refresh-cw',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 4 23 10 17 10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('1 20 1 14 7 14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevron-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 18 15 12 9 6')
				]),
			_List_Nil)
		]));
var $author$project$HomePage$renderOption = F3(
	function (icon, text, msg) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(msg),
					$elm$html$Html$Attributes$class('rounded p-5 border border-gray-200 flex items-center justify-between gap-5')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('flex gap-5 items-center')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$author$project$Page$renderIcon(
									{icon: icon, size: 16})
								])),
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(text)
								]))
						])),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Page$renderIcon(
							{icon: $feathericons$elm_feather$FeatherIcons$chevronRight, size: 24})
						]))
				]));
	});
var $feathericons$elm_feather$FeatherIcons$settings = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'settings',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z')
				]),
			_List_Nil)
		]));
var $author$project$HomePage$view = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			$author$project$Page$renderHeader(
			{icon: $feathericons$elm_feather$FeatherIcons$settings, msg: $author$project$HomePage$GoToSettingsPage, text: 'Dashboard'}),
			$author$project$Page$renderBody(
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('flex flex-col gap-5')
					]),
				_List_fromArray(
					[
						A3($author$project$HomePage$renderOption, $feathericons$elm_feather$FeatherIcons$arrowUpCircle, 'Expense', $author$project$HomePage$GoToExpensePage),
						A3($author$project$HomePage$renderOption, $feathericons$elm_feather$FeatherIcons$dollarSign, 'Income', $author$project$HomePage$GoToIncomePage),
						A3($author$project$HomePage$renderOption, $feathericons$elm_feather$FeatherIcons$refreshCw, 'Transfer', $author$project$HomePage$GoToTransferPage),
						A3($author$project$HomePage$renderOption, $feathericons$elm_feather$FeatherIcons$activity, 'Recent', $author$project$HomePage$GoToRecentsPage),
						A3($author$project$HomePage$renderOption, $feathericons$elm_feather$FeatherIcons$fileText, 'Balances', $author$project$HomePage$GoToAccountsPage),
						A3($author$project$HomePage$renderOption, $feathericons$elm_feather$FeatherIcons$logOut, 'Logout', $author$project$HomePage$Logout)
					])))
		]));
var $author$project$IncomePage$SaveIncome = {$: 'SaveIncome'};
var $author$project$IncomePage$Saving = {$: 'Saving'};
var $author$project$IncomePage$UpdateAmount = function (a) {
	return {$: 'UpdateAmount', a: a};
};
var $author$project$IncomePage$UpdateDate = function (a) {
	return {$: 'UpdateDate', a: a};
};
var $author$project$IncomePage$UpdateInfo = function (a) {
	return {$: 'UpdateInfo', a: a};
};
var $author$project$IncomePage$UpdateIntoAccount = function (a) {
	return {$: 'UpdateIntoAccount', a: a};
};
var $author$project$IncomePage$UpdateNotes = function (a) {
	return {$: 'UpdateNotes', a: a};
};
var $elm$core$String$trim = _String_trim;
var $author$project$IncomePage$isFormValid = function (model) {
	var notEmpty = !A2(
		$elm$core$List$any,
		A2($elm$core$Basics$composeR, $elm$core$String$trim, $elm$core$String$isEmpty),
		_List_fromArray(
			[model.date, model.info, model.intoAccount, model.amount]));
	var amountIsValidNumber = function () {
		var _v0 = $elm$core$String$toFloat(model.amount);
		if (_v0.$ === 'Nothing') {
			return false;
		} else {
			return true;
		}
	}();
	return notEmpty && amountIsValidNumber;
};
var $author$project$IncomePage$pageHeader = $author$project$Page$renderHeader(
	{icon: $feathericons$elm_feather$FeatherIcons$home, msg: $author$project$IncomePage$GoToHomePage, text: 'Income'});
var $author$project$IncomePage$pageRenderer = function (content) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$IncomePage$pageHeader,
				$author$project$Page$renderBody(content)
			]));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $author$project$IncomePage$view = F2(
	function (globals, model) {
		return $author$project$IncomePage$pageRenderer(
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('flex flex-col gap-5')
					]),
				_List_fromArray(
					[
						(globals.sheetError !== '') ? $author$project$ExpenseTracker$showError(globals.sheetError) : ((globals.sheetId === '') ? $author$project$ExpenseTracker$showError('No sheet ID.') : $elm$html$Html$text('')),
						A2(
						$author$project$Page$formElement,
						'How much?',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.amount),
									$elm$html$Html$Events$onInput($author$project$IncomePage$UpdateAmount),
									$elm$html$Html$Attributes$type_('number')
								]),
							_List_Nil)),
						A2(
						$author$project$Page$formElement,
						'To which account?',
						A2(
							$elm$html$Html$select,
							_List_fromArray(
								[
									$elm$html$Html$Events$onInput($author$project$IncomePage$UpdateIntoAccount),
									$elm$html$Html$Attributes$placeholder('Select account')
								]),
							A2(
								$elm$core$List$map,
								function (option) {
									return A2(
										$elm$html$Html$option,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$value(option),
												$elm$html$Html$Attributes$selected(
												_Utils_eq(option, model.intoAccount))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												(option === '') ? 'Select account' : option)
											]));
								},
								$elm$core$List$concat(
									_List_fromArray(
										[
											_List_fromArray(
											['']),
											globals.accounts
										]))))),
						A2(
						$author$project$Page$formElement,
						'When?',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.date),
									$elm$html$Html$Events$onInput($author$project$IncomePage$UpdateDate),
									$elm$html$Html$Attributes$type_('date')
								]),
							_List_Nil)),
						A2(
						$author$project$Page$formElement,
						'Deets?',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.info),
									$elm$html$Html$Events$onInput($author$project$IncomePage$UpdateInfo)
								]),
							_List_Nil)),
						A2(
						$author$project$Page$formElement,
						'Notes (optional)',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.notes),
									$elm$html$Html$Events$onInput($author$project$IncomePage$UpdateNotes)
								]),
							_List_Nil)),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('flex justify-around gap-4')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$IncomePage$GoToHomePage)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Back')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('primary'),
										$elm$html$Html$Attributes$disabled(
										(!$author$project$IncomePage$isFormValid(model)) || _Utils_eq(model.pageState, $author$project$IncomePage$Saving)),
										$elm$html$Html$Events$onClick($author$project$IncomePage$SaveIncome)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Save')
									]))
							]))
					])));
	});
var $author$project$RecentsPage$GoToHomePage = {$: 'GoToHomePage'};
var $author$project$RecentsPage$txnCard = function (txn) {
	var withDefault = $elm$core$Maybe$withDefault('');
	var dict = $elm$core$Dict$fromList(
		A2(
			$elm$core$List$indexedMap,
			F2(
				function (idx, val) {
					return _Utils_Tuple2(
						$elm$core$String$fromInt(idx),
						val);
				}),
			txn));
	var note = withDefault(
		A2($elm$core$Dict$get, '1', dict));
	var xpns = withDefault(
		A2($elm$core$Dict$get, '3', dict));
	var date = withDefault(
		A2($elm$core$Dict$get, '0', dict));
	var acc = withDefault(
		A2($elm$core$Dict$get, '4', dict));
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('text-xs text-gray-500')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(date + (' | ' + acc))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(xpns)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('text-xs text-gray-500')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(note)
					]))
			]));
};
var $author$project$RecentsPage$renderBody = function (_v0) {
	var transactions = _v0.transactions;
	switch (transactions.$) {
		case 'Transactions':
			var txns = transactions.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('flex flex-col gap-10')
					]),
				A2($elm$core$List$map, $author$project$RecentsPage$txnCard, txns));
		case 'Loading':
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Loading recent transactions...')
					]));
		default:
			var str = transactions.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(str)
					]));
	}
};
var $author$project$RecentsPage$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Page$renderHeader(
				{icon: $feathericons$elm_feather$FeatherIcons$home, msg: $author$project$RecentsPage$GoToHomePage, text: 'Recent Txns'}),
				$author$project$Page$renderBody(
				$author$project$RecentsPage$renderBody(model))
			]));
};
var $author$project$SettingsPage$Reset = {$: 'Reset'};
var $author$project$SettingsPage$SaveSheetSettings = {$: 'SaveSheetSettings'};
var $author$project$SettingsPage$UpdateAccountSheetName = function (a) {
	return {$: 'UpdateAccountSheetName', a: a};
};
var $author$project$SettingsPage$UpdateExpenseSheetName = function (a) {
	return {$: 'UpdateExpenseSheetName', a: a};
};
var $author$project$SettingsPage$UpdateSheetId = function (a) {
	return {$: 'UpdateSheetId', a: a};
};
var $author$project$SettingsPage$GoToHomePage = {$: 'GoToHomePage'};
var $author$project$SettingsPage$pageHeader = {icon: $feathericons$elm_feather$FeatherIcons$home, msg: $author$project$SettingsPage$GoToHomePage, text: 'Settings'};
var $author$project$SettingsPage$pageWrapper = function (body) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Page$renderHeader($author$project$SettingsPage$pageHeader),
				$author$project$Page$renderBody(body)
			]));
};
var $author$project$SettingsPage$view = function (_v0) {
	var sheetId = _v0.sheetId;
	var accountSheet = _v0.accountSheet;
	var expenseSheet = _v0.expenseSheet;
	return $author$project$SettingsPage$pageWrapper(
		A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('flex flex-col gap-10')
				]),
			_List_fromArray(
				[
					A2(
					$author$project$Page$formElement,
					'Sheet ID:',
					A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value(sheetId),
								$elm$html$Html$Events$onInput($author$project$SettingsPage$UpdateSheetId)
							]),
						_List_Nil)),
					A2(
					$author$project$Page$formElement,
					'Accounts sheet name:',
					A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value(accountSheet),
								$elm$html$Html$Events$onInput($author$project$SettingsPage$UpdateAccountSheetName)
							]),
						_List_Nil)),
					A2(
					$author$project$Page$formElement,
					'Expenses sheet name:',
					A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$value(expenseSheet),
								$elm$html$Html$Events$onInput($author$project$SettingsPage$UpdateExpenseSheetName)
							]),
						_List_Nil)),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('flex justify-around gap-4')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('primary'),
									$elm$html$Html$Events$onClick($author$project$SettingsPage$SaveSheetSettings)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Save')
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Events$onClick($author$project$SettingsPage$Reset)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Reset')
								]))
						]))
				])));
};
var $author$project$TransferPage$SaveTransfer = {$: 'SaveTransfer'};
var $author$project$TransferPage$UpdateAmount = function (a) {
	return {$: 'UpdateAmount', a: a};
};
var $author$project$TransferPage$UpdateDate = function (a) {
	return {$: 'UpdateDate', a: a};
};
var $author$project$TransferPage$UpdateFromAccount = function (a) {
	return {$: 'UpdateFromAccount', a: a};
};
var $author$project$TransferPage$UpdateInfo = function (a) {
	return {$: 'UpdateInfo', a: a};
};
var $author$project$TransferPage$UpdateNotes = function (a) {
	return {$: 'UpdateNotes', a: a};
};
var $author$project$TransferPage$UpdateToAccount = function (a) {
	return {$: 'UpdateToAccount', a: a};
};
var $author$project$TransferPage$isFormValid = function (model) {
	var modelAsNumber = function () {
		var _v0 = $elm$core$String$toFloat(model.amount);
		if (_v0.$ === 'Nothing') {
			return false;
		} else {
			return true;
		}
	}();
	var dateIsValid = !$elm$core$String$isEmpty(model.date);
	var accountsValid = !($elm$core$String$isEmpty(model.toAccount) || $elm$core$String$isEmpty(model.fromAccount));
	var accountsNotSame = !_Utils_eq(model.toAccount, model.fromAccount);
	return dateIsValid && (modelAsNumber && (accountsValid && accountsNotSame));
};
var $author$project$TransferPage$pageHeader = $author$project$Page$renderHeader(
	{icon: $feathericons$elm_feather$FeatherIcons$home, msg: $author$project$TransferPage$GoToHomePage, text: 'Transfer'});
var $author$project$TransferPage$pageWrapper = function (body) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$TransferPage$pageHeader,
				$author$project$Page$renderBody(body)
			]));
};
var $author$project$TransferPage$view = F2(
	function (globals, model) {
		return $author$project$TransferPage$pageWrapper(
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('flex flex-col gap-5')
					]),
				_List_fromArray(
					[
						(globals.sheetError !== '') ? $author$project$ExpenseTracker$showError(globals.sheetError) : ((globals.sheetId === '') ? $author$project$ExpenseTracker$showError('No sheet ID.') : $elm$html$Html$text('')),
						A2(
						$author$project$Page$formElement,
						'From which account?',
						A2(
							$elm$html$Html$select,
							_List_fromArray(
								[
									$elm$html$Html$Events$onInput($author$project$TransferPage$UpdateFromAccount),
									$elm$html$Html$Attributes$placeholder('Select account')
								]),
							A2(
								$elm$core$List$map,
								function (option) {
									return A2(
										$elm$html$Html$option,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$value(option),
												$elm$html$Html$Attributes$selected(
												_Utils_eq(option, model.fromAccount))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												(option === '') ? 'Select account' : option)
											]));
								},
								$elm$core$List$concat(
									_List_fromArray(
										[
											_List_fromArray(
											['']),
											globals.accounts
										]))))),
						A2(
						$author$project$Page$formElement,
						'To which account?',
						A2(
							$elm$html$Html$select,
							_List_fromArray(
								[
									$elm$html$Html$Events$onInput($author$project$TransferPage$UpdateToAccount),
									$elm$html$Html$Attributes$placeholder('Select account')
								]),
							A2(
								$elm$core$List$map,
								function (option) {
									return A2(
										$elm$html$Html$option,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$value(option),
												$elm$html$Html$Attributes$selected(
												_Utils_eq(option, model.toAccount))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												(option === '') ? 'Select account' : option)
											]));
								},
								$elm$core$List$concat(
									_List_fromArray(
										[
											_List_fromArray(
											['']),
											globals.accounts
										]))))),
						A2(
						$author$project$Page$formElement,
						'How much?',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.amount),
									$elm$html$Html$Events$onInput($author$project$TransferPage$UpdateAmount),
									$elm$html$Html$Attributes$type_('number')
								]),
							_List_Nil)),
						A2(
						$author$project$Page$formElement,
						'When?',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.date),
									$elm$html$Html$Events$onInput($author$project$TransferPage$UpdateDate),
									$elm$html$Html$Attributes$type_('date')
								]),
							_List_Nil)),
						A2(
						$author$project$Page$formElement,
						'Deets? (optional)',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.info),
									$elm$html$Html$Events$onInput($author$project$TransferPage$UpdateInfo)
								]),
							_List_Nil)),
						A2(
						$author$project$Page$formElement,
						'Notes (optional)',
						A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(model.notes),
									$elm$html$Html$Events$onInput($author$project$TransferPage$UpdateNotes)
								]),
							_List_Nil)),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('flex justify-around gap-4')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$TransferPage$GoToHomePage)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Back')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('primary'),
										$elm$html$Html$Attributes$disabled(
										(!$author$project$TransferPage$isFormValid(model)) || _Utils_eq(model.pageState, $author$project$TransferPage$Saving)),
										$elm$html$Html$Events$onClick($author$project$TransferPage$SaveTransfer)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Save')
									]))
							]))
					])));
	});
var $author$project$Main$DoLogin = {$: 'DoLogin'};
var $feathericons$elm_feather$FeatherIcons$user = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'user',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('7'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil)
		]));
var $author$project$Main$viewLoginPage = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('flex flex-col h-screen items-center gap-10 justify-center')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$author$project$Page$renderIcon(
					{icon: $feathericons$elm_feather$FeatherIcons$user, size: 42})
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('primary'),
					$elm$html$Html$Events$onClick($author$project$Main$DoLogin)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Login')
				]))
		]));
var $author$project$Main$view = function (model) {
	var pageContent = function () {
		var _v0 = model.currentPage;
		switch (_v0.$) {
			case 'Loading':
				return $author$project$Page$pageLoaderDiv;
			case 'Login':
				return $author$project$Main$viewLoginPage;
			case 'HomePage':
				return A2($elm$html$Html$map, $author$project$Main$HomePageMsg, $author$project$HomePage$view);
			case 'ExpenseTrackerPage':
				return A2(
					$elm$html$Html$map,
					$author$project$Main$ExpenseTrackerPageMsg,
					A2(
						$author$project$ExpenseTracker$view,
						A6($author$project$Page$Global, model.auth.token, model.sheetSettings.sheetId, model.accounts, model.sheetError, model.sheetSettings.accountSheet, model.sheetSettings.expenseSheet),
						model.expenseTracker));
			case 'SettingsPage':
				return A2(
					$elm$html$Html$map,
					$author$project$Main$SettingsPageMsg,
					$author$project$SettingsPage$view(model.sheetSettings));
			case 'TransferPage':
				return A2(
					$elm$html$Html$map,
					$author$project$Main$TransferPageMsg,
					A2(
						$author$project$TransferPage$view,
						$author$project$Main$getGlobals(model),
						model.transferPage));
			case 'IncomePage':
				return A2(
					$elm$html$Html$map,
					$author$project$Main$IncomePageMsg,
					A2(
						$author$project$IncomePage$view,
						$author$project$Main$getGlobals(model),
						model.incomePage));
			case 'GlobalError':
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Page$renderBody(
							A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('flex flex-col gap-10')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Something went wrong catastrophically. Try logging in again.'),
										A2(
										$elm$html$Html$div,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('primary'),
														$elm$html$Html$Events$onClick($author$project$Main$Logout)
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Login again ->')
													]))
											]))
									])))
						]));
			case 'RecentsPage':
				return A2(
					$elm$html$Html$map,
					$author$project$Main$RecentsPageMsg,
					$author$project$RecentsPage$view(model.recentsPage));
			default:
				return A2(
					$elm$html$Html$map,
					$author$project$Main$AccountsPageMsg,
					$author$project$AccountsPage$view(model.accountsPage));
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				pageContent,
				$author$project$Page$viewToaster(model.toastMsg)
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));