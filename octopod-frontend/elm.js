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
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
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



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});



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
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $author$project$Main$LinkClicked = function (a) {
	return {$: 'LinkClicked', a: a};
};
var $author$project$Main$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
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
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Main$Initialization = function (a) {
	return {$: 'Initialization', a: a};
};
var $author$project$Main$InitializationMsg = function (a) {
	return {$: 'InitializationMsg', a: a};
};
var $author$project$Page$Initialization$AdjustTimeZone = function (a) {
	return {$: 'AdjustTimeZone', a: a};
};
var $krisajenkins$remotedata$RemoteData$Loading = {$: 'Loading'};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $author$project$Config$emptySettings = function (key) {
	return {navKey: key, projectName: '', zone: $elm$time$Time$utc};
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$here = _Time_here(_Utils_Tuple0);
var $author$project$Page$Initialization$init = F2(
	function (url, key) {
		return _Utils_Tuple2(
			{
				config: $krisajenkins$remotedata$RemoteData$Loading,
				projectName: $krisajenkins$remotedata$RemoteData$Loading,
				settings: $author$project$Config$emptySettings(key),
				url: url
			},
			A2($elm$core$Task$perform, $author$project$Page$Initialization$AdjustTimeZone, $elm$time$Time$here));
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Main$updateWith = F3(
	function (toModel, toMsg, _v0) {
		var subModel = _v0.a;
		var subCmd = _v0.b;
		return _Utils_Tuple2(
			toModel(subModel),
			A2($elm$core$Platform$Cmd$map, toMsg, subCmd));
	});
var $author$project$Main$init = F3(
	function (_v0, url, key) {
		return A3(
			$author$project$Main$updateWith,
			$author$project$Main$Initialization,
			$author$project$Main$InitializationMsg,
			A2($author$project$Page$Initialization$init, url, key));
	});
var $author$project$Main$DeploymentMsg = function (a) {
	return {$: 'DeploymentMsg', a: a};
};
var $author$project$Main$DeploymentsMsg = function (a) {
	return {$: 'DeploymentsMsg', a: a};
};
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Page$Deployment$WSUpdate = function (a) {
	return {$: 'WSUpdate', a: a};
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Page$Deployment$deploymentReceiver = _Platform_incomingPort('deploymentReceiver', $elm$json$Json$Decode$string);
var $author$project$Page$Deployment$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Page$Deployment$deploymentReceiver($author$project$Page$Deployment$WSUpdate)
			]));
};
var $author$project$Page$Deployments$ActiveTableMsg = function (a) {
	return {$: 'ActiveTableMsg', a: a};
};
var $author$project$Page$Deployments$ArchivedTableMsg = function (a) {
	return {$: 'ArchivedTableMsg', a: a};
};
var $author$project$Page$Deployments$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $author$project$Page$Deployments$WSUpdate = function (a) {
	return {$: 'WSUpdate', a: a};
};
var $author$project$Page$Deployments$deploymentsReceiver = _Platform_incomingPort('deploymentsReceiver', $elm$json$Json$Decode$string);
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
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
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $author$project$Page$Deployments$Table$getDropdownId = function (model) {
	var _v0 = model.tableType;
	if (_v0.$ === 'ActiveTable') {
		return 'active-dropdown';
	} else {
		return 'archived-dropdown';
	}
};
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
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
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
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
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onMouseDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousedown');
var $author$project$Page$Deployments$Table$CloseMenu = {$: 'CloseMenu'};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		$elm$json$Json$Decode$andThen,
		thunk,
		$elm$json$Json$Decode$succeed(_Utils_Tuple0));
};
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $author$project$Page$Deployments$Table$isOutsideDropdown = function (dropdownId) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$andThen,
				function (id) {
					return _Utils_eq(dropdownId, id) ? $elm$json$Json$Decode$succeed(false) : $elm$json$Json$Decode$fail('check parent node');
				},
				A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string)),
				$elm$json$Json$Decode$lazy(
				function (_v0) {
					return A2(
						$elm$json$Json$Decode$field,
						'parentNode',
						$author$project$Page$Deployments$Table$isOutsideDropdown(dropdownId));
				}),
				$elm$json$Json$Decode$succeed(true)
			]));
};
var $author$project$Page$Deployments$Table$outsideTarget = function (dropdownId) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (isOutside) {
			return isOutside ? $elm$json$Json$Decode$succeed($author$project$Page$Deployments$Table$CloseMenu) : $elm$json$Json$Decode$fail('inside dropdown');
		},
		A2(
			$elm$json$Json$Decode$field,
			'target',
			$author$project$Page$Deployments$Table$isOutsideDropdown(dropdownId)));
};
var $author$project$Page$Deployments$Table$subscriptions = function (model) {
	var _v0 = model.menuButton;
	if (_v0.$ === 'Just') {
		return $elm$browser$Browser$Events$onMouseDown(
			$author$project$Page$Deployments$Table$outsideTarget(
				$author$project$Page$Deployments$Table$getDropdownId(model)));
	} else {
		return $elm$core$Platform$Sub$none;
	}
};
var $author$project$Page$Deployments$tick = 15;
var $author$project$Page$Deployments$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				$elm$core$Platform$Sub$map,
				$author$project$Page$Deployments$ActiveTableMsg,
				$author$project$Page$Deployments$Table$subscriptions(model.activeTable)),
				A2(
				$elm$core$Platform$Sub$map,
				$author$project$Page$Deployments$ArchivedTableMsg,
				$author$project$Page$Deployments$Table$subscriptions(model.archivedTable)),
				$author$project$Page$Deployments$deploymentsReceiver($author$project$Page$Deployments$WSUpdate),
				A2($elm$time$Time$every, $author$project$Page$Deployments$tick * 1000, $author$project$Page$Deployments$Tick)
			]));
};
var $author$project$Main$subscriptions = function (model) {
	switch (model.$) {
		case 'Initialization':
			return $elm$core$Platform$Sub$none;
		case 'Deployments':
			var deployments = model.a;
			return A2(
				$elm$core$Platform$Sub$map,
				$author$project$Main$DeploymentsMsg,
				$author$project$Page$Deployments$subscriptions(deployments));
		default:
			var deployment = model.a;
			return A2(
				$elm$core$Platform$Sub$map,
				$author$project$Main$DeploymentMsg,
				$author$project$Page$Deployment$subscriptions(deployment));
	}
};
var $author$project$Main$Deployment = function (a) {
	return {$: 'Deployment', a: a};
};
var $author$project$Main$Deployments = function (a) {
	return {$: 'Deployments', a: a};
};
var $author$project$Page$Deployment$getConfig = function (model) {
	return model.config;
};
var $author$project$Page$Deployments$getConfig = function (model) {
	return model.config;
};
var $author$project$Config$AppAuth = function (a) {
	return {$: 'AppAuth', a: a};
};
var $author$project$Config$AppUrl = function (a) {
	return {$: 'AppUrl', a: a};
};
var $author$project$Config$Config = F4(
	function (appUrl, wsUrl, appAuth, k8sDashboardUrlTemplate) {
		return {appAuth: appAuth, appUrl: appUrl, k8sDashboardUrlTemplate: k8sDashboardUrlTemplate, wsUrl: wsUrl};
	});
var $author$project$Config$emptyConfig = A4(
	$author$project$Config$Config,
	$author$project$Config$AppUrl(''),
	'',
	$author$project$Config$AppAuth(''),
	'');
var $author$project$Page$Initialization$getConfig = function (model) {
	var _v0 = model.config;
	if (_v0.$ === 'Success') {
		var cfg = _v0.a;
		return cfg;
	} else {
		return $author$project$Config$emptyConfig;
	}
};
var $author$project$Main$getConfig = function (model) {
	switch (model.$) {
		case 'Initialization':
			var subModel = model.a;
			return $author$project$Page$Initialization$getConfig(subModel);
		case 'Deployments':
			var subModel = model.a;
			return $author$project$Page$Deployments$getConfig(subModel);
		default:
			var subModel = model.a;
			return $author$project$Page$Deployment$getConfig(subModel);
	}
};
var $author$project$Page$Deployment$getSettings = function (model) {
	return model.settings;
};
var $author$project$Page$Deployments$getSettings = function (model) {
	return model.settings;
};
var $author$project$Page$Initialization$getSettings = function (model) {
	return model.settings;
};
var $author$project$Main$getSettings = function (model) {
	switch (model.$) {
		case 'Initialization':
			var subModel = model.a;
			return $author$project$Page$Initialization$getSettings(subModel);
		case 'Deployments':
			var subModel = model.a;
			return $author$project$Page$Deployments$getSettings(subModel);
		default:
			var subModel = model.a;
			return $author$project$Page$Deployment$getSettings(subModel);
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $jinjor$elm_debounce$Debounce$Debounce = function (a) {
	return {$: 'Debounce', a: a};
};
var $jinjor$elm_debounce$Debounce$init = $jinjor$elm_debounce$Debounce$Debounce(
	{input: _List_Nil, locked: false});
var $turboMaCk$any_set$Set$Any$AnySet = function (a) {
	return {$: 'AnySet', a: a};
};
var $turboMaCk$any_dict$Dict$Any$AnyDict = function (a) {
	return {$: 'AnyDict', a: a};
};
var $turboMaCk$any_dict$Dict$Any$empty = function (toKey) {
	return $turboMaCk$any_dict$Dict$Any$AnyDict(
		{dict: $elm$core$Dict$empty, toKey: toKey});
};
var $turboMaCk$any_set$Set$Any$empty = A2($elm$core$Basics$composeL, $turboMaCk$any_set$Set$Any$AnySet, $turboMaCk$any_dict$Dict$Any$empty);
var $author$project$Types$Action$unActionId = function (_v0) {
	var id = _v0.a;
	return id;
};
var $author$project$Page$Deployment$ActionTable$init = {
	openedAppOverrides: $turboMaCk$any_set$Set$Any$empty($author$project$Types$Action$unActionId),
	openedDeployemntOverrides: $turboMaCk$any_set$Set$Any$empty($author$project$Types$Action$unActionId)
};
var $krisajenkins$remotedata$RemoteData$NotAsked = {$: 'NotAsked'};
var $author$project$Page$Sidebar$CreateUpdate$Update = {$: 'Update'};
var $author$project$Page$Sidebar$CreateUpdate$initUpdate = F3(
	function (config, visibility, deploymentName) {
		return {appDefaults: $krisajenkins$remotedata$RemoteData$Loading, appKeys: $krisajenkins$remotedata$RemoteData$Loading, appOverrides: $krisajenkins$remotedata$RemoteData$Loading, config: config, deployment: $krisajenkins$remotedata$RemoteData$Loading, deploymentDefaults: $krisajenkins$remotedata$RemoteData$Loading, deploymentKeys: $krisajenkins$remotedata$RemoteData$Loading, deploymentOverrides: $krisajenkins$remotedata$RemoteData$Loading, mode: $author$project$Page$Sidebar$CreateUpdate$Update, name: deploymentName, nameEdited: false, saveResp: $krisajenkins$remotedata$RemoteData$NotAsked, visibility: visibility};
	});
var $author$project$Page$Deployment$DeploymentFullInfoResponse = function (a) {
	return {$: 'DeploymentFullInfoResponse', a: a};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Types$Deployment$Deployment = F5(
	function (deployment, status, metadata, createdAt, updatedAt) {
		return {createdAt: createdAt, deployment: deployment, metadata: metadata, status: status, updatedAt: updatedAt};
	});
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					var _v2 = callback(a);
					var parseB = _v2.a;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var $elm$parser$Parser$Advanced$end = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				$elm$core$String$length(s.src),
				s.offset) ? A3($elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $elm$core$Basics$round = _Basics_round;
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $elm$core$String$toFloat = _String_toFloat;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs = A2(
	$elm$parser$Parser$andThen,
	function (str) {
		if ($elm$core$String$length(str) <= 9) {
			var _v0 = $elm$core$String$toFloat('0.' + str);
			if (_v0.$ === 'Just') {
				var floatVal = _v0.a;
				return $elm$parser$Parser$succeed(
					$elm$core$Basics$round(floatVal * 1000));
			} else {
				return $elm$parser$Parser$problem('Invalid float: \"' + (str + '\"'));
			}
		} else {
			return $elm$parser$Parser$problem(
				'Expected at most 9 digits, but got ' + $elm$core$String$fromInt(
					$elm$core$String$length(str)));
		}
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompWhile($elm$core$Char$isDigit)));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts = F6(
	function (monthYearDayMs, hour, minute, second, ms, utcOffsetMinutes) {
		return $elm$time$Time$millisToPosix((((monthYearDayMs + (((hour * 60) * 60) * 1000)) + (((minute - utcOffsetMinutes) * 60) * 1000)) + (second * 1000)) + ms);
	});
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var p = _v2.a;
					var x = _v2.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v2.a;
					var a = _v2.b;
					var s1 = _v2.c;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3(
							$elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $elm$parser$Parser$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$core$String$append = _String_append;
var $elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0.a;
			var _v1 = parse(s0);
			if (_v1.$ === 'Good') {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (step.$ === 'Loop') {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
			});
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (step.$ === 'Loop') {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt = function (quantity) {
	var helper = function (str) {
		if (_Utils_eq(
			$elm$core$String$length(str),
			quantity)) {
			var _v0 = $elm$core$String$toInt(str);
			if (_v0.$ === 'Just') {
				var intVal = _v0.a;
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$Done,
					$elm$parser$Parser$succeed(intVal));
			} else {
				return $elm$parser$Parser$problem('Invalid integer: \"' + (str + '\"'));
			}
		} else {
			return A2(
				$elm$parser$Parser$map,
				function (nextChar) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$String$append, str, nextChar));
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
		}
	};
	return A2($elm$parser$Parser$loop, '', helper);
};
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear = 1970;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay = function (day) {
	return $elm$parser$Parser$problem(
		'Invalid day: ' + $elm$core$String$fromInt(day));
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$Basics$neq = _Utils_notEqual;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear = function (year) {
	return (!A2($elm$core$Basics$modBy, 4, year)) && ((!(!A2($elm$core$Basics$modBy, 100, year))) || (!A2($elm$core$Basics$modBy, 400, year)));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore = function (y1) {
	var y = y1 - 1;
	return (((y / 4) | 0) - ((y / 100) | 0)) + ((y / 400) | 0);
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay = 86400000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear = 31536000000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay = function (_v0) {
	var year = _v0.a;
	var month = _v0.b;
	var dayInMonth = _v0.c;
	if (dayInMonth < 0) {
		return $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth);
	} else {
		var succeedWith = function (extraMs) {
			var yearMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear * (year - $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear);
			var days = ((month < 3) || (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year))) ? (dayInMonth - 1) : dayInMonth;
			var dayMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay * (days + ($rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore(year) - $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore($rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear)));
			return $elm$parser$Parser$succeed((extraMs + yearMs) + dayMs);
		};
		switch (month) {
			case 1:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(0);
			case 2:
				return ((dayInMonth > 29) || ((dayInMonth === 29) && (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year)))) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(2678400000);
			case 3:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(5097600000);
			case 4:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(7776000000);
			case 5:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(10368000000);
			case 6:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(13046400000);
			case 7:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(15638400000);
			case 8:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(18316800000);
			case 9:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(20995200000);
			case 10:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(23587200000);
			case 11:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(26265600000);
			case 12:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(28857600000);
			default:
				return $elm$parser$Parser$problem(
					'Invalid month: \"' + ($elm$core$String$fromInt(month) + '\"'));
		}
	}
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs = A2(
	$elm$parser$Parser$andThen,
	$rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F3(
						function (year, month, day) {
							return _Utils_Tuple3(year, month, day);
						})),
				$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(4)),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed($elm$core$Basics$identity),
							$elm$parser$Parser$symbol('-')),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
					]))),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($elm$core$Basics$identity),
						$elm$parser$Parser$symbol('-')),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
				]))));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes = function () {
	var utcOffsetMinutesFromParts = F3(
		function (multiplier, hours, minutes) {
			return (multiplier * (hours * 60)) + minutes;
		});
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return 0;
					},
					$elm$parser$Parser$symbol('Z')),
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(utcOffsetMinutesFromParts),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$map,
										function (_v1) {
											return 1;
										},
										$elm$parser$Parser$symbol('+')),
										A2(
										$elm$parser$Parser$map,
										function (_v2) {
											return -1;
										},
										$elm$parser$Parser$symbol('-'))
									]))),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									$elm$parser$Parser$succeed($elm$core$Basics$identity),
									$elm$parser$Parser$symbol(':')),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
								$elm$parser$Parser$succeed(0)
							]))),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(0),
					$elm$parser$Parser$end)
				])));
}();
var $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601 = A2(
	$elm$parser$Parser$andThen,
	function (datePart) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed(
											$rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts(datePart)),
										$elm$parser$Parser$symbol('T')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$elm$parser$Parser$oneOf(
									_List_fromArray(
										[
											A2(
											$elm$parser$Parser$keeper,
											A2(
												$elm$parser$Parser$ignorer,
												$elm$parser$Parser$succeed($elm$core$Basics$identity),
												$elm$parser$Parser$symbol(':')),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
										]))),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$keeper,
										A2(
											$elm$parser$Parser$ignorer,
											$elm$parser$Parser$succeed($elm$core$Basics$identity),
											$elm$parser$Parser$symbol(':')),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
										$elm$parser$Parser$succeed(0)
									]))),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$symbol('.')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs),
									$elm$parser$Parser$succeed(0)
								]))),
					A2($elm$parser$Parser$ignorer, $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes, $elm$parser$Parser$end)),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						A6($rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts, datePart, 0, 0, 0, 0, 0)),
					$elm$parser$Parser$end)
				]));
	},
	$rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs);
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (_v0.$ === 'Ok') {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$toTime = function (str) {
	return A2($elm$parser$Parser$run, $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601, str);
};
var $elm_community$json_extra$Json$Decode$Extra$datetime = A2(
	$elm$json$Json$Decode$andThen,
	function (dateString) {
		var _v0 = $rtfeldman$elm_iso8601_date_strings$Iso8601$toTime(dateString);
		if (_v0.$ === 'Ok') {
			var v = _v0.a;
			return $elm$json$Json$Decode$succeed(v);
		} else {
			return $elm$json$Json$Decode$fail('Expecting an ISO-8601 formatted date+time string');
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Types$Deployment$DeploymentName = function (a) {
	return {$: 'DeploymentName', a: a};
};
var $author$project$Types$Deployment$Info = F3(
	function (name, appOverrides, deploymentOverrides) {
		return {appOverrides: appOverrides, deploymentOverrides: deploymentOverrides, name: name};
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Types$Override$Override = F2(
	function (name, value) {
		return {name: name, value: value};
	});
var $author$project$Types$Override$OverrideName = function (a) {
	return {$: 'OverrideName', a: a};
};
var $author$project$Types$Override$OverrideName_ = function (a) {
	return {$: 'OverrideName_', a: a};
};
var $author$project$Types$Override$OverrideValue_ = function (a) {
	return {$: 'OverrideValue_', a: a};
};
var $author$project$Types$Override$ValueAdded = function (a) {
	return {$: 'ValueAdded', a: a};
};
var $author$project$Types$Override$ValueDeleted = {$: 'ValueDeleted'};
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder = F3(
	function (path, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return $elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						decoder,
						$elm$json$Json$Decode$null(fallback)
					]));
		};
		var handleResult = function (input) {
			var _v0 = A2(
				$elm$json$Json$Decode$decodeValue,
				A2($elm$json$Json$Decode$at, path, $elm$json$Json$Decode$value),
				input);
			if (_v0.$ === 'Ok') {
				var rawValue = _v0.a;
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					nullOr(valDecoder),
					rawValue);
				if (_v1.$ === 'Ok') {
					var finalResult = _v1.a;
					return $elm$json$Json$Decode$succeed(finalResult);
				} else {
					return A2(
						$elm$json$Json$Decode$at,
						path,
						nullOr(valDecoder));
				}
			} else {
				return $elm$json$Json$Decode$succeed(fallback);
			}
		};
		return A2($elm$json$Json$Decode$andThen, handleResult, $elm$json$Json$Decode$value);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder,
				_List_fromArray(
					[key]),
				valDecoder,
				fallback),
			decoder);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2($elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$resolve = $elm$json$Json$Decode$andThen($elm$core$Basics$identity);
var $author$project$Types$Override$overrideValueDecoder = function () {
	var toDecoder = F2(
		function (tag, contents) {
			var _v0 = _Utils_Tuple2(tag, contents);
			_v0$2:
			while (true) {
				switch (_v0.a) {
					case 'ValueDeleted':
						return $elm$json$Json$Decode$succeed($author$project$Types$Override$ValueDeleted);
					case 'ValueAdded':
						if (_v0.b.$ === 'Just') {
							var value = _v0.b.a;
							return $elm$json$Json$Decode$succeed(
								$author$project$Types$Override$ValueAdded(value));
						} else {
							break _v0$2;
						}
					default:
						break _v0$2;
				}
			}
			return $elm$json$Json$Decode$fail('Unknown DeploymentStatus: ' + tag);
		});
	return $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$resolve(
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'contents',
			$elm$json$Json$Decode$maybe($elm$json$Json$Decode$string),
			$elm$core$Maybe$Nothing,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'tag',
				$elm$json$Json$Decode$string,
				$elm$json$Json$Decode$succeed(toDecoder))));
}();
var $author$project$Types$Override$overrideDecoder = function () {
	var overrideHelperDecoder = $elm$json$Json$Decode$list(
		$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2($elm$json$Json$Decode$map, $author$project$Types$Override$OverrideName_, $elm$json$Json$Decode$string),
					A2($elm$json$Json$Decode$map, $author$project$Types$Override$OverrideValue_, $author$project$Types$Override$overrideValueDecoder)
				])));
	var listDecoder = function (vals) {
		_v0$2:
		while (true) {
			if (vals.b) {
				if (vals.a.$ === 'OverrideName_') {
					if ((vals.b.b && (vals.b.a.$ === 'OverrideValue_')) && (!vals.b.b.b)) {
						var name = vals.a.a;
						var _v1 = vals.b;
						var val = _v1.a.a;
						return $elm$json$Json$Decode$succeed(
							A2(
								$author$project$Types$Override$Override,
								$author$project$Types$Override$OverrideName(name),
								val));
					} else {
						break _v0$2;
					}
				} else {
					if ((vals.b.b && (vals.b.a.$ === 'OverrideName_')) && (!vals.b.b.b)) {
						var val = vals.a.a;
						var _v2 = vals.b;
						var name = _v2.a.a;
						return $elm$json$Json$Decode$succeed(
							A2(
								$author$project$Types$Override$Override,
								$author$project$Types$Override$OverrideName(name),
								val));
					} else {
						break _v0$2;
					}
				}
			} else {
				break _v0$2;
			}
		}
		return $elm$json$Json$Decode$fail('Unknown override');
	};
	return A2($elm$json$Json$Decode$andThen, listDecoder, overrideHelperDecoder);
}();
var $author$project$Types$Deployment$infoDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'deployment_overrides',
	$elm$json$Json$Decode$list($author$project$Types$Override$overrideDecoder),
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'app_overrides',
		$elm$json$Json$Decode$list($author$project$Types$Override$overrideDecoder),
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'name',
			A2($elm$json$Json$Decode$map, $author$project$Types$Deployment$DeploymentName, $elm$json$Json$Decode$string),
			$elm$json$Json$Decode$succeed($author$project$Types$Deployment$Info))));
var $author$project$Types$Deployment$Metadata = F2(
	function (name, link) {
		return {link: link, name: name};
	});
var $author$project$Types$Deployment$metadataDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'link',
	$elm$json$Json$Decode$string,
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'name',
		$elm$json$Json$Decode$string,
		$elm$json$Json$Decode$succeed($author$project$Types$Deployment$Metadata)));
var $author$project$Types$Deployment$DeploymentNotPending = function (a) {
	return {$: 'DeploymentNotPending', a: a};
};
var $author$project$Types$Deployment$DeploymentPending = function (a) {
	return {$: 'DeploymentPending', a: a};
};
var $author$project$Types$Deployment$ArchivePending = {$: 'ArchivePending'};
var $author$project$Types$Deployment$Archived = {$: 'Archived'};
var $author$project$Types$Deployment$CleanupFailed = {$: 'CleanupFailed'};
var $author$project$Types$Deployment$CreatePending = {$: 'CreatePending'};
var $author$project$Types$Deployment$Failure = function (a) {
	return {$: 'Failure', a: a};
};
var $author$project$Types$Deployment$Running = {$: 'Running'};
var $author$project$Types$Deployment$UpdatePending = {$: 'UpdatePending'};
var $author$project$Types$Deployment$GenericFailure = {$: 'GenericFailure'};
var $author$project$Types$Deployment$PartialAvailability = {$: 'PartialAvailability'};
var $author$project$Types$Deployment$TagMismatch = {$: 'TagMismatch'};
var $author$project$Types$Deployment$failureTypeDecoder = function () {
	var tagDecoder = function (tag) {
		switch (tag) {
			case 'GenericFailure':
				return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$GenericFailure);
			case 'PartialAvailability':
				return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$PartialAvailability);
			case 'TagMismatch':
				return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$TagMismatch);
			default:
				return $elm$json$Json$Decode$fail('Unknown FailureType: ' + tag);
		}
	};
	return A2($elm$json$Json$Decode$andThen, tagDecoder, $elm$json$Json$Decode$string);
}();
var $author$project$Types$Deployment$deploymentStatusDecoder = function () {
	var toDecoder = F2(
		function (tag, contents) {
			var _v0 = _Utils_Tuple2(tag, contents);
			_v0$7:
			while (true) {
				switch (_v0.a) {
					case 'Running':
						return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$Running);
					case 'CreatePending':
						return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$CreatePending);
					case 'UpdatePending':
						return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$UpdatePending);
					case 'ArchivePending':
						return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$ArchivePending);
					case 'Archived':
						return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$Archived);
					case 'CleanupFailed':
						return $elm$json$Json$Decode$succeed($author$project$Types$Deployment$CleanupFailed);
					case 'Failure':
						if (_v0.b.$ === 'Just') {
							var failureType = _v0.b.a;
							return $elm$json$Json$Decode$succeed(
								$author$project$Types$Deployment$Failure(failureType));
						} else {
							break _v0$7;
						}
					default:
						break _v0$7;
				}
			}
			return $elm$json$Json$Decode$fail('Unknown DeploymentStatus: ' + tag);
		});
	return $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$resolve(
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'contents',
			$elm$json$Json$Decode$maybe($author$project$Types$Deployment$failureTypeDecoder),
			$elm$core$Maybe$Nothing,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'tag',
				$elm$json$Json$Decode$string,
				$elm$json$Json$Decode$succeed(toDecoder))));
}();
var $author$project$Types$Deployment$statusDecoder = function () {
	var toDecoder = F2(
		function (tag, contents) {
			switch (tag) {
				case 'DeploymentNotPending':
					return $elm$json$Json$Decode$succeed(
						$author$project$Types$Deployment$DeploymentNotPending(contents));
				case 'DeploymentPending':
					return $elm$json$Json$Decode$succeed(
						$author$project$Types$Deployment$DeploymentPending(contents));
				default:
					return $elm$json$Json$Decode$fail('Unknown Status: ' + tag);
			}
		});
	return $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$resolve(
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'recorded_status',
			$author$project$Types$Deployment$deploymentStatusDecoder,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'tag',
				$elm$json$Json$Decode$string,
				$elm$json$Json$Decode$succeed(toDecoder))));
}();
var $author$project$Types$Deployment$deploymentDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'updated_at',
	$elm_community$json_extra$Json$Decode$Extra$datetime,
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'created_at',
		$elm_community$json_extra$Json$Decode$Extra$datetime,
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'metadata',
			$elm$json$Json$Decode$list($author$project$Types$Deployment$metadataDecoder),
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'status',
				$author$project$Types$Deployment$statusDecoder,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'deployment',
					$author$project$Types$Deployment$infoDecoder,
					$elm$json$Json$Decode$succeed($author$project$Types$Deployment$Deployment))))));
var $author$project$Types$Deployment$unDeploymentName = function (_v0) {
	var name = _v0.a;
	return name;
};
var $author$project$Api$Endpoint$Endpoint = function (a) {
	return {$: 'Endpoint', a: a};
};
var $elm$url$Url$Builder$toQueryPair = function (_v0) {
	var key = _v0.a;
	var value = _v0.b;
	return key + ('=' + value);
};
var $elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			$elm$core$String$join,
			'&',
			A2($elm$core$List$map, $elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var $elm$url$Url$Builder$crossOrigin = F3(
	function (prePath, pathSegments, parameters) {
		return prePath + ('/' + (A2($elm$core$String$join, '/', pathSegments) + $elm$url$Url$Builder$toQuery(parameters)));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Config$unwrapAppUrl = function (_v0) {
	var appUrl = _v0.a;
	return appUrl;
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Api$Endpoint$url = F3(
	function (mAppUrl, paths, queryParams) {
		return $author$project$Api$Endpoint$Endpoint(
			A3(
				$elm$url$Url$Builder$crossOrigin,
				A2(
					$elm$core$Maybe$withDefault,
					'/',
					A2($elm$core$Maybe$map, $author$project$Config$unwrapAppUrl, mAppUrl)),
				paths,
				queryParams));
	});
var $author$project$Api$Endpoint$deploymentFullInfo = F2(
	function (deploymentName, appUrl) {
		return A3(
			$author$project$Api$Endpoint$url,
			$elm$core$Maybe$Just(appUrl),
			_List_fromArray(
				[
					'api',
					'v1',
					'deployments',
					$author$project$Types$Deployment$unDeploymentName(deploymentName),
					'full_info'
				]),
			_List_Nil);
	});
var $krisajenkins$remotedata$RemoteData$Failure = function (a) {
	return {$: 'Failure', a: a};
};
var $krisajenkins$remotedata$RemoteData$Success = function (a) {
	return {$: 'Success', a: a};
};
var $krisajenkins$remotedata$RemoteData$fromResult = function (result) {
	if (result.$ === 'Err') {
		var e = result.a;
		return $krisajenkins$remotedata$RemoteData$Failure(e);
	} else {
		var x = result.a;
		return $krisajenkins$remotedata$RemoteData$Success(x);
	}
};
var $elm$http$Http$Header = F2(
	function (a, b) {
		return {$: 'Header', a: a, b: b};
	});
var $elm$http$Http$header = $elm$http$Http$Header;
var $author$project$Api$authHeader = function (_v0) {
	var appAuth = _v0.a;
	return A2($elm$http$Http$header, 'authorization', appAuth);
};
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
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $author$project$Api$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $author$project$Api$BadStatus = F2(
	function (a, b) {
		return {$: 'BadStatus', a: a, b: b};
	});
var $author$project$Api$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $author$project$Api$NetworkError = {$: 'NetworkError'};
var $author$project$Api$Timeout = {$: 'Timeout'};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $author$project$Api$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			function (response) {
				switch (response.$) {
					case 'BadUrl_':
						var url = response.a;
						return $elm$core$Result$Err(
							$author$project$Api$BadUrl(url));
					case 'Timeout_':
						return $elm$core$Result$Err($author$project$Api$Timeout);
					case 'NetworkError_':
						return $elm$core$Result$Err($author$project$Api$NetworkError);
					case 'BadStatus_':
						var metadata = response.a;
						var body = response.b;
						return $elm$core$Result$Err(
							A2($author$project$Api$BadStatus, metadata.statusCode, body));
					default:
						var body = response.b;
						var _v1 = A2($elm$json$Json$Decode$decodeString, decoder, body);
						if (_v1.$ === 'Ok') {
							var value = _v1.a;
							return $elm$core$Result$Ok(value);
						} else {
							var err = _v1.a;
							return $elm$core$Result$Err(
								$author$project$Api$BadBody(
									$elm$json$Json$Decode$errorToString(err)));
						}
				}
			});
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
var $author$project$Api$Endpoint$unwrap = function (_v0) {
	var str = _v0.a;
	return str;
};
var $author$project$Api$Endpoint$request = function (config) {
	return $elm$http$Http$request(
		{
			body: config.body,
			expect: config.expect,
			headers: config.headers,
			method: config.method,
			timeout: config.timeout,
			tracker: config.tracker,
			url: $author$project$Api$Endpoint$unwrap(config.url)
		});
};
var $author$project$Api$get = F4(
	function (config, url, decoder, msg) {
		return $author$project$Api$Endpoint$request(
			{
				body: $elm$http$Http$emptyBody,
				expect: A2($author$project$Api$expectJson, msg, decoder),
				headers: _List_fromArray(
					[
						$author$project$Api$authHeader(config.appAuth)
					]),
				method: 'GET',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: url(config.appUrl)
			});
	});
var $author$project$Page$Deployment$reqDeployment = F2(
	function (deploymentName, cfg) {
		return A4(
			$author$project$Api$get,
			cfg,
			$author$project$Api$Endpoint$deploymentFullInfo(deploymentName),
			$author$project$Types$Deployment$deploymentDecoder,
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployment$DeploymentFullInfoResponse));
	});
var $author$project$Page$Deployment$DeploymentInfoResponse = function (a) {
	return {$: 'DeploymentInfoResponse', a: a};
};
var $author$project$Api$Endpoint$deploymentInfo = F2(
	function (deploymentName, appUrl) {
		return A3(
			$author$project$Api$Endpoint$url,
			$elm$core$Maybe$Just(appUrl),
			_List_fromArray(
				[
					'api',
					'v1',
					'deployments',
					$author$project$Types$Deployment$unDeploymentName(deploymentName),
					'info'
				]),
			_List_Nil);
	});
var $author$project$Types$Action$LogWrapper = function (logs) {
	return {logs: logs};
};
var $author$project$Types$Action$Log = F7(
	function (action, actionId, createdAt, appOverrides, deploymentOverrides, duration, exitCode) {
		return {action: action, actionId: actionId, appOverrides: appOverrides, createdAt: createdAt, deploymentOverrides: deploymentOverrides, duration: duration, exitCode: exitCode};
	});
var $author$project$Types$Action$ActionId = function (a) {
	return {$: 'ActionId', a: a};
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Types$Action$actionIdDecoder = A2($elm$json$Json$Decode$map, $author$project$Types$Action$ActionId, $elm$json$Json$Decode$int);
var $author$project$Types$Action$Archive = {$: 'Archive'};
var $author$project$Types$Action$Create = {$: 'Create'};
var $author$project$Types$Action$Restore = {$: 'Restore'};
var $author$project$Types$Action$Update = {$: 'Update'};
var $author$project$Types$Action$actionTypeDecoder = function () {
	var tagDecoder = function (tag) {
		switch (tag) {
			case 'CreateAction':
				return $elm$json$Json$Decode$succeed($author$project$Types$Action$Create);
			case 'UpdateAction':
				return $elm$json$Json$Decode$succeed($author$project$Types$Action$Update);
			case 'RestoreAction':
				return $elm$json$Json$Decode$succeed($author$project$Types$Action$Restore);
			case 'ArchiveAction':
				return $elm$json$Json$Decode$succeed($author$project$Types$Action$Archive);
			default:
				return $elm$json$Json$Decode$fail('Unknown ActionType: ' + tag);
		}
	};
	return A2($elm$json$Json$Decode$andThen, tagDecoder, $elm$json$Json$Decode$string);
}();
var $author$project$Types$Action$Duration = F2(
	function (months, time) {
		return {months: months, time: time};
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Types$Action$durationDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'time',
	$elm$json$Json$Decode$float,
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'months',
		$elm$json$Json$Decode$int,
		$elm$json$Json$Decode$succeed($author$project$Types$Action$Duration)));
var $author$project$Types$Action$ExitCode = function (a) {
	return {$: 'ExitCode', a: a};
};
var $author$project$Types$Action$exitCodeDecocder = A2($elm$json$Json$Decode$map, $author$project$Types$Action$ExitCode, $elm$json$Json$Decode$int);
var $author$project$Types$Action$logDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'exit_code',
	$author$project$Types$Action$exitCodeDecocder,
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'duration',
		$author$project$Types$Action$durationDecoder,
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'deployment_dep_overrides',
			$elm$json$Json$Decode$list($author$project$Types$Override$overrideDecoder),
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'deployment_app_overrides',
				$elm$json$Json$Decode$list($author$project$Types$Override$overrideDecoder),
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'created_at',
					$elm_community$json_extra$Json$Decode$Extra$datetime,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'action_id',
						$author$project$Types$Action$actionIdDecoder,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'action',
							$author$project$Types$Action$actionTypeDecoder,
							$elm$json$Json$Decode$succeed($author$project$Types$Action$Log))))))));
var $author$project$Types$Action$logWrapperDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'logs',
	$elm$json$Json$Decode$list($author$project$Types$Action$logDecoder),
	$elm$json$Json$Decode$succeed($author$project$Types$Action$LogWrapper));
var $author$project$Page$Deployment$reqLogs = F2(
	function (deploymentName, cfg) {
		return A4(
			$author$project$Api$get,
			cfg,
			$author$project$Api$Endpoint$deploymentInfo(deploymentName),
			$elm$json$Json$Decode$list($author$project$Types$Action$logWrapperDecoder),
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployment$DeploymentInfoResponse));
	});
var $author$project$Page$Deployment$init = F3(
	function (settings, config, deploymentName) {
		return _Utils_Tuple2(
			{
				actionTable: $author$project$Page$Deployment$ActionTable$init,
				appDefaults: $krisajenkins$remotedata$RemoteData$Loading,
				appOverrides: $krisajenkins$remotedata$RemoteData$Loading,
				archivePopup: $elm$core$Maybe$Nothing,
				config: config,
				debounce: $jinjor$elm_debounce$Debounce$init,
				deployment: $krisajenkins$remotedata$RemoteData$Loading,
				deploymentDefaults: $krisajenkins$remotedata$RemoteData$Loading,
				deploymentName: deploymentName,
				deploymentOverrides: $krisajenkins$remotedata$RemoteData$Loading,
				logs: $krisajenkins$remotedata$RemoteData$Loading,
				restoreDisabled: false,
				settings: settings,
				sidebar: A3($author$project$Page$Sidebar$CreateUpdate$initUpdate, config, false, deploymentName)
			},
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($author$project$Page$Deployment$reqDeployment, deploymentName, config),
						A2($author$project$Page$Deployment$reqLogs, deploymentName, config)
					])));
	});
var $author$project$Page$Deployments$Table$ActiveTable = {$: 'ActiveTable'};
var $author$project$Page$Deployments$Table$ArchivedTable = {$: 'ArchivedTable'};
var $author$project$Page$Deployments$Table$Desc = function (a) {
	return {$: 'Desc', a: a};
};
var $author$project$Page$Deployments$Table$Updated = {$: 'Updated'};
var $author$project$Page$Deployments$Table$init = F3(
	function (config, settings, tableType) {
		return {
			archivePopup: $elm$core$Maybe$Nothing,
			config: config,
			menuButton: $elm$core$Maybe$Nothing,
			openedAppOverrides: $turboMaCk$any_set$Set$Any$empty($author$project$Types$Deployment$unDeploymentName),
			openedDeployemntOverrides: $turboMaCk$any_set$Set$Any$empty($author$project$Types$Deployment$unDeploymentName),
			settings: settings,
			sort: $author$project$Page$Deployments$Table$Desc($author$project$Page$Deployments$Table$Updated),
			tableType: tableType
		};
	});
var $author$project$Page$Sidebar$CreateUpdate$Create = {$: 'Create'};
var $author$project$Page$Sidebar$CreateUpdate$initCreate = F2(
	function (config, visibility) {
		return {
			appDefaults: $krisajenkins$remotedata$RemoteData$Loading,
			appKeys: $krisajenkins$remotedata$RemoteData$Loading,
			appOverrides: $krisajenkins$remotedata$RemoteData$Loading,
			config: config,
			deployment: $krisajenkins$remotedata$RemoteData$NotAsked,
			deploymentDefaults: $krisajenkins$remotedata$RemoteData$Loading,
			deploymentKeys: $krisajenkins$remotedata$RemoteData$Loading,
			deploymentOverrides: $krisajenkins$remotedata$RemoteData$Loading,
			mode: $author$project$Page$Sidebar$CreateUpdate$Create,
			name: $author$project$Types$Deployment$DeploymentName(''),
			nameEdited: false,
			saveResp: $krisajenkins$remotedata$RemoteData$NotAsked,
			visibility: visibility
		};
	});
var $author$project$Page$Deployments$DeploymentsResponse = function (a) {
	return {$: 'DeploymentsResponse', a: a};
};
var $author$project$Api$Endpoint$deployments = function (appUrl) {
	return A3(
		$author$project$Api$Endpoint$url,
		$elm$core$Maybe$Just(appUrl),
		_List_fromArray(
			['api', 'v1', 'deployments']),
		_List_Nil);
};
var $author$project$Page$Deployments$reqDeployments = function (cfg) {
	return A4(
		$author$project$Api$get,
		cfg,
		$author$project$Api$Endpoint$deployments,
		$elm$json$Json$Decode$list($author$project$Types$Deployment$deploymentDecoder),
		A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployments$DeploymentsResponse));
};
var $author$project$Page$Deployments$init = F2(
	function (settings, config) {
		return _Utils_Tuple2(
			{
				activeTable: A3($author$project$Page$Deployments$Table$init, config, settings, $author$project$Page$Deployments$Table$ActiveTable),
				archivedTable: A3($author$project$Page$Deployments$Table$init, config, settings, $author$project$Page$Deployments$Table$ArchivedTable),
				config: config,
				debounce: $jinjor$elm_debounce$Debounce$init,
				deployments: $krisajenkins$remotedata$RemoteData$Loading,
				search: '',
				settings: settings,
				showArchived: false,
				sidebar: A2($author$project$Page$Sidebar$CreateUpdate$initCreate, config, false),
				updated: 0
			},
			$author$project$Page$Deployments$reqDeployments(config));
	});
var $author$project$Main$changeRouteTo = F2(
	function (maybeRoute, model) {
		if (maybeRoute.$ === 'Nothing') {
			return A3(
				$author$project$Main$updateWith,
				$author$project$Main$Deployments,
				$author$project$Main$DeploymentsMsg,
				A2(
					$author$project$Page$Deployments$init,
					$author$project$Main$getSettings(model),
					$author$project$Main$getConfig(model)));
		} else {
			if (maybeRoute.a.$ === 'Deployments') {
				var _v1 = maybeRoute.a;
				return A3(
					$author$project$Main$updateWith,
					$author$project$Main$Deployments,
					$author$project$Main$DeploymentsMsg,
					A2(
						$author$project$Page$Deployments$init,
						$author$project$Main$getSettings(model),
						$author$project$Main$getConfig(model)));
			} else {
				var deploymentName = maybeRoute.a.a;
				return A3(
					$author$project$Main$updateWith,
					$author$project$Main$Deployment,
					$author$project$Main$DeploymentMsg,
					A3(
						$author$project$Page$Deployment$init,
						$author$project$Main$getSettings(model),
						$author$project$Main$getConfig(model),
						deploymentName));
			}
		}
	});
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Route$Deployment = function (a) {
	return {$: 'Deployment', a: a};
};
var $author$project$Route$Deployments = {$: 'Deployments'};
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return $elm$url$Url$Parser$Parser(
			function (_v0) {
				var visited = _v0.visited;
				var unvisited = _v0.unvisited;
				var params = _v0.params;
				var frag = _v0.frag;
				var value = _v0.value;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _v2 = stringToSomething(next);
					if (_v2.$ === 'Just') {
						var nextValue = _v2.a;
						return _List_fromArray(
							[
								A5(
								$elm$url$Url$Parser$State,
								A2($elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.visited;
		var unvisited = _v0.unvisited;
		var params = _v0.params;
		var frag = _v0.frag;
		var value = _v0.value;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var visited = _v1.visited;
				var unvisited = _v1.unvisited;
				var params = _v1.params;
				var frag = _v1.frag;
				var value = _v1.value;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
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
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$s = function (str) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var visited = _v0.visited;
			var unvisited = _v0.unvisited;
			var params = _v0.params;
			var frag = _v0.frag;
			var value = _v0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						$elm$url$Url$Parser$State,
						A2($elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $author$project$Route$parser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$Deployments,
			$elm$url$Url$Parser$s('deployments')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$Deployment,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('deployments'),
				A2(
					$elm$url$Url$Parser$custom,
					'deployment',
					function (str) {
						return $elm$core$Maybe$Just(
							$author$project$Types$Deployment$DeploymentName(str));
					})))
		]));
var $author$project$Route$fromUrl = function (url) {
	return A2(
		$elm$url$Url$Parser$parse,
		$author$project$Route$parser,
		_Utils_update(
			url,
			{
				fragment: $elm$core$Maybe$Nothing,
				path: A2($elm$core$Maybe$withDefault, '', url.fragment)
			}));
};
var $author$project$Page$Deployment$getNavKey = function (model) {
	return model.settings.navKey;
};
var $author$project$Page$Deployments$getNavKey = function (model) {
	return model.settings.navKey;
};
var $author$project$Page$Initialization$getNavKey = function (model) {
	return model.settings.navKey;
};
var $author$project$Main$getNavKey = function (model) {
	switch (model.$) {
		case 'Initialization':
			var subModel = model.a;
			return $author$project$Page$Initialization$getNavKey(subModel);
		case 'Deployments':
			var subModel = model.a;
			return $author$project$Page$Deployments$getNavKey(subModel);
		default:
			var subModel = model.a;
			return $author$project$Page$Deployment$getNavKey(subModel);
	}
};
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $author$project$Page$Deployment$ActionMsg = function (a) {
	return {$: 'ActionMsg', a: a};
};
var $author$project$Page$Deployment$AppOverridesMsg = function (a) {
	return {$: 'AppOverridesMsg', a: a};
};
var $author$project$Page$Deployment$CreateSidebarMsg = function (a) {
	return {$: 'CreateSidebarMsg', a: a};
};
var $author$project$Page$Deployment$DeploymentOverridesMsg = function (a) {
	return {$: 'DeploymentOverridesMsg', a: a};
};
var $author$project$Html$Overrides$Read = {$: 'Read'};
var $author$project$Page$Deployment$DebounceMsg = function (a) {
	return {$: 'DebounceMsg', a: a};
};
var $jinjor$elm_debounce$Debounce$Soon = F2(
	function (a, b) {
		return {$: 'Soon', a: a, b: b};
	});
var $jinjor$elm_debounce$Debounce$soon = $jinjor$elm_debounce$Debounce$Soon(0);
var $author$project$Page$Deployment$debounceConfig = {
	strategy: $jinjor$elm_debounce$Debounce$soon(2000),
	transform: $author$project$Page$Deployment$DebounceMsg
};
var $author$project$Types$Override$unOverrideName = function (_v0) {
	var name = _v0.a;
	return name;
};
var $author$project$Html$Overrides$mkDefaultNameDict = function (defaultsList) {
	return $elm$core$Dict$fromList(
		A2(
			$elm$core$List$map,
			function (d) {
				return _Utils_Tuple2(
					$author$project$Types$Override$unOverrideName(d.name),
					d);
			},
			defaultsList));
};
var $author$project$Html$Overrides$mkDefaultsDict = F2(
	function (ids, defaults) {
		return A6(
			$elm$core$Dict$merge,
			F3(
				function (_v0, _v1, res) {
					return res;
				}),
			F4(
				function (_v2, id, def, res) {
					return A3($elm$core$Dict$insert, id, def, res);
				}),
			F3(
				function (_v3, _v4, res) {
					return res;
				}),
			ids,
			defaults,
			$elm$core$Dict$empty);
	});
var $author$project$Html$Overrides$mkEditsDict = F2(
	function (ids, edits) {
		return A6(
			$elm$core$Dict$merge,
			F3(
				function (_v0, _v1, res) {
					return res;
				}),
			F4(
				function (_v2, id, def, res) {
					return A3($elm$core$Dict$insert, id, def, res);
				}),
			F3(
				function (_v3, _v4, res) {
					return res;
				}),
			ids,
			edits,
			$elm$core$Dict$empty);
	});
var $author$project$Html$Overrides$mkEditsNameDict = function (editsList) {
	return $elm$core$Dict$fromList(
		A2(
			$elm$core$List$map,
			function (d) {
				return _Utils_Tuple2(
					$author$project$Types$Override$unOverrideName(d.name),
					d);
			},
			editsList));
};
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $author$project$Html$Overrides$mkIds = F2(
	function (defaults, edits) {
		return A6(
			$elm$core$Dict$merge,
			F3(
				function (n, _v0, res) {
					return A3(
						$elm$core$Dict$insert,
						n,
						$elm$core$Dict$size(res),
						res);
				}),
			F4(
				function (n, _v1, _v2, res) {
					return A3(
						$elm$core$Dict$insert,
						n,
						$elm$core$Dict$size(res),
						res);
				}),
			F3(
				function (n, _v3, res) {
					return A3(
						$elm$core$Dict$insert,
						n,
						$elm$core$Dict$size(res),
						res);
				}),
			defaults,
			edits,
			$elm$core$Dict$empty);
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$Html$Overrides$getWrappedName = function (w) {
	if (w.$ === 'DefaultOverride') {
		var x = w.a;
		return x.name;
	} else {
		var x = w.a;
		return x.name;
	}
};
var $author$project$Html$Overrides$getWrappedValue = function (w) {
	if (w.$ === 'DefaultOverride') {
		var x = w.a;
		return x.value;
	} else {
		var x = w.a;
		return x.value;
	}
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$list_extra$List$Extra$init = function (items) {
	if (!items.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var nonEmptyList = items;
		return A2(
			$elm$core$Maybe$map,
			$elm$core$List$reverse,
			$elm$core$List$tail(
				$elm$core$List$reverse(nonEmptyList)));
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm_community$list_extra$List$Extra$tailsHelp = F2(
	function (e, list) {
		if (list.b) {
			var x = list.a;
			var xs = list.b;
			return A2(
				$elm$core$List$cons,
				A2($elm$core$List$cons, e, x),
				A2($elm$core$List$cons, x, xs));
		} else {
			return _List_Nil;
		}
	});
var $elm_community$list_extra$List$Extra$tails = A2(
	$elm$core$List$foldr,
	$elm_community$list_extra$List$Extra$tailsHelp,
	_List_fromArray(
		[_List_Nil]));
var $author$project$Html$Overrides$mkOpenedPaths = function (overridesData) {
	return $elm$core$Set$fromList(
		$elm$core$List$concat(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					$author$project$Html$Overrides$getWrappedName,
					A2(
						$elm$core$Basics$composeR,
						$author$project$Types$Override$unOverrideName,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$String$split('.'),
							A2(
								$elm$core$Basics$composeR,
								$elm_community$list_extra$List$Extra$init,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Maybe$withDefault(_List_Nil),
									A2($elm$core$Basics$composeR, $elm$core$List$reverse, $elm_community$list_extra$List$Extra$tails)))))),
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						$author$project$Html$Overrides$getWrappedValue,
						function (x) {
							return x === '';
						}),
					A2(
						$elm$core$List$map,
						$elm$core$Tuple$second,
						$elm$core$Dict$toList(overridesData))))));
};
var $author$project$Html$Overrides$Deleted = {$: 'Deleted'};
var $author$project$Html$Overrides$Edited = {$: 'Edited'};
var $author$project$Html$Overrides$New = {$: 'New'};
var $author$project$Html$Overrides$OverrideData = F3(
	function (name, value, status) {
		return {name: name, status: status, value: value};
	});
var $author$project$Html$Overrides$mkOverrideDataList = F2(
	function (defaults, edits) {
		return A6(
			$elm$core$Dict$merge,
			F3(
				function (_v0, _v1, res) {
					return res;
				}),
			F4(
				function (id, def, edit, res) {
					var _v2 = edit.value;
					if (_v2.$ === 'ValueAdded') {
						var v = _v2.a;
						return A3(
							$elm$core$Dict$insert,
							id,
							A3($author$project$Html$Overrides$OverrideData, edit.name, v, $author$project$Html$Overrides$Edited),
							res);
					} else {
						return A3(
							$elm$core$Dict$insert,
							id,
							A3($author$project$Html$Overrides$OverrideData, edit.name, def.value, $author$project$Html$Overrides$Deleted),
							res);
					}
				}),
			F3(
				function (id, edit, res) {
					var _v3 = edit.value;
					if (_v3.$ === 'ValueAdded') {
						var v = _v3.a;
						return A3(
							$elm$core$Dict$insert,
							id,
							A3($author$project$Html$Overrides$OverrideData, edit.name, v, $author$project$Html$Overrides$New),
							res);
					} else {
						return res;
					}
				}),
			defaults,
			edits,
			$elm$core$Dict$empty);
	});
var $author$project$Html$Overrides$compareOverrideData = F2(
	function (a, b) {
		var _v0 = _Utils_Tuple2(a.status, b.status);
		_v0$3:
		while (true) {
			_v0$4:
			while (true) {
				switch (_v0.a.$) {
					case 'Edited':
						switch (_v0.b.$) {
							case 'Edited':
								var _v5 = _v0.a;
								var _v6 = _v0.b;
								return A2(
									$elm$core$Basics$compare,
									$author$project$Types$Override$unOverrideName(a.name),
									$author$project$Types$Override$unOverrideName(b.name));
							case 'Deleted':
								break _v0$4;
							default:
								var _v10 = _v0.b;
								return $elm$core$Basics$GT;
						}
					case 'Deleted':
						switch (_v0.b.$) {
							case 'Deleted':
								var _v1 = _v0.a;
								var _v2 = _v0.b;
								return A2(
									$elm$core$Basics$compare,
									$author$project$Types$Override$unOverrideName(a.name),
									$author$project$Types$Override$unOverrideName(b.name));
							case 'New':
								break _v0$3;
							default:
								break _v0$3;
						}
					default:
						switch (_v0.b.$) {
							case 'Deleted':
								break _v0$4;
							case 'New':
								var _v3 = _v0.a;
								var _v4 = _v0.b;
								return A2(
									$elm$core$Basics$compare,
									$author$project$Types$Override$unOverrideName(a.name),
									$author$project$Types$Override$unOverrideName(b.name));
							default:
								var _v9 = _v0.a;
								return $elm$core$Basics$LT;
						}
				}
			}
			var _v8 = _v0.b;
			return $elm$core$Basics$GT;
		}
		var _v7 = _v0.a;
		return $elm$core$Basics$LT;
	});
var $author$project$Html$Overrides$compareWrappedOverride = F2(
	function (a, b) {
		var _v0 = _Utils_Tuple2(a, b);
		if (_v0.a.$ === 'NonOverrideWithDefault') {
			if (_v0.b.$ === 'NonOverrideWithDefault') {
				var x = _v0.a.a;
				var y = _v0.b.a;
				return A2($author$project$Html$Overrides$compareOverrideData, x, y);
			} else {
				return $elm$core$Basics$LT;
			}
		} else {
			if (_v0.b.$ === 'NonOverrideWithDefault') {
				return $elm$core$Basics$GT;
			} else {
				var x = _v0.a.a;
				var y = _v0.b.a;
				return A2(
					$elm$core$Basics$compare,
					$author$project$Types$Override$unOverrideName(x.name),
					$author$project$Types$Override$unOverrideName(y.name));
			}
		}
	});
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
var $author$project$Html$Overrides$treeHasEmptyOrEdited = F2(
	function (overrideData, t) {
		if (t.$ === 'Node') {
			var cs = t.b;
			return A2(
				$elm$core$List$any,
				$author$project$Html$Overrides$treeHasEmptyOrEdited(overrideData),
				cs);
		} else {
			var i = t.a;
			var _v1 = A2($elm$core$Dict$get, i, overrideData);
			if (_v1.$ === 'Just') {
				if (_v1.a.$ === 'NonOverrideWithDefault') {
					return true;
				} else {
					var x = _v1.a.a;
					return x.value === '';
				}
			} else {
				return false;
			}
		}
	});
var $author$project$Html$Overrides$compareTree = F3(
	function (overrideData, a, b) {
		var _v0 = _Utils_Tuple2(a, b);
		_v0$1:
		while (true) {
			_v0$2:
			while (true) {
				_v0$4:
				while (true) {
					if (_v0.a.$ === 'Node') {
						if (_v0.b.$ === 'Node') {
							if ((_v0.a.b.b && (_v0.a.b.a.$ === 'Leaf')) && (!_v0.a.b.b.b)) {
								if ((_v0.b.b.b && (_v0.b.b.a.$ === 'Leaf')) && (!_v0.b.b.b.b)) {
									var _v1 = _v0.a;
									var x = _v1.a;
									var _v2 = _v1.b;
									var i = _v2.a.a;
									var _v3 = _v0.b;
									var y = _v3.a;
									var _v4 = _v3.b;
									var j = _v4.a.a;
									var _v5 = _Utils_Tuple2(
										A2($elm$core$Dict$get, i, overrideData),
										A2($elm$core$Dict$get, j, overrideData));
									if ((_v5.a.$ === 'Just') && (_v5.b.$ === 'Just')) {
										var aa = _v5.a.a;
										var bb = _v5.b.a;
										return A2($author$project$Html$Overrides$compareWrappedOverride, aa, bb);
									} else {
										return A2($elm$core$Basics$compare, x, y);
									}
								} else {
									break _v0$1;
								}
							} else {
								if ((_v0.b.b.b && (_v0.b.b.a.$ === 'Leaf')) && (!_v0.b.b.b.b)) {
									break _v0$2;
								} else {
									var _v10 = _v0.a;
									var x = _v10.a;
									var _v11 = _v0.b;
									var y = _v11.a;
									var _v12 = _Utils_Tuple2(
										A2($author$project$Html$Overrides$treeHasEmptyOrEdited, overrideData, a),
										A2($author$project$Html$Overrides$treeHasEmptyOrEdited, overrideData, b));
									if (_v12.a) {
										if (_v12.b) {
											return A2($elm$core$Basics$compare, x, y);
										} else {
											return $elm$core$Basics$LT;
										}
									} else {
										if (!_v12.b) {
											return A2($elm$core$Basics$compare, x, y);
										} else {
											return $elm$core$Basics$GT;
										}
									}
								}
							}
						} else {
							if ((_v0.a.b.b && (_v0.a.b.a.$ === 'Leaf')) && (!_v0.a.b.b.b)) {
								break _v0$1;
							} else {
								break _v0$4;
							}
						}
					} else {
						if ((((_v0.b.$ === 'Node') && _v0.b.b.b) && (_v0.b.b.a.$ === 'Leaf')) && (!_v0.b.b.b.b)) {
							break _v0$2;
						} else {
							break _v0$4;
						}
					}
				}
				return $elm$core$Basics$EQ;
			}
			var _v8 = _v0.b;
			var _v9 = _v8.b;
			return $elm$core$Basics$GT;
		}
		var _v6 = _v0.a;
		var _v7 = _v6.b;
		return $elm$core$Basics$LT;
	});
var $author$project$Tree$getLabel = function (t) {
	if (t.$ === 'Node') {
		var l = t.a;
		return $elm$core$Maybe$Just(l);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Tree$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $author$project$Tree$Node = F2(
	function (a, b) {
		return {$: 'Node', a: a, b: b};
	});
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $elm_community$list_extra$List$Extra$gatherWith = F2(
	function (testFn, list) {
		var helper = F2(
			function (scattered, gathered) {
				helper:
				while (true) {
					if (!scattered.b) {
						return $elm$core$List$reverse(gathered);
					} else {
						var toGather = scattered.a;
						var population = scattered.b;
						var _v1 = A2(
							$elm$core$List$partition,
							testFn(toGather),
							population);
						var gathering = _v1.a;
						var remaining = _v1.b;
						var $temp$scattered = remaining,
							$temp$gathered = A2(
							$elm$core$List$cons,
							_Utils_Tuple2(toGather, gathering),
							gathered);
						scattered = $temp$scattered;
						gathered = $temp$gathered;
						continue helper;
					}
				}
			});
		return A2(helper, list, _List_Nil);
	});
var $elm_community$list_extra$List$Extra$gatherEqualsBy = F2(
	function (extract, list) {
		return A2(
			$elm_community$list_extra$List$Extra$gatherWith,
			F2(
				function (a, b) {
					return _Utils_eq(
						extract(a),
						extract(b));
				}),
			list);
	});
var $author$project$Tree$getChildren = function (t) {
	if (t.$ === 'Node') {
		var cs = t.b;
		return cs;
	} else {
		return _List_Nil;
	}
};
var $elm$core$List$sortWith = _List_sortWith;
var $author$project$Tree$mergeTreesByWithSort = F3(
	function (mrg, cmp, ts) {
		var f = function (x) {
			if (x.a.$ === 'Node') {
				var _v1 = x.a;
				var l = _v1.a;
				var cs = _v1.b;
				var ns = x.b;
				return A2(
					$author$project$Tree$Node,
					l,
					A2(
						$elm$core$List$sortWith,
						cmp,
						A3(
							$author$project$Tree$mergeTreesByWithSort,
							mrg,
							cmp,
							$elm$core$List$concat(
								A2(
									$elm$core$List$cons,
									cs,
									A2($elm$core$List$map, $author$project$Tree$getChildren, ns))))));
			} else {
				var a = x.a.a;
				return $author$project$Tree$Leaf(a);
			}
		};
		return A2(
			$elm$core$List$sortWith,
			cmp,
			A2(
				$elm$core$List$map,
				f,
				A2($elm_community$list_extra$List$Extra$gatherEqualsBy, mrg, ts)));
	});
var $author$project$Tree$mergeTreesWithSort = $author$project$Tree$mergeTreesByWithSort($author$project$Tree$getLabel);
var $author$project$Tree$pathToTree = F2(
	function (a, v) {
		if (a.b) {
			var l = a.a;
			var ls = a.b;
			return A2(
				$author$project$Tree$Node,
				l,
				_List_fromArray(
					[
						A2($author$project$Tree$pathToTree, ls, v)
					]));
		} else {
			return $author$project$Tree$Leaf(v);
		}
	});
var $author$project$Html$Overrides$mkTree = F2(
	function (ids, overridesData) {
		return A2(
			$author$project$Tree$mergeTreesWithSort,
			$author$project$Html$Overrides$compareTree(overridesData),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var n = _v0.a;
					var id = _v0.b;
					return A2(
						$author$project$Tree$pathToTree,
						A2($elm$core$String$split, '.', n),
						id);
				},
				$elm$core$Dict$toList(ids)));
	});
var $author$project$Html$Overrides$DefaultOverride = function (a) {
	return {$: 'DefaultOverride', a: a};
};
var $author$project$Html$Overrides$NonOverrideWithDefault = function (a) {
	return {$: 'NonOverrideWithDefault', a: a};
};
var $author$project$Html$Overrides$mkWrappedOverride = F2(
	function (defaults, edits) {
		return A6(
			$elm$core$Dict$merge,
			F3(
				function (id, d, res) {
					return A3(
						$elm$core$Dict$insert,
						id,
						$author$project$Html$Overrides$DefaultOverride(d),
						res);
				}),
			F4(
				function (id, _v0, e, res) {
					return A3(
						$elm$core$Dict$insert,
						id,
						$author$project$Html$Overrides$NonOverrideWithDefault(e),
						res);
				}),
			F3(
				function (id, e, res) {
					return A3(
						$elm$core$Dict$insert,
						id,
						$author$project$Html$Overrides$NonOverrideWithDefault(e),
						res);
				}),
			defaults,
			edits,
			$elm$core$Dict$empty);
	});
var $author$project$Html$Overrides$init = F5(
	function (defaults, edits, keys, mode, name) {
		var editNameDict = $author$project$Html$Overrides$mkEditsNameDict(edits);
		var defaultNameDict = $author$project$Html$Overrides$mkDefaultNameDict(defaults);
		var ids = A2($author$project$Html$Overrides$mkIds, defaultNameDict, editNameDict);
		var defaultsDict = A2($author$project$Html$Overrides$mkDefaultsDict, ids, defaultNameDict);
		var overridesDict = A2(
			$author$project$Html$Overrides$mkOverrideDataList,
			defaultsDict,
			A2($author$project$Html$Overrides$mkEditsDict, ids, editNameDict));
		var treeData = A2($author$project$Html$Overrides$mkWrappedOverride, defaultsDict, overridesDict);
		return {
			autocomplete: $elm$core$Maybe$Nothing,
			defaultOverrides: defaultsDict,
			editedOverrides: overridesDict,
			keys: keys,
			mode: mode,
			name: name,
			nextId: $elm$core$Dict$size(defaultsDict) + $elm$core$Dict$size(overridesDict),
			openedNames: $author$project$Html$Overrides$mkOpenedPaths(treeData),
			treeSchema: A2($author$project$Html$Overrides$mkTree, ids, treeData)
		};
	});
var $author$project$Page$Sidebar$CreateUpdate$DeploymentFullInfoResponse = function (a) {
	return {$: 'DeploymentFullInfoResponse', a: a};
};
var $author$project$Page$Sidebar$CreateUpdate$reqDeployment = F2(
	function (deploymentName, cfg) {
		return A4(
			$author$project$Api$get,
			cfg,
			$author$project$Api$Endpoint$deploymentFullInfo(deploymentName),
			$author$project$Types$Deployment$deploymentDecoder,
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Sidebar$CreateUpdate$DeploymentFullInfoResponse));
	});
var $author$project$Page$Sidebar$CreateUpdate$initUpdateReq = F2(
	function (config, deploymentName) {
		return A2($author$project$Page$Sidebar$CreateUpdate$reqDeployment, deploymentName, config);
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Types$Deployment$isDeploymentArchived = function (d) {
	var _v0 = d.status;
	if (_v0.$ === 'DeploymentPending') {
		return false;
	} else {
		var s = _v0.a;
		return A2(
			$elm$core$List$member,
			s,
			_List_fromArray(
				[$author$project$Types$Deployment$ArchivePending, $author$project$Types$Deployment$Archived]));
	}
};
var $krisajenkins$remotedata$RemoteData$map = F2(
	function (f, data) {
		switch (data.$) {
			case 'Success':
				var value = data.a;
				return $krisajenkins$remotedata$RemoteData$Success(
					f(value));
			case 'Loading':
				return $krisajenkins$remotedata$RemoteData$Loading;
			case 'NotAsked':
				return $krisajenkins$remotedata$RemoteData$NotAsked;
			default:
				var error = data.a;
				return $krisajenkins$remotedata$RemoteData$Failure(error);
		}
	});
var $krisajenkins$remotedata$RemoteData$andMap = F2(
	function (wrappedValue, wrappedFunction) {
		var _v0 = _Utils_Tuple2(wrappedFunction, wrappedValue);
		_v0$2:
		while (true) {
			_v0$3:
			while (true) {
				_v0$4:
				while (true) {
					_v0$5:
					while (true) {
						switch (_v0.a.$) {
							case 'Success':
								switch (_v0.b.$) {
									case 'Success':
										var f = _v0.a.a;
										var value = _v0.b.a;
										return $krisajenkins$remotedata$RemoteData$Success(
											f(value));
									case 'Failure':
										break _v0$2;
									case 'Loading':
										break _v0$4;
									default:
										var _v4 = _v0.b;
										return $krisajenkins$remotedata$RemoteData$NotAsked;
								}
							case 'Failure':
								var error = _v0.a.a;
								return $krisajenkins$remotedata$RemoteData$Failure(error);
							case 'Loading':
								switch (_v0.b.$) {
									case 'Failure':
										break _v0$2;
									case 'Loading':
										break _v0$3;
									case 'NotAsked':
										break _v0$3;
									default:
										break _v0$3;
								}
							default:
								switch (_v0.b.$) {
									case 'Failure':
										break _v0$2;
									case 'Loading':
										break _v0$4;
									case 'NotAsked':
										break _v0$5;
									default:
										break _v0$5;
								}
						}
					}
					var _v3 = _v0.a;
					return $krisajenkins$remotedata$RemoteData$NotAsked;
				}
				var _v2 = _v0.b;
				return $krisajenkins$remotedata$RemoteData$Loading;
			}
			var _v1 = _v0.a;
			return $krisajenkins$remotedata$RemoteData$Loading;
		}
		var error = _v0.b.a;
		return $krisajenkins$remotedata$RemoteData$Failure(error);
	});
var $krisajenkins$remotedata$RemoteData$map2 = F3(
	function (f, a, b) {
		return A2(
			$krisajenkins$remotedata$RemoteData$andMap,
			b,
			A2($krisajenkins$remotedata$RemoteData$map, f, a));
	});
var $jinjor$elm_debounce$Debounce$Flush = function (a) {
	return {$: 'Flush', a: a};
};
var $jinjor$elm_debounce$Debounce$SendIfLengthNotChangedFrom = function (a) {
	return {$: 'SendIfLengthNotChangedFrom', a: a};
};
var $elm$core$Process$sleep = _Process_sleep;
var $jinjor$elm_debounce$Debounce$delayCmd = F2(
	function (delay, msg) {
		return A2(
			$elm$core$Task$perform,
			function (_v0) {
				return msg;
			},
			$elm$core$Process$sleep(delay));
	});
var $jinjor$elm_debounce$Debounce$length = function (_v0) {
	var input = _v0.a.input;
	return $elm$core$List$length(input);
};
var $jinjor$elm_debounce$Debounce$push = F3(
	function (config, a, _v0) {
		var d = _v0.a;
		var newDebounce = $jinjor$elm_debounce$Debounce$Debounce(
			_Utils_update(
				d,
				{
					input: A2($elm$core$List$cons, a, d.input)
				}));
		var selfCmd = function () {
			var _v1 = config.strategy;
			switch (_v1.$) {
				case 'Manual':
					var offset = _v1.a;
					return d.locked ? $elm$core$Platform$Cmd$none : A2(
						$jinjor$elm_debounce$Debounce$delayCmd,
						offset,
						$jinjor$elm_debounce$Debounce$Flush($elm$core$Maybe$Nothing));
				case 'Soon':
					var offset = _v1.a;
					var delay = _v1.b;
					return d.locked ? $elm$core$Platform$Cmd$none : A2(
						$jinjor$elm_debounce$Debounce$delayCmd,
						offset,
						$jinjor$elm_debounce$Debounce$Flush(
							$elm$core$Maybe$Just(delay)));
				default:
					var delay = _v1.a;
					return A2(
						$jinjor$elm_debounce$Debounce$delayCmd,
						delay,
						$jinjor$elm_debounce$Debounce$SendIfLengthNotChangedFrom(
							$jinjor$elm_debounce$Debounce$length(newDebounce)));
			}
		}();
		return _Utils_Tuple2(
			newDebounce,
			A2($elm$core$Platform$Cmd$map, config.transform, selfCmd));
	});
var $author$project$Page$Deployment$AppOverridesResponse = function (a) {
	return {$: 'AppOverridesResponse', a: a};
};
var $author$project$Api$Endpoint$appOverrides = function (appUrl) {
	return A3(
		$author$project$Api$Endpoint$url,
		$elm$core$Maybe$Just(appUrl),
		_List_fromArray(
			['api', 'v1', 'application_overrides']),
		_List_Nil);
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
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Types$OverrideWithDefault$defaultOverrideEncoder = function (defaultOverride) {
	return A2(
		$elm$json$Json$Encode$list,
		$elm$json$Json$Encode$string,
		_List_fromArray(
			[
				$author$project$Types$Override$unOverrideName(defaultOverride.name),
				defaultOverride.value
			]));
};
var $author$project$Types$OverrideWithDefault$OverrideWithDefault = F2(
	function (name, value) {
		return {name: name, value: value};
	});
var $author$project$Types$OverrideWithDefault$defaultOverridesDecode = function () {
	var f = function (x) {
		if ((x.b && x.b.b) && (!x.b.b.b)) {
			var p = x.a;
			var _v1 = x.b;
			var v = _v1.a;
			return $elm$json$Json$Decode$succeed(
				A2(
					$author$project$Types$OverrideWithDefault$OverrideWithDefault,
					$author$project$Types$Override$OverrideName(p),
					v));
		} else {
			return $elm$json$Json$Decode$fail('not an override');
		}
	};
	return $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$resolve(
		A2(
			$elm$json$Json$Decode$map,
			f,
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
}();
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $author$project$Api$post = F5(
	function (config, url, body, decoder, msg) {
		return $author$project$Api$Endpoint$request(
			{
				body: body,
				expect: A2($author$project$Api$expectJson, msg, decoder),
				headers: _List_fromArray(
					[
						$author$project$Api$authHeader(config.appAuth)
					]),
				method: 'POST',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: url(config.appUrl)
			});
	});
var $author$project$Page$Deployment$reqAppOverrides = F2(
	function (config, body) {
		return A5(
			$author$project$Api$post,
			config,
			$author$project$Api$Endpoint$appOverrides,
			$elm$http$Http$jsonBody(
				A2($elm$json$Json$Encode$list, $author$project$Types$OverrideWithDefault$defaultOverrideEncoder, body)),
			$elm$json$Json$Decode$list($author$project$Types$OverrideWithDefault$defaultOverridesDecode),
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployment$AppOverridesResponse));
	});
var $author$project$Page$Deployment$DeleteDeploymentResp = function (a) {
	return {$: 'DeleteDeploymentResp', a: a};
};
var $author$project$Api$delete = F5(
	function (config, url, body, decoder, msg) {
		return $author$project$Api$Endpoint$request(
			{
				body: body,
				expect: A2($author$project$Api$expectJson, msg, decoder),
				headers: _List_fromArray(
					[
						$author$project$Api$authHeader(config.appAuth)
					]),
				method: 'DELETE',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: url(config.appUrl)
			});
	});
var $author$project$Api$Endpoint$deleteDeployment = F2(
	function (deploymentName, appUrl) {
		return A3(
			$author$project$Api$Endpoint$url,
			$elm$core$Maybe$Just(appUrl),
			_List_fromArray(
				[
					'api',
					'v1',
					'deployments',
					$author$project$Types$Deployment$unDeploymentName(deploymentName)
				]),
			_List_Nil);
	});
var $author$project$Page$Deployment$reqDeleteDeployment = F2(
	function (config, deploymentName) {
		return A5(
			$author$project$Api$delete,
			config,
			$author$project$Api$Endpoint$deleteDeployment(deploymentName),
			$elm$http$Http$emptyBody,
			$elm$json$Json$Decode$string,
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployment$DeleteDeploymentResp));
	});
var $author$project$Page$Deployment$DeploymentOverridesResponse = function (a) {
	return {$: 'DeploymentOverridesResponse', a: a};
};
var $author$project$Api$Endpoint$deploymentOverrides = function (appUrl) {
	return A3(
		$author$project$Api$Endpoint$url,
		$elm$core$Maybe$Just(appUrl),
		_List_fromArray(
			['api', 'v1', 'deployment_overrides']),
		_List_Nil);
};
var $author$project$Page$Deployment$reqDeploymentOverrides = function (config) {
	return A4(
		$author$project$Api$get,
		config,
		$author$project$Api$Endpoint$deploymentOverrides,
		$elm$json$Json$Decode$list($author$project$Types$OverrideWithDefault$defaultOverridesDecode),
		A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployment$DeploymentOverridesResponse));
};
var $author$project$Page$Deployment$RestoreDeploymentResp = function (a) {
	return {$: 'RestoreDeploymentResp', a: a};
};
var $author$project$Api$patch = F4(
	function (config, url, decoder, msg) {
		return $author$project$Api$Endpoint$request(
			{
				body: $elm$http$Http$emptyBody,
				expect: A2($author$project$Api$expectJson, msg, decoder),
				headers: _List_fromArray(
					[
						$author$project$Api$authHeader(config.appAuth)
					]),
				method: 'PATCH',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: url(config.appUrl)
			});
	});
var $author$project$Api$Endpoint$restoreDeployment = F2(
	function (deploymentName, appUrl) {
		return A3(
			$author$project$Api$Endpoint$url,
			$elm$core$Maybe$Just(appUrl),
			_List_fromArray(
				[
					'api',
					'v1',
					'deployments',
					$author$project$Types$Deployment$unDeploymentName(deploymentName),
					'restore'
				]),
			_List_Nil);
	});
var $author$project$Page$Deployment$reqRestoreDeployment = F2(
	function (config, deploymentName) {
		return A4(
			$author$project$Api$patch,
			config,
			$author$project$Api$Endpoint$restoreDeployment(deploymentName),
			$elm$json$Json$Decode$string,
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployment$RestoreDeploymentResp));
	});
var $jinjor$elm_debounce$Debounce$takeAll = F3(
	function (send, head, tail) {
		return _Utils_Tuple2(
			_List_Nil,
			A2(send, head, tail));
	});
var $krisajenkins$remotedata$RemoteData$unwrap = F3(
	function (_default, _function, remoteData) {
		if (remoteData.$ === 'Success') {
			var data = remoteData.a;
			return _function(data);
		} else {
			return _default;
		}
	});
var $jinjor$elm_debounce$Debounce$update = F4(
	function (config, send, msg, _v0) {
		var d = _v0.a;
		switch (msg.$) {
			case 'NoOp':
				return _Utils_Tuple2(
					$jinjor$elm_debounce$Debounce$Debounce(d),
					$elm$core$Platform$Cmd$none);
			case 'Flush':
				var tryAgainAfter = msg.a;
				var _v2 = d.input;
				if (_v2.b) {
					var head = _v2.a;
					var tail = _v2.b;
					var selfCmd = function () {
						if (tryAgainAfter.$ === 'Just') {
							var delay = tryAgainAfter.a;
							return A2(
								$jinjor$elm_debounce$Debounce$delayCmd,
								delay,
								$jinjor$elm_debounce$Debounce$Flush(
									$elm$core$Maybe$Just(delay)));
						} else {
							return $elm$core$Platform$Cmd$none;
						}
					}();
					var _v3 = A2(send, head, tail);
					var input = _v3.a;
					var sendCmd = _v3.b;
					return _Utils_Tuple2(
						$jinjor$elm_debounce$Debounce$Debounce(
							_Utils_update(
								d,
								{input: input, locked: true})),
						$elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									sendCmd,
									A2($elm$core$Platform$Cmd$map, config.transform, selfCmd)
								])));
				} else {
					return _Utils_Tuple2(
						$jinjor$elm_debounce$Debounce$Debounce(
							_Utils_update(
								d,
								{locked: false})),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var lastInputLength = msg.a;
				var _v5 = _Utils_Tuple2(
					_Utils_cmp(
						$elm$core$List$length(d.input),
						lastInputLength) < 1,
					d.input);
				if (_v5.a && _v5.b.b) {
					var _v6 = _v5.b;
					var head = _v6.a;
					var tail = _v6.b;
					var _v7 = A2(send, head, tail);
					var input = _v7.a;
					var cmd = _v7.b;
					return _Utils_Tuple2(
						$jinjor$elm_debounce$Debounce$Debounce(
							_Utils_update(
								d,
								{input: input})),
						cmd);
				} else {
					return _Utils_Tuple2(
						$jinjor$elm_debounce$Debounce$Debounce(d),
						$elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Html$Overrides$deleteOverride = F2(
	function (ix, model) {
		var _delete = function (mOverride) {
			var _v0 = _Utils_Tuple2(
				mOverride,
				A2($elm$core$Dict$get, ix, model.defaultOverrides));
			_v0$2:
			while (true) {
				if (_v0.a.$ === 'Nothing') {
					if (_v0.b.$ === 'Just') {
						var _v1 = _v0.a;
						var _default = _v0.b.a;
						return $elm$core$Maybe$Just(
							{name: _default.name, status: $author$project$Html$Overrides$Deleted, value: _default.value});
					} else {
						break _v0$2;
					}
				} else {
					if (_v0.b.$ === 'Nothing') {
						var override = _v0.a.a;
						var _v2 = _v0.b;
						return $elm$core$Maybe$Just(
							_Utils_update(
								override,
								{status: $author$project$Html$Overrides$Deleted}));
					} else {
						break _v0$2;
					}
				}
			}
			return $elm$core$Maybe$Nothing;
		};
		return _Utils_update(
			model,
			{
				editedOverrides: A3($elm$core$Dict$update, ix, _delete, model.editedOverrides)
			});
	});
var $author$project$Html$Overrides$editOverrideName = F3(
	function (ix, name, model) {
		var editName = function (mOverride) {
			var _v0 = _Utils_Tuple2(
				mOverride,
				A2($elm$core$Dict$get, ix, model.defaultOverrides));
			if (_v0.a.$ === 'Just') {
				if (_v0.b.$ === 'Just') {
					var override = _v0.a.a;
					var _default = _v0.b.a;
					return (_Utils_eq(
						$author$project$Types$Override$OverrideName(name),
						_default.name) && _Utils_eq(override.value, _default.value)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						_Utils_update(
							override,
							{
								name: $author$project$Types$Override$OverrideName(name),
								status: $author$project$Html$Overrides$Edited
							}));
				} else {
					var override = _v0.a.a;
					var _v1 = _v0.b;
					return $elm$core$Maybe$Just(
						_Utils_update(
							override,
							{
								name: $author$project$Types$Override$OverrideName(name),
								status: $author$project$Html$Overrides$New
							}));
				}
			} else {
				if (_v0.b.$ === 'Just') {
					var _v2 = _v0.a;
					var _default = _v0.b.a;
					return _Utils_eq(
						$author$project$Types$Override$OverrideName(name),
						_default.name) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						{
							name: $author$project$Types$Override$OverrideName(name),
							status: $author$project$Html$Overrides$Edited,
							value: _default.value
						});
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		};
		return _Utils_update(
			model,
			{
				editedOverrides: A3($elm$core$Dict$update, ix, editName, model.editedOverrides)
			});
	});
var $author$project$Html$Overrides$editOverrideValue = F3(
	function (ix, value, model) {
		var editName = function (mOverride) {
			var _v0 = _Utils_Tuple2(
				mOverride,
				A2($elm$core$Dict$get, ix, model.defaultOverrides));
			if (_v0.a.$ === 'Just') {
				if (_v0.b.$ === 'Just') {
					var override = _v0.a.a;
					var _default = _v0.b.a;
					return (_Utils_eq(value, _default.value) && _Utils_eq(override.name, _default.name)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						_Utils_update(
							override,
							{status: $author$project$Html$Overrides$Edited, value: value}));
				} else {
					var override = _v0.a.a;
					var _v1 = _v0.b;
					return $elm$core$Maybe$Just(
						_Utils_update(
							override,
							{status: $author$project$Html$Overrides$New, value: value}));
				}
			} else {
				if (_v0.b.$ === 'Just') {
					var _v2 = _v0.a;
					var _default = _v0.b.a;
					return _Utils_eq(value, _default.value) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						{name: _default.name, status: $author$project$Html$Overrides$Edited, value: value});
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		};
		return _Utils_update(
			model,
			{
				editedOverrides: A3($elm$core$Dict$update, ix, editName, model.editedOverrides)
			});
	});
var $author$project$Html$Overrides$newOverride = function (model) {
	return _Utils_update(
		model,
		{
			editedOverrides: A3(
				$elm$core$Dict$insert,
				model.nextId,
				A3(
					$author$project$Html$Overrides$OverrideData,
					$author$project$Types$Override$OverrideName(''),
					'',
					$author$project$Html$Overrides$New),
				model.editedOverrides),
			nextId: model.nextId + 1
		});
};
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$remove, key, dict));
	});
var $author$project$Html$Overrides$restoreOverride = F2(
	function (ix, model) {
		var restore = function (mOverride) {
			var _v0 = _Utils_Tuple2(
				mOverride,
				A2($elm$core$Dict$get, ix, model.defaultOverrides));
			if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Nothing')) {
				var override = _v0.a.a;
				var _v1 = _v0.b;
				return $elm$core$Maybe$Just(
					_Utils_update(
						override,
						{status: $author$project$Html$Overrides$New}));
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		return _Utils_update(
			model,
			{
				editedOverrides: A3($elm$core$Dict$update, ix, restore, model.editedOverrides)
			});
	});
var $author$project$Html$Overrides$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'AddOverride':
				return _Utils_Tuple2(
					$author$project$Html$Overrides$newOverride(model),
					$elm$core$Platform$Cmd$none);
			case 'DeleteOverride':
				var ix = msg.a;
				return _Utils_Tuple2(
					A2($author$project$Html$Overrides$deleteOverride, ix, model),
					$elm$core$Platform$Cmd$none);
			case 'RestoreOverride':
				var ix = msg.a;
				return _Utils_Tuple2(
					A2($author$project$Html$Overrides$restoreOverride, ix, model),
					$elm$core$Platform$Cmd$none);
			case 'EditOverrideName':
				var ix = msg.a;
				var name = msg.b;
				return _Utils_Tuple2(
					A3($author$project$Html$Overrides$editOverrideName, ix, name, model),
					$elm$core$Platform$Cmd$none);
			case 'EditOverrideValue':
				var ix = msg.a;
				var value = msg.b;
				return _Utils_Tuple2(
					A3($author$project$Html$Overrides$editOverrideValue, ix, value, model),
					$elm$core$Platform$Cmd$none);
			case 'OpenName':
				var name = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedNames: A2($elm$core$Set$insert, name, model.openedNames)
						}),
					$elm$core$Platform$Cmd$none);
			case 'CloseName':
				var name = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedNames: A2($elm$core$Set$remove, name, model.openedNames)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ShowAutocomplete':
				var ix = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							autocomplete: $elm$core$Maybe$Just(ix)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{autocomplete: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $turboMaCk$any_dict$Dict$Any$insert = F3(
	function (k, v, _v0) {
		var inner = _v0.a;
		return $turboMaCk$any_dict$Dict$Any$AnyDict(
			_Utils_update(
				inner,
				{
					dict: A3(
						$elm$core$Dict$insert,
						inner.toKey(k),
						_Utils_Tuple2(k, v),
						inner.dict)
				}));
	});
var $turboMaCk$any_set$Set$Any$insert = F2(
	function (a, _v0) {
		var dict = _v0.a;
		return $turboMaCk$any_set$Set$Any$AnySet(
			A3($turboMaCk$any_dict$Dict$Any$insert, a, _Utils_Tuple0, dict));
	});
var $turboMaCk$any_dict$Dict$Any$remove = F2(
	function (k, _v0) {
		var inner = _v0.a;
		return $turboMaCk$any_dict$Dict$Any$AnyDict(
			_Utils_update(
				inner,
				{
					dict: A2(
						$elm$core$Dict$remove,
						inner.toKey(k),
						inner.dict)
				}));
	});
var $turboMaCk$any_set$Set$Any$remove = F2(
	function (a, _v0) {
		var dict = _v0.a;
		return $turboMaCk$any_set$Set$Any$AnySet(
			A2($turboMaCk$any_dict$Dict$Any$remove, a, dict));
	});
var $author$project$Page$Deployment$ActionTable$update = F2(
	function (cmd, model) {
		switch (cmd.$) {
			case 'OpenDeploymentOverrides':
				var id = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedDeployemntOverrides: A2($turboMaCk$any_set$Set$Any$insert, id, model.openedDeployemntOverrides)
						}),
					$elm$core$Platform$Cmd$none);
			case 'CloseDeploymentOverrides':
				var id = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedDeployemntOverrides: A2($turboMaCk$any_set$Set$Any$remove, id, model.openedDeployemntOverrides)
						}),
					$elm$core$Platform$Cmd$none);
			case 'OpenAppOverrides':
				var id = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedAppOverrides: A2($turboMaCk$any_set$Set$Any$insert, id, model.openedAppOverrides)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var id = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedAppOverrides: A2($turboMaCk$any_set$Set$Any$remove, id, model.openedAppOverrides)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Page$Sidebar$CreateUpdate$AppOverridesMsg = function (a) {
	return {$: 'AppOverridesMsg', a: a};
};
var $author$project$Page$Sidebar$CreateUpdate$DeploymentOverridesMsg = function (a) {
	return {$: 'DeploymentOverridesMsg', a: a};
};
var $author$project$Html$Overrides$Write = {$: 'Write'};
var $author$project$Html$Overrides$dataChanged = function (msg) {
	switch (msg.$) {
		case 'DeleteOverride':
			return true;
		case 'RestoreOverride':
			return true;
		case 'EditOverrideName':
			return true;
		case 'EditOverrideValue':
			return true;
		default:
			return false;
	}
};
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Html$Overrides$getEditedOverrides = function (model) {
	return A2(
		$elm$core$List$map,
		function (o) {
			var _v0 = o.status;
			switch (_v0.$) {
				case 'Deleted':
					return A2($author$project$Types$Override$Override, o.name, $author$project$Types$Override$ValueDeleted);
				case 'New':
					return A2(
						$author$project$Types$Override$Override,
						o.name,
						$author$project$Types$Override$ValueAdded(o.value));
				default:
					return A2(
						$author$project$Types$Override$Override,
						o.name,
						$author$project$Types$Override$ValueAdded(o.value));
			}
		},
		$elm$core$Dict$values(model.editedOverrides));
};
var $author$project$Html$Overrides$getFullOverrides = function (model) {
	return A6(
		$elm$core$Dict$merge,
		F3(
			function (_v0, e, res) {
				var _v1 = e.status;
				if (_v1.$ === 'New') {
					return A2(
						$elm$core$List$cons,
						{name: e.name, value: e.value},
						res);
				} else {
					return res;
				}
			}),
		F4(
			function (_v2, e, _v3, res) {
				var _v4 = e.status;
				if (_v4.$ === 'Edited') {
					return A2(
						$elm$core$List$cons,
						{name: e.name, value: e.value},
						res);
				} else {
					return res;
				}
			}),
		F3(
			function (_v5, d, res) {
				return A2($elm$core$List$cons, d, res);
			}),
		model.editedOverrides,
		model.defaultOverrides,
		_List_Nil);
};
var $author$project$Page$Sidebar$CreateUpdate$DeploymentOverrideKeysResponse = function (a) {
	return {$: 'DeploymentOverrideKeysResponse', a: a};
};
var $author$project$Api$Endpoint$deploymentOverrideKeys = function (appUrl) {
	return A3(
		$author$project$Api$Endpoint$url,
		$elm$core$Maybe$Just(appUrl),
		_List_fromArray(
			['api', 'v1', 'deployment_override_keys']),
		_List_Nil);
};
var $author$project$Types$Override$overrideNameDecoder = A2($elm$json$Json$Decode$map, $author$project$Types$Override$OverrideName, $elm$json$Json$Decode$string);
var $author$project$Page$Sidebar$CreateUpdate$reqDeploymentOverrideKeys = function (config) {
	return A4(
		$author$project$Api$get,
		config,
		$author$project$Api$Endpoint$deploymentOverrideKeys,
		$elm$json$Json$Decode$list($author$project$Types$Override$overrideNameDecoder),
		A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Sidebar$CreateUpdate$DeploymentOverrideKeysResponse));
};
var $author$project$Page$Sidebar$CreateUpdate$DeploymentOverridesResponse = function (a) {
	return {$: 'DeploymentOverridesResponse', a: a};
};
var $author$project$Page$Sidebar$CreateUpdate$reqDeploymentOverrides = function (config) {
	return A4(
		$author$project$Api$get,
		config,
		$author$project$Api$Endpoint$deploymentOverrides,
		$elm$json$Json$Decode$list($author$project$Types$OverrideWithDefault$defaultOverridesDecode),
		A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Sidebar$CreateUpdate$DeploymentOverridesResponse));
};
var $author$project$Page$Sidebar$CreateUpdate$initCreateReq = function (config) {
	return $elm$core$Platform$Cmd$batch(
		_List_fromArray(
			[
				$author$project$Page$Sidebar$CreateUpdate$reqDeploymentOverrideKeys(config),
				$author$project$Page$Sidebar$CreateUpdate$reqDeploymentOverrides(config)
			]));
};
var $author$project$Page$Sidebar$CreateUpdate$AppOverrideKeysResponse = function (a) {
	return {$: 'AppOverrideKeysResponse', a: a};
};
var $author$project$Api$Endpoint$appOverrideKeys = function (appUrl) {
	return A3(
		$author$project$Api$Endpoint$url,
		$elm$core$Maybe$Just(appUrl),
		_List_fromArray(
			['api', 'v1', 'application_override_keys']),
		_List_Nil);
};
var $author$project$Page$Sidebar$CreateUpdate$reqAppOverrideKeys = F2(
	function (config, body) {
		return A5(
			$author$project$Api$post,
			config,
			$author$project$Api$Endpoint$appOverrideKeys,
			$elm$http$Http$jsonBody(
				A2($elm$json$Json$Encode$list, $author$project$Types$OverrideWithDefault$defaultOverrideEncoder, body)),
			$elm$json$Json$Decode$list($author$project$Types$Override$overrideNameDecoder),
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Sidebar$CreateUpdate$AppOverrideKeysResponse));
	});
var $author$project$Page$Sidebar$CreateUpdate$AppOverridesResponse = function (a) {
	return {$: 'AppOverridesResponse', a: a};
};
var $author$project$Page$Sidebar$CreateUpdate$reqAppOverrides = F2(
	function (config, body) {
		return A5(
			$author$project$Api$post,
			config,
			$author$project$Api$Endpoint$appOverrides,
			$elm$http$Http$jsonBody(
				A2($elm$json$Json$Encode$list, $author$project$Types$OverrideWithDefault$defaultOverrideEncoder, body)),
			$elm$json$Json$Decode$list($author$project$Types$OverrideWithDefault$defaultOverridesDecode),
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Sidebar$CreateUpdate$AppOverridesResponse));
	});
var $author$project$Page$Sidebar$CreateUpdate$SaveDeploymentResponse = function (a) {
	return {$: 'SaveDeploymentResponse', a: a};
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
var $author$project$Types$Override$overrideValueEncode = function (value) {
	if (value.$ === 'ValueAdded') {
		var v = value.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'tag',
					$elm$json$Json$Encode$string('ValueAdded')),
					_Utils_Tuple2(
					'contents',
					$elm$json$Json$Encode$string(v))
				]));
	} else {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'tag',
					$elm$json$Json$Encode$string('ValueDeleted'))
				]));
	}
};
var $author$project$Types$Override$overrideHelperEncode = function (helper) {
	if (helper.$ === 'OverrideName_') {
		var name = helper.a;
		return $elm$json$Json$Encode$string(name);
	} else {
		var val = helper.a;
		return $author$project$Types$Override$overrideValueEncode(val);
	}
};
var $author$project$Types$Override$overrideToHelper = function (override) {
	return _List_fromArray(
		[
			$author$project$Types$Override$OverrideName_(
			$author$project$Types$Override$unOverrideName(override.name)),
			$author$project$Types$Override$OverrideValue_(override.value)
		]);
};
var $author$project$Types$Override$overrideEncode = function (override) {
	var helper = $author$project$Types$Override$overrideToHelper(override);
	return A2($elm$json$Json$Encode$list, $author$project$Types$Override$overrideHelperEncode, helper);
};
var $author$project$Types$Deployment$infoEncode = function (info) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(
					$author$project$Types$Deployment$unDeploymentName(info.name))),
				_Utils_Tuple2(
				'app_overrides',
				A2($elm$json$Json$Encode$list, $author$project$Types$Override$overrideEncode, info.appOverrides)),
				_Utils_Tuple2(
				'deployment_overrides',
				A2($elm$json$Json$Encode$list, $author$project$Types$Override$overrideEncode, info.deploymentOverrides))
			]));
};
var $author$project$Api$Endpoint$saveDeployment = function (appUrl) {
	return A3(
		$author$project$Api$Endpoint$url,
		$elm$core$Maybe$Just(appUrl),
		_List_fromArray(
			['api', 'v1', 'deployments']),
		_List_Nil);
};
var $author$project$Page$Sidebar$CreateUpdate$reqCreateDeployment = F2(
	function (config, body) {
		return A5(
			$author$project$Api$post,
			config,
			$author$project$Api$Endpoint$saveDeployment,
			$elm$http$Http$jsonBody(
				$author$project$Types$Deployment$infoEncode(body)),
			$elm$json$Json$Decode$succeed(_Utils_Tuple0),
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Sidebar$CreateUpdate$SaveDeploymentResponse));
	});
var $author$project$Api$put = F5(
	function (config, url, body, decoder, msg) {
		return $author$project$Api$Endpoint$request(
			{
				body: body,
				expect: A2($author$project$Api$expectJson, msg, decoder),
				headers: _List_fromArray(
					[
						$author$project$Api$authHeader(config.appAuth)
					]),
				method: 'PUT',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: url(config.appUrl)
			});
	});
var $author$project$Api$Endpoint$updateDeployment = F2(
	function (deploymentName, appUrl) {
		return A3(
			$author$project$Api$Endpoint$url,
			$elm$core$Maybe$Just(appUrl),
			_List_fromArray(
				[
					'api',
					'v1',
					'deployments',
					$author$project$Types$Deployment$unDeploymentName(deploymentName)
				]),
			_List_Nil);
	});
var $author$project$Types$Deployment$updateInfoEncode = function (info) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'app_overrides',
				A2($elm$json$Json$Encode$list, $author$project$Types$Override$overrideEncode, info.appOverrides)),
				_Utils_Tuple2(
				'deployment_overrides',
				A2($elm$json$Json$Encode$list, $author$project$Types$Override$overrideEncode, info.deploymentOverrides))
			]));
};
var $author$project$Page$Sidebar$CreateUpdate$reqUpdateDeployment = F3(
	function (deploymentName, config, body) {
		return A5(
			$author$project$Api$put,
			config,
			$author$project$Api$Endpoint$updateDeployment(deploymentName),
			$elm$http$Http$jsonBody(
				$author$project$Types$Deployment$updateInfoEncode(body)),
			$elm$json$Json$Decode$succeed(_Utils_Tuple0),
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Sidebar$CreateUpdate$SaveDeploymentResponse));
	});
var $author$project$Page$Sidebar$CreateUpdate$update = F2(
	function (cmd, model) {
		var updateWith = F3(
			function (toModel, toMsg, _v10) {
				var subModel = _v10.a;
				var subCmd = _v10.b;
				return _Utils_Tuple2(
					toModel(subModel),
					A2($elm$core$Platform$Cmd$map, toMsg, subCmd));
			});
		var initOverrides = F4(
			function (name, edits, defaults, keys) {
				return A5($author$project$Html$Overrides$init, defaults, edits, keys, $author$project$Html$Overrides$Write, name);
			});
		var _v0 = function () {
			var _v1 = model.deployment;
			if (_v1.$ === 'Success') {
				var deployment = _v1.a;
				return _Utils_Tuple2(deployment.deployment.appOverrides, deployment.deployment.deploymentOverrides);
			} else {
				return _Utils_Tuple2(_List_Nil, _List_Nil);
			}
		}();
		var appEdits = _v0.a;
		var deployEdits = _v0.b;
		switch (cmd.$) {
			case 'DeploymentOverrideKeysResponse':
				var keys = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							deploymentKeys: keys,
							deploymentOverrides: A3(
								$krisajenkins$remotedata$RemoteData$map2,
								A2(initOverrides, 'Deployment configuration', deployEdits),
								model.deploymentDefaults,
								keys)
						}),
					$elm$core$Platform$Cmd$none);
			case 'DeploymentOverridesResponse':
				var overrides = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							deploymentDefaults: overrides,
							deploymentOverrides: A3(
								$krisajenkins$remotedata$RemoteData$map2,
								A2(initOverrides, 'Deployment configuration', deployEdits),
								overrides,
								model.deploymentKeys)
						}),
					function () {
						if (overrides.$ === 'Success') {
							var defaults = overrides.a;
							return $elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										A2($author$project$Page$Sidebar$CreateUpdate$reqAppOverrideKeys, model.config, defaults),
										A2($author$project$Page$Sidebar$CreateUpdate$reqAppOverrides, model.config, defaults)
									]));
						} else {
							return $elm$core$Platform$Cmd$none;
						}
					}());
			case 'AppOverrideKeysResponse':
				var keys = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							appKeys: keys,
							appOverrides: A3(
								$krisajenkins$remotedata$RemoteData$map2,
								A2(initOverrides, 'App configuration', appEdits),
								model.appDefaults,
								keys)
						}),
					$elm$core$Platform$Cmd$none);
			case 'AppOverridesResponse':
				var overrides = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							appDefaults: overrides,
							appOverrides: A3(
								$krisajenkins$remotedata$RemoteData$map2,
								A2(initOverrides, 'App configuration', appEdits),
								overrides,
								model.appKeys)
						}),
					$elm$core$Platform$Cmd$none);
			case 'Close':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{visibility: false}),
					$elm$core$Platform$Cmd$none);
			case 'NameInput':
				var name = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							name: $author$project$Types$Deployment$DeploymentName(name),
							nameEdited: true
						}),
					$elm$core$Platform$Cmd$none);
			case 'AppOverridesMsg':
				var subMsg = cmd.a;
				var _v4 = model.appOverrides;
				if (_v4.$ === 'Success') {
					var appOverrides = _v4.a;
					return A3(
						updateWith,
						function (updated) {
							return _Utils_update(
								model,
								{
									appOverrides: $krisajenkins$remotedata$RemoteData$Success(updated)
								});
						},
						$author$project$Page$Sidebar$CreateUpdate$AppOverridesMsg,
						A2($author$project$Html$Overrides$update, subMsg, appOverrides));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'DeploymentOverridesMsg':
				var subMsg = cmd.a;
				var _v5 = model.deploymentOverrides;
				if (_v5.$ === 'Success') {
					var deploymentOverrides = _v5.a;
					var _v6 = A3(
						updateWith,
						function (deploymentOverrides_) {
							return _Utils_update(
								model,
								{
									deploymentOverrides: $krisajenkins$remotedata$RemoteData$Success(deploymentOverrides_)
								});
						},
						$author$project$Page$Sidebar$CreateUpdate$DeploymentOverridesMsg,
						A2($author$project$Html$Overrides$update, subMsg, deploymentOverrides));
					var model_ = _v6.a;
					var cmd_ = _v6.b;
					var _v7 = _Utils_Tuple2(
						$author$project$Html$Overrides$dataChanged(subMsg),
						model_.deploymentOverrides);
					if (_v7.a && (_v7.b.$ === 'Success')) {
						var deploymentOverrides_ = _v7.b.a;
						return _Utils_Tuple2(
							_Utils_update(
								model_,
								{appOverrides: $krisajenkins$remotedata$RemoteData$Loading}),
							$elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										A2(
										$author$project$Page$Sidebar$CreateUpdate$reqAppOverrideKeys,
										model_.config,
										$author$project$Html$Overrides$getFullOverrides(deploymentOverrides_)),
										A2(
										$author$project$Page$Sidebar$CreateUpdate$reqAppOverrides,
										model_.config,
										$author$project$Html$Overrides$getFullOverrides(deploymentOverrides_)),
										cmd_
									])));
					} else {
						return _Utils_Tuple2(model_, cmd_);
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'Save':
				var _v8 = _Utils_Tuple2(model.deploymentOverrides, model.appOverrides);
				if ((_v8.a.$ === 'Success') && (_v8.b.$ === 'Success')) {
					var deploymentOverrides = _v8.a.a;
					var appOverrides = _v8.b.a;
					var deploymentOverridesData = $author$project$Html$Overrides$getEditedOverrides(deploymentOverrides);
					var appOverridesData = $author$project$Html$Overrides$getEditedOverrides(appOverrides);
					var info = {appOverrides: appOverridesData, deploymentOverrides: deploymentOverridesData, name: model.name};
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{saveResp: $krisajenkins$remotedata$RemoteData$Loading}),
						function () {
							var _v9 = model.mode;
							if (_v9.$ === 'Create') {
								return A2($author$project$Page$Sidebar$CreateUpdate$reqCreateDeployment, model.config, info);
							} else {
								return A3($author$project$Page$Sidebar$CreateUpdate$reqUpdateDeployment, model.name, model.config, info);
							}
						}());
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'SaveDeploymentResponse':
				if (cmd.a.$ === 'Success') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{visibility: false}),
						$elm$core$Platform$Cmd$none);
				} else {
					var resp = cmd.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{saveResp: resp}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var resp = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							deployment: resp,
							name: A3(
								$krisajenkins$remotedata$RemoteData$unwrap,
								$author$project$Types$Deployment$DeploymentName(''),
								function (x) {
									return x.deployment.name;
								},
								resp)
						}),
					$author$project$Page$Sidebar$CreateUpdate$initCreateReq(model.config));
		}
	});
var $author$project$Page$Deployment$update = F2(
	function (cmd, model) {
		var updateWith = F3(
			function (toModel, toMsg, _v8) {
				var subModel = _v8.a;
				var subCmd = _v8.b;
				return _Utils_Tuple2(
					toModel(subModel),
					A2($elm$core$Platform$Cmd$map, toMsg, subCmd));
			});
		var initOverrides = F3(
			function (name, defaults, edits) {
				return A5($author$project$Html$Overrides$init, defaults, edits, _List_Nil, $author$project$Html$Overrides$Read, name);
			});
		switch (cmd.$) {
			case 'DeploymentFullInfoResponse':
				var deployment = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{deployment: deployment}),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Page$Deployment$reqDeploymentOverrides(model.config)
							])));
			case 'DeploymentInfoResponse':
				var logs = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{logs: logs}),
					$elm$core$Platform$Cmd$none);
			case 'WSUpdate':
				var _v1 = A3($jinjor$elm_debounce$Debounce$push, $author$project$Page$Deployment$debounceConfig, _Utils_Tuple0, model.debounce);
				var debounce = _v1.a;
				var subCmd = _v1.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{debounce: debounce}),
					subCmd);
			case 'DebounceMsg':
				var msg = cmd.a;
				var restoreDisabled = A3(
					$krisajenkins$remotedata$RemoteData$unwrap,
					false,
					function (x) {
						return $author$project$Types$Deployment$isDeploymentArchived(x) ? model.restoreDisabled : false;
					},
					model.deployment);
				var _v2 = A4(
					$jinjor$elm_debounce$Debounce$update,
					$author$project$Page$Deployment$debounceConfig,
					$jinjor$elm_debounce$Debounce$takeAll(
						F2(
							function (_v3, _v4) {
								return $elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											A2($author$project$Page$Deployment$reqDeployment, model.deploymentName, model.config),
											A2($author$project$Page$Deployment$reqLogs, model.deploymentName, model.config)
										]));
							})),
					msg,
					model.debounce);
				var debounce = _v2.a;
				var subCmd = _v2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{debounce: debounce, restoreDisabled: restoreDisabled}),
					subCmd);
			case 'DeploymentOverridesResponse':
				var overrides = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							deploymentDefaults: overrides,
							deploymentOverrides: A3(
								$krisajenkins$remotedata$RemoteData$map2,
								initOverrides('Deployment configuration'),
								overrides,
								A2(
									$krisajenkins$remotedata$RemoteData$map,
									function (x) {
										return x.deployment.deploymentOverrides;
									},
									model.deployment))
						}),
					function () {
						if (overrides.$ === 'Success') {
							var defaults = overrides.a;
							return A2($author$project$Page$Deployment$reqAppOverrides, model.config, defaults);
						} else {
							return $elm$core$Platform$Cmd$none;
						}
					}());
			case 'AppOverridesResponse':
				var overrides = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							appDefaults: overrides,
							appOverrides: A3(
								$krisajenkins$remotedata$RemoteData$map2,
								initOverrides('App configuration'),
								overrides,
								A2(
									$krisajenkins$remotedata$RemoteData$map,
									function (x) {
										return x.deployment.appOverrides;
									},
									model.deployment))
						}),
					$elm$core$Platform$Cmd$none);
			case 'AppOverridesMsg':
				var subMsg = cmd.a;
				var _v6 = model.appOverrides;
				if (_v6.$ === 'Success') {
					var appOverrides = _v6.a;
					return A3(
						updateWith,
						function (updated) {
							return _Utils_update(
								model,
								{
									appOverrides: $krisajenkins$remotedata$RemoteData$Success(updated)
								});
						},
						$author$project$Page$Deployment$AppOverridesMsg,
						A2($author$project$Html$Overrides$update, subMsg, appOverrides));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'DeploymentOverridesMsg':
				var subMsg = cmd.a;
				var _v7 = model.deploymentOverrides;
				if (_v7.$ === 'Success') {
					var deploymentOverrides = _v7.a;
					return A3(
						updateWith,
						function (updated) {
							return _Utils_update(
								model,
								{
									deploymentOverrides: $krisajenkins$remotedata$RemoteData$Success(updated)
								});
						},
						$author$project$Page$Deployment$DeploymentOverridesMsg,
						A2($author$project$Html$Overrides$update, subMsg, deploymentOverrides));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'ShowSidebar':
				var deployment = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sidebar: A3($author$project$Page$Sidebar$CreateUpdate$initUpdate, model.config, true, deployment.deployment.name)
						}),
					A2(
						$elm$core$Platform$Cmd$map,
						$author$project$Page$Deployment$CreateSidebarMsg,
						A2($author$project$Page$Sidebar$CreateUpdate$initUpdateReq, model.config, deployment.deployment.name)));
			case 'CreateSidebarMsg':
				var subMsg = cmd.a;
				return A3(
					updateWith,
					function (sidebar) {
						return _Utils_update(
							model,
							{sidebar: sidebar});
					},
					$author$project$Page$Deployment$CreateSidebarMsg,
					A2($author$project$Page$Sidebar$CreateUpdate$update, subMsg, model.sidebar));
			case 'ActionMsg':
				var subMsg = cmd.a;
				return A3(
					updateWith,
					function (updated) {
						return _Utils_update(
							model,
							{actionTable: updated});
					},
					$author$project$Page$Deployment$ActionMsg,
					A2($author$project$Page$Deployment$ActionTable$update, subMsg, model.actionTable));
			case 'OpenArchivePopup':
				var deploymentName = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							archivePopup: $elm$core$Maybe$Just(deploymentName)
						}),
					$elm$core$Platform$Cmd$none);
			case 'CloseArchivePopup':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{archivePopup: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteDeploymentReq':
				var deploymentName = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{archivePopup: $elm$core$Maybe$Nothing}),
					A2($author$project$Page$Deployment$reqDeleteDeployment, model.config, deploymentName));
			case 'DeleteDeploymentResp':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'RestoreDeploymentReq':
				var deploymentName = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{restoreDisabled: true}),
					A2($author$project$Page$Deployment$reqRestoreDeployment, model.config, deploymentName));
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Page$Deployments$CreateSidebarMsg = function (a) {
	return {$: 'CreateSidebarMsg', a: a};
};
var $author$project$Page$Deployments$DebounceMsg = function (a) {
	return {$: 'DebounceMsg', a: a};
};
var $author$project$Page$Deployments$debounceConfig = {
	strategy: $jinjor$elm_debounce$Debounce$soon(2000),
	transform: $author$project$Page$Deployments$DebounceMsg
};
var $author$project$Route$routeToPieces = function (page) {
	if (page.$ === 'Deployments') {
		return _List_fromArray(
			['deployments']);
	} else {
		var deployment = page.a;
		return _List_fromArray(
			[
				'deployments',
				$author$project$Types$Deployment$unDeploymentName(deployment)
			]);
	}
};
var $author$project$Route$routeToString = function (page) {
	return '#/' + A2(
		$elm$core$String$join,
		'/',
		$author$project$Route$routeToPieces(page));
};
var $author$project$Route$pushUrl = F2(
	function (key, route) {
		return A2(
			$elm$browser$Browser$Navigation$pushUrl,
			key,
			$author$project$Route$routeToString(route));
	});
var $author$project$Page$Deployments$Table$DeleteDeploymentResp = function (a) {
	return {$: 'DeleteDeploymentResp', a: a};
};
var $author$project$Page$Deployments$Table$reqDeleteDeployment = F2(
	function (config, deploymentName) {
		return A5(
			$author$project$Api$delete,
			config,
			$author$project$Api$Endpoint$deleteDeployment(deploymentName),
			$elm$http$Http$emptyBody,
			$elm$json$Json$Decode$string,
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployments$Table$DeleteDeploymentResp));
	});
var $author$project$Page$Deployments$Table$RestoreDeploymentResp = function (a) {
	return {$: 'RestoreDeploymentResp', a: a};
};
var $author$project$Page$Deployments$Table$reqRestoreDeployment = F2(
	function (config, deploymentName) {
		return A4(
			$author$project$Api$patch,
			config,
			$author$project$Api$Endpoint$restoreDeployment(deploymentName),
			$elm$json$Json$Decode$string,
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Deployments$Table$RestoreDeploymentResp));
	});
var $author$project$Page$Deployments$Table$update = F2(
	function (cmd, model) {
		switch (cmd.$) {
			case 'SortChanged':
				var sort = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sort: sort}),
					$elm$core$Platform$Cmd$none);
			case 'OpenDeploymentOverrides':
				var name = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedDeployemntOverrides: A2($turboMaCk$any_set$Set$Any$insert, name, model.openedDeployemntOverrides)
						}),
					$elm$core$Platform$Cmd$none);
			case 'CloseDeploymentOverrides':
				var name = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedDeployemntOverrides: A2($turboMaCk$any_set$Set$Any$remove, name, model.openedDeployemntOverrides)
						}),
					$elm$core$Platform$Cmd$none);
			case 'OpenAppOverrides':
				var name = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedAppOverrides: A2($turboMaCk$any_set$Set$Any$insert, name, model.openedAppOverrides)
						}),
					$elm$core$Platform$Cmd$none);
			case 'CloseAppOverrides':
				var name = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							openedAppOverrides: A2($turboMaCk$any_set$Set$Any$remove, name, model.openedAppOverrides)
						}),
					$elm$core$Platform$Cmd$none);
			case 'OpenMenu':
				var name = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							menuButton: $elm$core$Maybe$Just(name)
						}),
					$elm$core$Platform$Cmd$none);
			case 'CloseMenu':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{menuButton: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'OpenArchivePopup':
				var deploymentName = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							archivePopup: $elm$core$Maybe$Just(deploymentName)
						}),
					$elm$core$Platform$Cmd$none);
			case 'CloseArchivePopup':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{archivePopup: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteDeploymentReq':
				var deploymentName = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{archivePopup: $elm$core$Maybe$Nothing}),
					A2($author$project$Page$Deployments$Table$reqDeleteDeployment, model.config, deploymentName));
			case 'DeleteDeploymentResp':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'RestoreDeploymentReq':
				var deploymentName = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{menuButton: $elm$core$Maybe$Nothing}),
					A2($author$project$Page$Deployments$Table$reqRestoreDeployment, model.config, deploymentName));
			case 'RestoreDeploymentResp':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'GoToDeployment':
				var deploymentName = cmd.a;
				return _Utils_Tuple2(
					model,
					A2(
						$author$project$Route$pushUrl,
						model.settings.navKey,
						$author$project$Route$Deployment(deploymentName)));
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Page$Deployments$update = F2(
	function (cmd, model) {
		var updateWith = F3(
			function (toModel, toMsg, _v6) {
				var subModel = _v6.a;
				var subCmd = _v6.b;
				return _Utils_Tuple2(
					toModel(subModel),
					A2($elm$core$Platform$Cmd$map, toMsg, subCmd));
			});
		switch (cmd.$) {
			case 'DeploymentsResponse':
				var deployments = cmd.a;
				var latestUpd = function () {
					if (deployments.$ === 'Success') {
						return 0;
					} else {
						return model.updated;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{deployments: deployments, updated: latestUpd}),
					$elm$core$Platform$Cmd$none);
			case 'SearchInput':
				var search = cmd.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{search: search}),
					$elm$core$Platform$Cmd$none);
			case 'ActiveTableMsg':
				if (cmd.a.$ === 'ShowEditSidebar') {
					var deploymentName = cmd.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								sidebar: A3($author$project$Page$Sidebar$CreateUpdate$initUpdate, model.config, true, deploymentName)
							}),
						A2(
							$elm$core$Platform$Cmd$map,
							$author$project$Page$Deployments$CreateSidebarMsg,
							A2($author$project$Page$Sidebar$CreateUpdate$initUpdateReq, model.config, deploymentName)));
				} else {
					var subMsg = cmd.a;
					return A3(
						updateWith,
						function (table) {
							return _Utils_update(
								model,
								{activeTable: table});
						},
						$author$project$Page$Deployments$ActiveTableMsg,
						A2($author$project$Page$Deployments$Table$update, subMsg, model.activeTable));
				}
			case 'ArchivedTableMsg':
				var subMsg = cmd.a;
				return A3(
					updateWith,
					function (table) {
						return _Utils_update(
							model,
							{archivedTable: table});
					},
					$author$project$Page$Deployments$ArchivedTableMsg,
					A2($author$project$Page$Deployments$Table$update, subMsg, model.archivedTable));
			case 'ToggleArchived':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{showArchived: !model.showArchived}),
					$elm$core$Platform$Cmd$none);
			case 'WSUpdate':
				var _v2 = A3($jinjor$elm_debounce$Debounce$push, $author$project$Page$Deployments$debounceConfig, _Utils_Tuple0, model.debounce);
				var debounce = _v2.a;
				var subCmd = _v2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{debounce: debounce}),
					subCmd);
			case 'ShowCreateSidebar':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sidebar: A2($author$project$Page$Sidebar$CreateUpdate$initCreate, model.config, true)
						}),
					A2(
						$elm$core$Platform$Cmd$map,
						$author$project$Page$Deployments$CreateSidebarMsg,
						$author$project$Page$Sidebar$CreateUpdate$initCreateReq(model.config)));
			case 'CreateSidebarMsg':
				var subMsg = cmd.a;
				return A3(
					updateWith,
					function (sidebar) {
						return _Utils_update(
							model,
							{sidebar: sidebar});
					},
					$author$project$Page$Deployments$CreateSidebarMsg,
					A2($author$project$Page$Sidebar$CreateUpdate$update, subMsg, model.sidebar));
			case 'DebounceMsg':
				var msg = cmd.a;
				var _v3 = A4(
					$jinjor$elm_debounce$Debounce$update,
					$author$project$Page$Deployments$debounceConfig,
					$jinjor$elm_debounce$Debounce$takeAll(
						F2(
							function (_v4, _v5) {
								return $author$project$Page$Deployments$reqDeployments(model.config);
							})),
					msg,
					model.debounce);
				var debounce = _v3.a;
				var subCmd = _v3.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{debounce: debounce}),
					subCmd);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{updated: model.updated + $author$project$Page$Deployments$tick}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Page$Initialization$getWebsocketAddress = _Platform_outgoingPort('getWebsocketAddress', $elm$json$Json$Encode$string);
var $elm$browser$Browser$Navigation$replaceUrl = _Browser_replaceUrl;
var $author$project$Route$replaceUrl = F2(
	function (key, route) {
		return A2(
			$elm$browser$Browser$Navigation$replaceUrl,
			key,
			$author$project$Route$routeToString(route));
	});
var $author$project$Page$Initialization$ConfigResponse = function (a) {
	return {$: 'ConfigResponse', a: a};
};
var $author$project$Config$configDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'kubernetes_dashboard_url_template',
	$elm$json$Json$Decode$string,
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'app_auth',
		A2($elm$json$Json$Decode$map, $author$project$Config$AppAuth, $elm$json$Json$Decode$string),
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'ws_url',
			$elm$json$Json$Decode$string,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'app_url',
				A2($elm$json$Json$Decode$map, $author$project$Config$AppUrl, $elm$json$Json$Decode$string),
				$elm$json$Json$Decode$succeed($author$project$Config$Config)))));
var $author$project$Api$Endpoint$configJson = A3(
	$author$project$Api$Endpoint$url,
	$elm$core$Maybe$Nothing,
	_List_fromArray(
		['config.json']),
	_List_Nil);
var $author$project$Page$Initialization$reqConfig = $author$project$Api$Endpoint$request(
	{
		body: $elm$http$Http$emptyBody,
		expect: A2(
			$author$project$Api$expectJson,
			A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Initialization$ConfigResponse),
			$author$project$Config$configDecoder),
		headers: _List_Nil,
		method: 'GET',
		timeout: $elm$core$Maybe$Nothing,
		tracker: $elm$core$Maybe$Nothing,
		url: $author$project$Api$Endpoint$configJson
	});
var $author$project$Page$Initialization$ProjectNameResponse = function (a) {
	return {$: 'ProjectNameResponse', a: a};
};
var $author$project$Api$Endpoint$projectName = function (appUrl) {
	return A3(
		$author$project$Api$Endpoint$url,
		$elm$core$Maybe$Just(appUrl),
		_List_fromArray(
			['api', 'v1', 'project_name']),
		_List_Nil);
};
var $author$project$Page$Initialization$reqProjectName = function (cfg) {
	return A4(
		$author$project$Api$get,
		cfg,
		$author$project$Api$Endpoint$projectName,
		$elm$json$Json$Decode$string,
		A2($elm$core$Basics$composeR, $krisajenkins$remotedata$RemoteData$fromResult, $author$project$Page$Initialization$ProjectNameResponse));
};
var $author$project$Config$setProjectName = F2(
	function (settings, projectName) {
		return _Utils_update(
			settings,
			{projectName: projectName});
	});
var $author$project$Config$setZone = F2(
	function (settings, zone) {
		return _Utils_update(
			settings,
			{zone: zone});
	});
var $author$project$Page$Initialization$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ConfigResponse':
				var webData = msg.a;
				if (webData.$ === 'Success') {
					var config = webData.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								config: $krisajenkins$remotedata$RemoteData$Success(config)
							}),
						$elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									$author$project$Page$Initialization$reqProjectName(config),
									$author$project$Page$Initialization$getWebsocketAddress(config.wsUrl + '/event')
								])));
				} else {
					var a = webData;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{config: a}),
						$elm$core$Platform$Cmd$none);
				}
			case 'ProjectNameResponse':
				var webData = msg.a;
				if (webData.$ === 'Success') {
					var projectName = webData.a;
					var _v3 = $author$project$Route$fromUrl(model.url);
					if (_v3.$ === 'Nothing') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									projectName: $krisajenkins$remotedata$RemoteData$Success(projectName),
									settings: A2($author$project$Config$setProjectName, model.settings, projectName)
								}),
							A2($author$project$Route$replaceUrl, model.settings.navKey, $author$project$Route$Deployments));
					} else {
						var route = _v3.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									projectName: $krisajenkins$remotedata$RemoteData$Success(projectName),
									settings: A2($author$project$Config$setProjectName, model.settings, projectName)
								}),
							A2($author$project$Route$replaceUrl, model.settings.navKey, route));
					}
				} else {
					var a = webData;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{projectName: a}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var zone = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							settings: A2($author$project$Config$setZone, model.settings, zone)
						}),
					$author$project$Page$Initialization$reqConfig);
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var _v0 = _Utils_Tuple2(msg, model);
		_v0$5:
		while (true) {
			switch (_v0.a.$) {
				case 'LinkClicked':
					var urlRequest = _v0.a.a;
					if (urlRequest.$ === 'Internal') {
						var url = urlRequest.a;
						return _Utils_Tuple2(
							model,
							A2(
								$elm$browser$Browser$Navigation$pushUrl,
								$author$project$Main$getNavKey(model),
								$elm$url$Url$toString(url)));
					} else {
						var href = urlRequest.a;
						return _Utils_Tuple2(
							model,
							$elm$browser$Browser$Navigation$load(href));
					}
				case 'UrlChanged':
					var url = _v0.a.a;
					return A2(
						$author$project$Main$changeRouteTo,
						$author$project$Route$fromUrl(url),
						model);
				case 'InitializationMsg':
					if (_v0.b.$ === 'Initialization') {
						var subMsg = _v0.a.a;
						var initialization = _v0.b.a;
						return A3(
							$author$project$Main$updateWith,
							$author$project$Main$Initialization,
							$author$project$Main$InitializationMsg,
							A2($author$project$Page$Initialization$update, subMsg, initialization));
					} else {
						break _v0$5;
					}
				case 'DeploymentsMsg':
					if (_v0.b.$ === 'Deployments') {
						var subMsg = _v0.a.a;
						var deployments = _v0.b.a;
						return A3(
							$author$project$Main$updateWith,
							$author$project$Main$Deployments,
							$author$project$Main$DeploymentsMsg,
							A2($author$project$Page$Deployments$update, subMsg, deployments));
					} else {
						break _v0$5;
					}
				default:
					if (_v0.b.$ === 'Deployment') {
						var subMsg = _v0.a.a;
						var deployments = _v0.b.a;
						return A3(
							$author$project$Main$updateWith,
							$author$project$Main$Deployment,
							$author$project$Main$DeploymentMsg,
							A2($author$project$Page$Deployment$update, subMsg, deployments));
					} else {
						break _v0$5;
					}
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Page$Deployment = {$: 'Deployment'};
var $author$project$Page$Deployments = {$: 'Deployments'};
var $author$project$Page$Initialization = {$: 'Initialization'};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$html$Html$b = _VirtualDom_node('b');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Html$Common$elClass = F3(
	function (el, cls, body) {
		return A2(
			el,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(cls)
				]),
			body);
	});
var $author$project$Html$Common$bClass = $author$project$Html$Common$elClass($elm$html$Html$b);
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Html$Common$divClass = $author$project$Html$Common$elClass($elm$html$Html$div);
var $elm$html$Html$header = _VirtualDom_node('header');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Page$viewHeader = function (projectName) {
	return A2(
		$elm$html$Html$header,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('header')
			]),
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'header__wrap container',
				_List_fromArray(
					[
						A2(
						$author$project$Html$Common$bClass,
						'header__logo',
						_List_fromArray(
							[
								$elm$html$Html$text('Deployment Manager')
							])),
						A2(
						$author$project$Html$Common$divClass,
						'header__project',
						_List_fromArray(
							[
								$elm$html$Html$text(projectName)
							]))
					]))
			]));
};
var $author$project$Page$view = F3(
	function (projectName, page, _v0) {
		var title = _v0.title;
		var content = _v0.content;
		return {
			body: A2(
				$elm$core$List$cons,
				$author$project$Page$viewHeader(projectName),
				content),
			title: title
		};
	});
var $author$project$Page$Deployment$CloseArchivePopup = {$: 'CloseArchivePopup'};
var $author$project$Page$Deployment$DeleteDeploymentReq = function (a) {
	return {$: 'DeleteDeploymentReq', a: a};
};
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$button = _VirtualDom_node('button');
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
var $author$project$Html$Common$buttonClass = F2(
	function (cls, msg) {
		return $elm$html$Html$button(
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(msg),
					$elm$html$Html$Attributes$class(cls)
				]));
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Page$Deployment$archivePopupView = function (deploymentName) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('classic-popup'),
				A2($elm$html$Html$Attributes$style, 'display', 'block')
			]),
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'classic-popup__container',
				_List_fromArray(
					[
						A2(
						$author$project$Html$Common$divClass,
						'classic-popup__viewport',
						_List_fromArray(
							[
								A2(
								$author$project$Html$Common$divClass,
								'classic-popup__slot',
								_List_fromArray(
									[
										A2(
										$author$project$Html$Common$divClass,
										'dialog dialog--archive',
										_List_fromArray(
											[
												A2(
												$author$project$Html$Common$divClass,
												'dialog__content',
												_List_fromArray(
													[
														$elm$html$Html$text('Are you sure you want to archive the'),
														A2($elm$html$Html$br, _List_Nil, _List_Nil),
														$elm$html$Html$text(
														$author$project$Types$Deployment$unDeploymentName(deploymentName)),
														$elm$html$Html$text(' deployment?')
													])),
												A2(
												$author$project$Html$Common$divClass,
												'dialog__footer',
												_List_fromArray(
													[
														A3(
														$author$project$Html$Common$buttonClass,
														'button dialog__action',
														$author$project$Page$Deployment$DeleteDeploymentReq(deploymentName),
														_List_fromArray(
															[
																$elm$html$Html$text('Archive')
															])),
														A3(
														$author$project$Html$Common$buttonClass,
														'button dialog__action button--secondary',
														$author$project$Page$Deployment$CloseArchivePopup,
														_List_fromArray(
															[
																$elm$html$Html$text('Cancel')
															]))
													]))
											])),
										A3($author$project$Html$Common$buttonClass, 'classic-popup__close', $author$project$Page$Deployment$CloseArchivePopup, _List_Nil)
									]))
							]))
					]))
			]));
};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $author$project$Route$href = function (targetRoute) {
	return $elm$html$Html$Attributes$href(
		$author$project$Route$routeToString(targetRoute));
};
var $author$project$Html$Common$aClassHrefInternal = F2(
	function (cls, hrf) {
		return $elm$html$Html$a(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(cls),
					$author$project$Route$href(hrf)
				]));
	});
var $author$project$Page$Deployment$backButton = A3(
	$author$project$Html$Common$aClassHrefInternal,
	'page__back dash dash--back dash--smaller',
	$author$project$Route$Deployments,
	_List_fromArray(
		[
			$elm$html$Html$text('All deployments')
		]));
var $author$project$Page$Deployment$OpenArchivePopup = function (a) {
	return {$: 'OpenArchivePopup', a: a};
};
var $author$project$Page$Deployment$RestoreDeploymentReq = function (a) {
	return {$: 'RestoreDeploymentReq', a: a};
};
var $author$project$Page$Deployment$ShowSidebar = function (a) {
	return {$: 'ShowSidebar', a: a};
};
var $elm$html$Html$Attributes$target = $elm$html$Html$Attributes$stringProperty('target');
var $author$project$Html$Common$aClassHrefExternal = F2(
	function (cls, hrf) {
		return $elm$html$Html$a(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(cls),
					$elm$html$Html$Attributes$href(hrf),
					$elm$html$Html$Attributes$target('_blank')
				]));
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $author$project$Html$Common$h1Class = $author$project$Html$Common$elClass($elm$html$Html$h1);
var $author$project$Types$Deployment$unStatus = function (status) {
	if (status.$ === 'DeploymentPending') {
		var ds = status.a;
		return ds;
	} else {
		var ds = status.a;
		return ds;
	}
};
var $author$project$Types$Deployment$isPending = function (status) {
	var _v0 = $author$project$Types$Deployment$unStatus(status);
	switch (_v0.$) {
		case 'Running':
			return false;
		case 'Failure':
			return false;
		case 'Archived':
			return false;
		case 'CreatePending':
			return true;
		case 'UpdatePending':
			return true;
		case 'ArchivePending':
			return true;
		default:
			return true;
	}
};
var $author$project$Types$Deployment$isDeploymentPending = function (status) {
	if (status.$ === 'DeploymentPending') {
		return true;
	} else {
		return $author$project$Types$Deployment$isPending(status);
	}
};
var $author$project$Page$Deployment$deploymentHeaderView = function (model) {
	var disabledButtonClass = F2(
		function (cls, disabled) {
			return disabled ? (cls + ' button--disabled') : cls;
		});
	var buttons = function () {
		var _v0 = model.deployment;
		if (_v0.$ === 'Success') {
			var deployment = _v0.a;
			return $author$project$Types$Deployment$isDeploymentPending(deployment.status) ? _List_fromArray(
				[
					A3(
					$author$project$Html$Common$aClassHrefExternal,
					'button button--logs page__action button--secondary',
					_Utils_ap(
						model.config.k8sDashboardUrlTemplate,
						$author$project$Types$Deployment$unDeploymentName(model.deploymentName)),
					_List_fromArray(
						[
							$elm$html$Html$text('Details')
						]))
				]) : ($author$project$Types$Deployment$isDeploymentArchived(deployment) ? _List_fromArray(
				[
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(disabledButtonClass, 'button button--restore page__action button--secondary', model.restoreDisabled)),
							$elm$html$Html$Events$onClick(
							$author$project$Page$Deployment$RestoreDeploymentReq(model.deploymentName)),
							$elm$html$Html$Attributes$disabled(model.restoreDisabled)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Restore from archive')
						])),
					A3(
					$author$project$Html$Common$aClassHrefExternal,
					'button button--logs page__action button--secondary',
					_Utils_ap(
						model.config.k8sDashboardUrlTemplate,
						$author$project$Types$Deployment$unDeploymentName(model.deploymentName)),
					_List_fromArray(
						[
							$elm$html$Html$text('Details')
						]))
				]) : _List_fromArray(
				[
					A3(
					$author$project$Html$Common$buttonClass,
					'button button--edit page__action',
					$author$project$Page$Deployment$ShowSidebar(deployment),
					_List_fromArray(
						[
							$elm$html$Html$text('Edit deployment')
						])),
					A3(
					$author$project$Html$Common$buttonClass,
					'button button--archive page__action button--secondary',
					$author$project$Page$Deployment$OpenArchivePopup(model.deploymentName),
					_List_fromArray(
						[
							$elm$html$Html$text('Move to archive')
						])),
					A3(
					$author$project$Html$Common$aClassHrefExternal,
					'button button--logs page__action button--secondary',
					_Utils_ap(
						model.config.k8sDashboardUrlTemplate,
						$author$project$Types$Deployment$unDeploymentName(model.deploymentName)),
					_List_fromArray(
						[
							$elm$html$Html$text('Details')
						]))
				]));
		} else {
			return _List_Nil;
		}
	}();
	return A2(
		$author$project$Html$Common$divClass,
		'page__head',
		A2(
			$elm$core$List$cons,
			A2(
				$author$project$Html$Common$h1Class,
				'page__heading title',
				_List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$Types$Deployment$unDeploymentName(model.deploymentName))
					])),
			buttons));
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $author$project$Html$Common$h3Class = $author$project$Html$Common$elClass($elm$html$Html$h3);
var $elm$html$Html$table = _VirtualDom_node('table');
var $author$project$Page$Deployment$ActionTable$CloseAppOverrides = function (a) {
	return {$: 'CloseAppOverrides', a: a};
};
var $author$project$Page$Deployment$ActionTable$CloseDeploymentOverrides = function (a) {
	return {$: 'CloseDeploymentOverrides', a: a};
};
var $author$project$Page$Deployment$ActionTable$OpenAppOverrides = function (a) {
	return {$: 'OpenAppOverrides', a: a};
};
var $author$project$Page$Deployment$ActionTable$OpenDeploymentOverrides = function (a) {
	return {$: 'OpenDeploymentOverrides', a: a};
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth = function (month) {
	switch (month.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $elm$time$Time$toMillis = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			1000,
			$elm$time$Time$posixToMillis(time));
	});
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $elm$time$Time$Apr = {$: 'Apr'};
var $elm$time$Time$Aug = {$: 'Aug'};
var $elm$time$Time$Dec = {$: 'Dec'};
var $elm$time$Time$Feb = {$: 'Feb'};
var $elm$time$Time$Jan = {$: 'Jan'};
var $elm$time$Time$Jul = {$: 'Jul'};
var $elm$time$Time$Jun = {$: 'Jun'};
var $elm$time$Time$Mar = {$: 'Mar'};
var $elm$time$Time$May = {$: 'May'};
var $elm$time$Time$Nov = {$: 'Nov'};
var $elm$time$Time$Oct = {$: 'Oct'};
var $elm$time$Time$Sep = {$: 'Sep'};
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_v0) {
			case 1:
				return $elm$time$Time$Jan;
			case 2:
				return $elm$time$Time$Feb;
			case 3:
				return $elm$time$Time$Mar;
			case 4:
				return $elm$time$Time$Apr;
			case 5:
				return $elm$time$Time$May;
			case 6:
				return $elm$time$Time$Jun;
			case 7:
				return $elm$time$Time$Jul;
			case 8:
				return $elm$time$Time$Aug;
			case 9:
				return $elm$time$Time$Sep;
			case 10:
				return $elm$time$Time$Oct;
			case 11:
				return $elm$time$Time$Nov;
			default:
				return $elm$time$Time$Dec;
		}
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString = F2(
	function (digits, time) {
		return A3(
			$elm$core$String$padLeft,
			digits,
			_Utils_chr('0'),
			$elm$core$String$fromInt(time));
	});
var $elm$time$Time$toSecond = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				1000));
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime = function (time) {
	return A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		4,
		A2($elm$time$Time$toYear, $elm$time$Time$utc, time)) + ('-' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		$rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth(
			A2($elm$time$Time$toMonth, $elm$time$Time$utc, time))) + ('-' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toDay, $elm$time$Time$utc, time)) + ('T' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toHour, $elm$time$Time$utc, time)) + (':' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toMinute, $elm$time$Time$utc, time)) + (':' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toSecond, $elm$time$Time$utc, time)) + ('.' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		3,
		A2($elm$time$Time$toMillis, $elm$time$Time$utc, time)) + 'Z'))))))))))));
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $turboMaCk$any_dict$Dict$Any$member = F2(
	function (k, _v0) {
		var dict = _v0.a.dict;
		var toKey = _v0.a.toKey;
		return A2(
			$elm$core$Dict$member,
			toKey(k),
			dict);
	});
var $turboMaCk$any_set$Set$Any$member = F2(
	function (a, _v0) {
		var dict = _v0.a;
		return A2($turboMaCk$any_dict$Dict$Any$member, a, dict);
	});
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
var $elm_community$html_extra$Html$Events$Extra$onClickStopPropagation = function (msg) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'click',
		$elm$json$Json$Decode$succeed(
			_Utils_Tuple2(msg, true)));
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Html$Common$spanClass = $author$project$Html$Common$elClass($elm$html$Html$span);
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
var $author$project$Page$Deployment$ActionTable$overridesView = F5(
	function (openCmd, closeCmd, id, opened, overrides) {
		var visibleOverrides = A2($elm$core$List$take, 3, overrides);
		var overrideView = function (override) {
			return A2(
				$author$project$Html$Common$divClass,
				'row',
				function () {
					var _v0 = override.value;
					if (_v0.$ === 'ValueAdded') {
						var v = _v0.a;
						return _List_fromArray(
							[
								A2(
								$author$project$Html$Common$spanClass,
								'key-default-pristine',
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Types$Override$unOverrideName(override.name) + ': ')
									])),
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('value-edited'),
										A2($elm$html$Html$Attributes$attribute, 'style', 'width:400px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:block;')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(v)
									]))
							]);
					} else {
						return _List_fromArray(
							[
								A2(
								$author$project$Html$Common$spanClass,
								'key-deleted',
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Types$Override$unOverrideName(override.name) + ': ')
									])),
								A2($author$project$Html$Common$divClass, 'listing__placeholder listing__placeholder__value', _List_Nil)
							]);
					}
				}());
		};
		var buttonClass_ = F3(
			function (cls, msg, body) {
				return A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(cls),
							$elm_community$html_extra$Html$Events$Extra$onClickStopPropagation(msg)
						]),
					body);
			});
		return _Utils_eq(
			$elm$core$List$length(overrides),
			$elm$core$List$length(visibleOverrides)) ? A2($elm$core$List$map, overrideView, overrides) : (A2($turboMaCk$any_set$Set$Any$member, id, opened) ? A2(
			$elm$core$List$append,
			A2($elm$core$List$map, overrideView, overrides),
			_List_fromArray(
				[
					A3(
					buttonClass_,
					'expander listing__more expander--open',
					closeCmd(id),
					_List_fromArray(
						[
							$elm$html$Html$text('Hide default configuration')
						]))
				])) : A2(
			$elm$core$List$append,
			A2($elm$core$List$map, overrideView, visibleOverrides),
			_List_fromArray(
				[
					A3(
					buttonClass_,
					'expander listing__more',
					openCmd(id),
					_List_fromArray(
						[
							$elm$html$Html$text('Show full configuration')
						]))
				])));
	});
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Types$Action$unExitCode = function (_v0) {
	var code = _v0.a;
	return code;
};
var $author$project$Page$Deployment$ActionTable$logView = F2(
	function (model, log) {
		var name = function () {
			var _v0 = log.action;
			switch (_v0.$) {
				case 'Create':
					return 'create';
				case 'Restore':
					return 'restore';
				case 'Update':
					return 'update';
				default:
					return 'archive';
			}
		}();
		var exitCode = _List_fromArray(
			[
				$elm$html$Html$text(
				$elm$core$String$fromInt(
					$author$project$Types$Action$unExitCode(log.exitCode)))
			]);
		var duration = _List_fromArray(
			[
				$elm$html$Html$text(
				$elm$core$String$fromInt(
					$elm$core$Basics$round(log.duration.time)) + 's')
			]);
		var deploymentOverrides = _List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'row',
				A5($author$project$Page$Deployment$ActionTable$overridesView, $author$project$Page$Deployment$ActionTable$OpenDeploymentOverrides, $author$project$Page$Deployment$ActionTable$CloseDeploymentOverrides, log.actionId, model.openedDeployemntOverrides, log.deploymentOverrides))
			]);
		var appOverrides = _List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'row',
				A5($author$project$Page$Deployment$ActionTable$overridesView, $author$project$Page$Deployment$ActionTable$OpenAppOverrides, $author$project$Page$Deployment$ActionTable$CloseAppOverrides, log.actionId, model.openedAppOverrides, log.appOverrides))
			]);
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name)
						])),
					A2($elm$html$Html$td, _List_Nil, deploymentOverrides),
					A2($elm$html$Html$td, _List_Nil, appOverrides),
					A2($elm$html$Html$td, _List_Nil, exitCode),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime(log.createdAt))
						])),
					A2($elm$html$Html$td, _List_Nil, duration)
				]));
	});
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $author$project$Page$Deployment$ActionTable$tableDeploymentsView = F2(
	function (model, logs) {
		return A2(
			$elm$html$Html$tbody,
			_List_Nil,
			A2(
				$elm$core$List$map,
				$author$project$Page$Deployment$ActionTable$logView(model),
				logs));
	});
var $elm$html$Html$Attributes$colspan = function (n) {
	return A2(
		_VirtualDom_attribute,
		'colspan',
		$elm$core$String$fromInt(n));
};
var $author$project$Html$Common$trClass = $author$project$Html$Common$elClass($elm$html$Html$tr);
var $author$project$Page$Deployment$ActionTable$tableFailureView = A2(
	$elm$html$Html$tbody,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$author$project$Html$Common$trClass,
			'no-table',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$colspan(8)
						]),
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$divClass,
							'null null--data',
							_List_fromArray(
								[
									A2(
									$author$project$Html$Common$bClass,
									'null__heading',
									_List_fromArray(
										[
											$elm$html$Html$text('Cannot retrieve the data')
										])),
									A2(
									$author$project$Html$Common$divClass,
									'null__message',
									_List_fromArray(
										[
											$elm$html$Html$text('Try to reload page')
										]))
								]))
						]))
				]))
		]));
var $author$project$Page$Deployment$ActionTable$tableLoadingView = A2(
	$elm$html$Html$tbody,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$author$project$Html$Common$trClass,
			'no-table',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$colspan(8)
						]),
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$divClass,
							'loading loading--enlarged loading--alternate',
							_List_fromArray(
								[
									$elm$html$Html$text('Loading...')
								]))
						]))
				]))
		]));
var $author$project$Page$Deployment$ActionTable$tablePrimaryBodyView = F2(
	function (model, deployments) {
		switch (deployments.$) {
			case 'Success':
				var d = deployments.a;
				return A2(
					$author$project$Page$Deployment$ActionTable$tableDeploymentsView,
					model,
					A2(
						$elm$core$List$concatMap,
						function ($) {
							return $.logs;
						},
						d));
			case 'Failure':
				return $author$project$Page$Deployment$ActionTable$tableFailureView;
			default:
				return $author$project$Page$Deployment$ActionTable$tableLoadingView;
		}
	});
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $author$project$Page$Deployment$ActionTable$tablePrimaryHeaderView = function () {
	var simpleTh = function (txt) {
		return A2(
			$elm$html$Html$th,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(txt)
				]));
	};
	return A2(
		$elm$html$Html$thead,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						simpleTh('Action Type'),
						simpleTh('Deployment configuration'),
						simpleTh('App configuration'),
						simpleTh('Exit code'),
						simpleTh('Created'),
						simpleTh('Deployment duration')
					]))
			]));
}();
var $author$project$Page$Deployment$ActionTable$view = F2(
	function (model, logs) {
		return A2(
			$author$project$Html$Common$divClass,
			'table table--actions',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$table,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Page$Deployment$ActionTable$tablePrimaryHeaderView,
							A2($author$project$Page$Deployment$ActionTable$tablePrimaryBodyView, model, logs)
						]))
				]));
	});
var $author$project$Page$Deployment$actionsView = function (model) {
	return A2(
		$author$project$Html$Common$divClass,
		'deployment__section',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$h3Class,
				'deployment__sub-heading',
				_List_fromArray(
					[
						$elm$html$Html$text('Actions')
					])),
				A2(
				$author$project$Html$Common$divClass,
				'deployment__widget',
				_List_fromArray(
					[
						A2(
						$elm$html$Html$map,
						$author$project$Page$Deployment$ActionMsg,
						A2($author$project$Page$Deployment$ActionTable$view, model.actionTable, model.logs))
					]))
			]));
};
var $author$project$Page$Deployment$deploymentLinksView = F2(
	function (_v0, deployment) {
		var link = function (m) {
			return A3(
				$author$project$Html$Common$aClassHrefExternal,
				'listing__item external bar bar--larger',
				m.link,
				_List_fromArray(
					[
						$elm$html$Html$text(m.name)
					]));
		};
		return A2(
			$author$project$Html$Common$divClass,
			'deployment__section',
			_List_fromArray(
				[
					A2(
					$author$project$Html$Common$h3Class,
					'deployment__sub-heading',
					_List_fromArray(
						[
							$elm$html$Html$text('Links')
						])),
					A2(
					$author$project$Html$Common$divClass,
					'deployment__widget',
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$divClass,
							'listing',
							A2($elm$core$List$map, link, deployment.metadata))
						]))
				]));
	});
var $author$project$Html$Common$dateView = F2(
	function (zone, time) {
		var year = $elm$core$String$fromInt(
			A2($elm$time$Time$toYear, zone, time));
		var toMonth = function (month_) {
			switch (month_.$) {
				case 'Jan':
					return '01';
				case 'Feb':
					return '02';
				case 'Mar':
					return '03';
				case 'Apr':
					return '04';
				case 'May':
					return '05';
				case 'Jun':
					return '06';
				case 'Jul':
					return '07';
				case 'Aug':
					return '08';
				case 'Sep':
					return '09';
				case 'Oct':
					return '10';
				case 'Nov':
					return '11';
				default:
					return '12';
			}
		};
		var month = toMonth(
			A2($elm$time$Time$toMonth, zone, time));
		var day = A3(
			$elm$core$String$padLeft,
			2,
			_Utils_chr('0'),
			$elm$core$String$fromInt(
				A2($elm$time$Time$toDay, zone, time)));
		return $elm$html$Html$text(
			A2(
				$elm$core$String$join,
				'-',
				_List_fromArray(
					[year, month, day])));
	});
var $author$project$Page$Deployment$deploymentSummaryView = F2(
	function (model, deployment) {
		var statusView = function () {
			var _v0 = deployment.status;
			if (_v0.$ === 'DeploymentPending') {
				return A2(
					$author$project$Html$Common$divClass,
					'status status--pending',
					_List_fromArray(
						[
							$elm$html$Html$text('Pending...')
						]));
			} else {
				switch (_v0.a.$) {
					case 'Running':
						var _v1 = _v0.a;
						return A2(
							$author$project$Html$Common$divClass,
							'status status--success',
							_List_fromArray(
								[
									$elm$html$Html$text('Running')
								]));
					case 'Failure':
						return A2(
							$author$project$Html$Common$divClass,
							'status status--failure',
							_List_fromArray(
								[
									$elm$html$Html$text('Failure')
								]));
					case 'CreatePending':
						var _v2 = _v0.a;
						return A2(
							$author$project$Html$Common$divClass,
							'loading loading--status-alike',
							_List_fromArray(
								[
									$elm$html$Html$text('Creating...')
								]));
					case 'UpdatePending':
						var _v3 = _v0.a;
						return A2(
							$author$project$Html$Common$divClass,
							'loading loading--status-alike',
							_List_fromArray(
								[
									$elm$html$Html$text('Updating...')
								]));
					case 'ArchivePending':
						var _v4 = _v0.a;
						return A2(
							$author$project$Html$Common$divClass,
							'loading loading--status-alike',
							_List_fromArray(
								[
									$elm$html$Html$text('Archiving...')
								]));
					case 'Archived':
						var _v5 = _v0.a;
						return A2(
							$author$project$Html$Common$divClass,
							'status status--archived',
							_List_fromArray(
								[
									$elm$html$Html$text('Archived')
								]));
					default:
						var _v6 = _v0.a;
						return A2(
							$author$project$Html$Common$divClass,
							'status status--failure',
							_List_fromArray(
								[
									$elm$html$Html$text('Cleanup failed (contact admin)')
								]));
				}
			}
		}();
		return A2(
			$author$project$Html$Common$divClass,
			'deployment__summary',
			_List_fromArray(
				[
					A2(
					$author$project$Html$Common$divClass,
					'deployment__stat',
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$bClass,
							'deployment__param',
							_List_fromArray(
								[
									$elm$html$Html$text('Status')
								])),
							A2(
							$author$project$Html$Common$divClass,
							'deployment__value',
							_List_fromArray(
								[statusView]))
						])),
					A2(
					$author$project$Html$Common$divClass,
					'deployment__stat',
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$bClass,
							'deployment__param',
							_List_fromArray(
								[
									$elm$html$Html$text('Created')
								])),
							A2(
							$author$project$Html$Common$divClass,
							'deployment__value',
							_List_fromArray(
								[
									A2($author$project$Html$Common$dateView, model.settings.zone, deployment.createdAt)
								]))
						])),
					A2(
					$author$project$Html$Common$divClass,
					'deployment__stat',
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$bClass,
							'deployment__param',
							_List_fromArray(
								[
									$elm$html$Html$text('Changed')
								])),
							A2(
							$author$project$Html$Common$divClass,
							'deployment__value',
							_List_fromArray(
								[
									A2($author$project$Html$Common$dateView, model.settings.zone, deployment.updatedAt)
								]))
						]))
				]));
	});
var $author$project$Html$Overrides$overridesSectionLoading = function (mode) {
	var rowLoader = A2(
		$author$project$Html$Common$divClass,
		'editable-row loader',
		_List_fromArray(
			[
				A2($author$project$Html$Common$divClass, 'editable-row__placeholder', _List_Nil),
				A2($author$project$Html$Common$divClass, 'editable-row__placeholder', _List_Nil),
				A2($author$project$Html$Common$divClass, 'overrides__delete spot spot--loader', _List_Nil)
			]));
	var addBtn = function () {
		if (mode.$ === 'Read') {
			return _List_Nil;
		} else {
			return _List_fromArray(
				[
					A2(
					$author$project$Html$Common$divClass,
					'padded',
					_List_fromArray(
						[
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('dash--disabled dash dash--add overrides__add'),
									$elm$html$Html$Attributes$disabled(true)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Add an override')
								]))
						]))
				]);
		}
	}();
	return A2(
		$author$project$Html$Common$divClass,
		'deployment__widget',
		_Utils_ap(
			addBtn,
			_List_fromArray(
				[rowLoader, rowLoader, rowLoader])));
};
var $author$project$Html$Overrides$AddOverride = {$: 'AddOverride'};
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $author$project$Html$Overrides$DeleteOverride = function (a) {
	return {$: 'DeleteOverride', a: a};
};
var $author$project$Html$Overrides$RestoreOverride = function (a) {
	return {$: 'RestoreOverride', a: a};
};
var $author$project$Html$Overrides$EditOverrideName = F2(
	function (a, b) {
		return {$: 'EditOverrideName', a: a, b: b};
	});
var $author$project$Html$Overrides$HideAutocomplete = {$: 'HideAutocomplete'};
var $author$project$Html$Overrides$ShowAutocomplete = function (a) {
	return {$: 'ShowAutocomplete', a: a};
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$Events$onBlur = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'blur',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
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
var $elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Html$Common$ulClass = $author$project$Html$Common$elClass($elm$html$Html$ul);
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Html$Overrides$overrideNameWriteView = F6(
	function (model, inputClass, deleted, overrideName, keys, ix) {
		var suggestions = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeR,
				$author$project$Types$Override$unOverrideName,
				$elm$core$String$startsWith(overrideName)),
			keys);
		var suggestionItem = function (suggestion) {
			return A2(
				$elm$html$Html$li,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('input__suggest'),
						$elm$html$Html$Events$onMouseDown(
						A2(
							$author$project$Html$Overrides$EditOverrideName,
							ix,
							$author$project$Types$Override$unOverrideName(suggestion)))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$b,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(overrideName)
							])),
						$elm$html$Html$text(
						A2(
							$elm$core$String$dropLeft,
							$elm$core$String$length(overrideName),
							$author$project$Types$Override$unOverrideName(suggestion)))
					]));
		};
		var suggestionsView = function () {
			var _v0 = model.autocomplete;
			if (_v0.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var ax = _v0.a;
				return (_Utils_eq(ax, ix) && ($elm$core$List$length(suggestions) > 0)) ? _List_fromArray(
					[
						A2(
						$author$project$Html$Common$ulClass,
						'input__dropdown',
						A2($elm$core$List$map, suggestionItem, suggestions))
					]) : _List_Nil;
			}
		}();
		var nameClass = 'editable-row__value input' + (deleted ? ' input--deleted' : '');
		var inputName = A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(inputClass),
					$elm$html$Html$Attributes$placeholder('value'),
					$elm$html$Html$Attributes$spellcheck(false),
					$elm$html$Html$Attributes$type_('text'),
					$elm$html$Html$Attributes$value(overrideName),
					$elm$html$Html$Attributes$disabled(deleted),
					$elm$html$Html$Events$onInput(
					$author$project$Html$Overrides$EditOverrideName(ix)),
					$elm$html$Html$Events$onFocus(
					$author$project$Html$Overrides$ShowAutocomplete(ix)),
					$elm$html$Html$Events$onBlur($author$project$Html$Overrides$HideAutocomplete)
				]),
			_List_Nil);
		return A2(
			$author$project$Html$Common$divClass,
			nameClass,
			A2($elm$core$List$cons, inputName, suggestionsView));
	});
var $author$project$Html$Overrides$EditOverrideValue = F2(
	function (a, b) {
		return {$: 'EditOverrideValue', a: a, b: b};
	});
var $author$project$Html$Overrides$overrideValueWriteView = F5(
	function (_v0, inputClass, deleted, overrideValue, ix) {
		var valueClass = 'editable-row__value input' + (deleted ? ' input--deleted' : '');
		var valueClass_ = ((!deleted) && (overrideValue === '')) ? (valueClass + ' input--error') : valueClass;
		return A2(
			$author$project$Html$Common$divClass,
			valueClass_,
			A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(inputClass),
							$elm$html$Html$Attributes$placeholder('value'),
							$elm$html$Html$Attributes$spellcheck(false),
							$elm$html$Html$Attributes$type_('text'),
							$elm$html$Html$Attributes$disabled(deleted),
							$elm$html$Html$Attributes$value(overrideValue),
							$elm$html$Html$Events$onInput(
							$author$project$Html$Overrides$EditOverrideValue(ix))
						]),
					_List_Nil),
				((!deleted) && (overrideValue === '')) ? _List_fromArray(
					[
						A2(
						$author$project$Html$Common$divClass,
						'input__output',
						_List_fromArray(
							[
								$elm$html$Html$text('Value can not be empty')
							]))
					]) : _List_Nil));
	});
var $author$project$Html$Overrides$overrideWriteView = F7(
	function (nameInputClass, valueInputClass, deleted, model, nameInput, valueInput, ix) {
		var _v0 = deleted ? _Utils_Tuple2(
			'editable-row__delete spot spot--undo',
			$author$project$Html$Overrides$RestoreOverride(ix)) : _Utils_Tuple2(
			'editable-row__delete spot spot--cancel',
			$author$project$Html$Overrides$DeleteOverride(ix));
		var btnClass = _v0.a;
		var btnMsg = _v0.b;
		return A2(
			$author$project$Html$Common$divClass,
			'row',
			_List_fromArray(
				[
					A2(
					$author$project$Html$Common$divClass,
					'editable-row',
					_List_fromArray(
						[
							A6($author$project$Html$Overrides$overrideNameWriteView, model, nameInputClass, deleted, nameInput, model.keys, ix),
							A5($author$project$Html$Overrides$overrideValueWriteView, model, valueInputClass, deleted, valueInput, ix),
							A3($author$project$Html$Common$buttonClass, btnClass, btnMsg, _List_Nil)
						]))
				]));
	});
var $author$project$Html$Overrides$defaultOverrideWriteView = A3($author$project$Html$Overrides$overrideWriteView, 'input__widget key-default-pristine', 'input__widget value-pristine', false);
var $author$project$Html$Overrides$deletedOverrideWriteView = A3($author$project$Html$Overrides$overrideWriteView, 'input__widget key-deleted', 'input__widget value-deleted', true);
var $author$project$Html$Overrides$editedOverrideWriteView = A3($author$project$Html$Overrides$overrideWriteView, 'input__widget key-default-edited', 'input__widget value-edited', false);
var $author$project$Html$Overrides$newOverrideWriteView = A3($author$project$Html$Overrides$overrideWriteView, 'input__widget key-custom-pristine', 'input__widget value-edited', false);
var $author$project$Html$Overrides$overrideWriteWrapper = F3(
	function (model, ix, wrapped) {
		var _v0 = function () {
			if (wrapped.$ === 'DefaultOverride') {
				var override = wrapped.a;
				return _Utils_Tuple3($author$project$Html$Overrides$defaultOverrideWriteView, override.name, override.value);
			} else {
				var override = wrapped.a;
				var _v2 = override.status;
				switch (_v2.$) {
					case 'Edited':
						return _Utils_Tuple3($author$project$Html$Overrides$editedOverrideWriteView, override.name, override.value);
					case 'Deleted':
						return _Utils_Tuple3($author$project$Html$Overrides$deletedOverrideWriteView, override.name, override.value);
					default:
						return _Utils_Tuple3($author$project$Html$Overrides$newOverrideWriteView, override.name, override.value);
				}
			}
		}();
		var viewer = _v0.a;
		var overrideName = _v0.b;
		var overrideValue = _v0.c;
		return A4(
			viewer,
			model,
			$author$project$Types$Override$unOverrideName(overrideName),
			overrideValue,
			ix);
	});
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Html$Overrides$newOverridesView = F2(
	function (model, overrides) {
		var overridesSorted = $elm$core$List$reverse(
			A2(
				$elm$core$List$sortBy,
				function (_v2) {
					var ix = _v2.a;
					return ix;
				},
				$elm$core$Dict$toList(overrides)));
		var newOverrideWriteView_ = function (_v1) {
			var ix = _v1.a;
			var override = _v1.b;
			return A3(
				$author$project$Html$Overrides$overrideWriteWrapper,
				model,
				ix,
				$author$project$Html$Overrides$NonOverrideWithDefault(override));
		};
		var _v0 = model.mode;
		if (_v0.$ === 'Write') {
			return A2($elm$core$List$map, newOverrideWriteView_, overridesSorted);
		} else {
			return _List_Nil;
		}
	});
var $elm_community$list_extra$List$Extra$inits = A2(
	$elm$core$List$foldr,
	F2(
		function (e, acc) {
			return A2(
				$elm$core$List$cons,
				_List_Nil,
				A2(
					$elm$core$List$map,
					$elm$core$List$cons(e),
					acc));
		}),
	_List_fromArray(
		[_List_Nil]));
var $author$project$Html$Overrides$isDefautOverride = function (w) {
	if (w.$ === 'DefaultOverride') {
		return true;
	} else {
		return false;
	}
};
var $author$project$Html$Overrides$CloseName = function (a) {
	return {$: 'CloseName', a: a};
};
var $author$project$Html$Overrides$OpenName = function (a) {
	return {$: 'OpenName', a: a};
};
var $author$project$Html$Overrides$overrideReadView = F4(
	function (nameClass, valueClass, nameInput, valueInput) {
		return A2(
			$author$project$Html$Common$divClass,
			'row',
			_List_fromArray(
				[
					A2(
					$author$project$Html$Common$spanClass,
					nameClass,
					_List_fromArray(
						[
							$elm$html$Html$text(nameInput + ': ')
						])),
					A2(
					$author$project$Html$Common$spanClass,
					valueClass,
					_List_fromArray(
						[
							$elm$html$Html$text(valueInput)
						]))
				]));
	});
var $author$project$Html$Overrides$defaultOverrideReadView = A2($author$project$Html$Overrides$overrideReadView, 'key-default-pristine', 'value-pristine');
var $author$project$Html$Overrides$deletedOverrideReadView = A2($author$project$Html$Overrides$overrideReadView, 'key-deleted', 'value-deleted');
var $author$project$Html$Overrides$editedOverrideReadView = A2($author$project$Html$Overrides$overrideReadView, 'key-default-edited', 'value-edited');
var $author$project$Html$Overrides$newOverrideReadView = A2($author$project$Html$Overrides$overrideReadView, 'key-custom-edited', 'value-edited');
var $author$project$Html$Overrides$overrideReadWrapper = function (wrapped) {
	if (wrapped.$ === 'DefaultOverride') {
		var override = wrapped.a;
		return A2(
			$author$project$Html$Overrides$defaultOverrideReadView,
			$author$project$Types$Override$unOverrideName(override.name),
			override.value);
	} else {
		var override = wrapped.a;
		var _v1 = override.status;
		switch (_v1.$) {
			case 'Edited':
				return A2(
					$author$project$Html$Overrides$editedOverrideReadView,
					$author$project$Types$Override$unOverrideName(override.name),
					override.value);
			case 'Deleted':
				return A2(
					$author$project$Html$Overrides$deletedOverrideReadView,
					$author$project$Types$Override$unOverrideName(override.name),
					override.value);
			default:
				return A2(
					$author$project$Html$Overrides$newOverrideReadView,
					$author$project$Types$Override$unOverrideName(override.name),
					override.value);
		}
	}
};
var $author$project$Html$Overrides$treeOverrideView = F5(
	function (model, treeData, editedNames, piecies, treeSchema) {
		if (treeSchema.$ === 'Node') {
			if ((treeSchema.b.b && (treeSchema.b.a.$ === 'Leaf')) && (!treeSchema.b.b.b)) {
				var _v1 = treeSchema.b;
				var ix = _v1.a.a;
				var _v2 = _Utils_Tuple2(
					model.mode,
					A2($elm$core$Dict$get, ix, treeData));
				_v2$2:
				while (true) {
					if (_v2.a.$ === 'Write') {
						if (_v2.b.$ === 'Just') {
							var _v3 = _v2.a;
							var override = _v2.b.a;
							return A3($author$project$Html$Overrides$overrideWriteWrapper, model, ix, override);
						} else {
							break _v2$2;
						}
					} else {
						if (_v2.b.$ === 'Just') {
							var _v4 = _v2.a;
							var override = _v2.b.a;
							return $author$project$Html$Overrides$overrideReadWrapper(override);
						} else {
							break _v2$2;
						}
					}
				}
				return A2($elm$html$Html$div, _List_Nil, _List_Nil);
			} else {
				var piece = treeSchema.a;
				var cs = treeSchema.b;
				var btnClass = A2(
					$elm$core$Set$member,
					A2($elm$core$List$cons, piece, piecies),
					editedNames) ? 'collapse__head collapse__head--has-changes' : 'collapse__head';
				return A2(
					$elm$core$Set$member,
					A2($elm$core$List$cons, piece, piecies),
					model.openedNames) ? A2(
					$author$project$Html$Common$divClass,
					'collapse--project collapse collapse--expanded',
					_List_fromArray(
						[
							A3(
							$author$project$Html$Common$buttonClass,
							btnClass,
							$author$project$Html$Overrides$CloseName(
								A2($elm$core$List$cons, piece, piecies)),
							_List_fromArray(
								[
									$elm$html$Html$text(piece)
								])),
							A2(
							$author$project$Html$Common$divClass,
							'collapse__inner',
							A2(
								$elm$core$List$map,
								A4(
									$author$project$Html$Overrides$treeOverrideView,
									model,
									treeData,
									editedNames,
									A2($elm$core$List$cons, piece, piecies)),
								cs))
						])) : A2(
					$author$project$Html$Common$divClass,
					'collapse--project collapse',
					_List_fromArray(
						[
							A3(
							$author$project$Html$Common$buttonClass,
							btnClass,
							$author$project$Html$Overrides$OpenName(
								A2($elm$core$List$cons, piece, piecies)),
							_List_fromArray(
								[
									$elm$html$Html$text(piece)
								])),
							A2($author$project$Html$Common$divClass, 'collapse__inner', _List_Nil)
						]));
			}
		} else {
			return A2($elm$html$Html$div, _List_Nil, _List_Nil);
		}
	});
var $author$project$Html$Overrides$overridesLevelView = function (model) {
	var treeData = A2($author$project$Html$Overrides$mkWrappedOverride, model.defaultOverrides, model.editedOverrides);
	var editedNames = $elm$core$Set$fromList(
		$elm$core$List$concat(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Tuple$second,
					A2(
						$elm$core$Basics$composeR,
						$author$project$Html$Overrides$getWrappedName,
						A2(
							$elm$core$Basics$composeR,
							$author$project$Types$Override$unOverrideName,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$String$split('.'),
								A2(
									$elm$core$Basics$composeR,
									$elm_community$list_extra$List$Extra$inits,
									$elm$core$List$map($elm$core$List$reverse)))))),
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$second,
						A2($elm$core$Basics$composeR, $author$project$Html$Overrides$isDefautOverride, $elm$core$Basics$not)),
					$elm$core$Dict$toList(treeData)))));
	return A2(
		$elm$core$List$map,
		A4($author$project$Html$Overrides$treeOverrideView, model, treeData, editedNames, _List_Nil),
		model.treeSchema);
};
var $author$project$Tree$values = function (t) {
	if (t.$ === 'Node') {
		var cs = t.b;
		return $elm$core$List$concat(
			A2($elm$core$List$map, $author$project$Tree$values, cs));
	} else {
		var x = t.a;
		return _List_fromArray(
			[x]);
	}
};
var $author$project$Html$Overrides$overridesSectionData = function (model) {
	var treeIds = $elm$core$Set$fromList(
		A2($elm$core$List$concatMap, $author$project$Tree$values, model.treeSchema));
	var newOverrides = A2(
		$elm$core$Dict$filter,
		F2(
			function (id, _v2) {
				return !A2($elm$core$Set$member, id, treeIds);
			}),
		model.editedOverrides);
	var isEmptyOverride = F2(
		function (_v1, v) {
			return v.value === '';
		});
	var hasEmptyOverrides = function (overrides) {
		return A3(
			$elm$core$Basics$composeR,
			$elm$core$Dict$isEmpty,
			$elm$core$Basics$not,
			A2($elm$core$Dict$filter, isEmptyOverride, overrides));
	};
	var addButton = function () {
		var _v0 = model.mode;
		if (_v0.$ === 'Write') {
			return _List_fromArray(
				[
					hasEmptyOverrides(newOverrides) ? A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('dash--disabled dash dash--add overrides__add'),
							$elm$html$Html$Attributes$disabled(true)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Add an override')
						])) : A3(
					$author$project$Html$Common$buttonClass,
					'dash dash--add overrides__add',
					$author$project$Html$Overrides$AddOverride,
					_List_fromArray(
						[
							$elm$html$Html$text('Add an override')
						]))
				]);
		} else {
			return _List_Nil;
		}
	}();
	return A2(
		$author$project$Html$Common$divClass,
		'deployment__widget',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'padded',
				_Utils_ap(
					addButton,
					_Utils_ap(
						A2($author$project$Html$Overrides$newOverridesView, model, newOverrides),
						$author$project$Html$Overrides$overridesLevelView(model))))
			]));
};
var $author$project$Html$Overrides$view = function (model) {
	return A2(
		$author$project$Html$Common$divClass,
		'deployment__section',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$h3Class,
				'deployment__sub-heading',
				_List_fromArray(
					[
						$elm$html$Html$text(model.name)
					])),
				$author$project$Html$Overrides$overridesSectionData(model)
			]));
};
var $author$project$Page$Deployment$overridesView = F2(
	function (overrides, mapper) {
		return A2(
			$elm$html$Html$map,
			mapper,
			function () {
				if (overrides.$ === 'Success') {
					var x = overrides.a;
					return $author$project$Html$Overrides$view(x);
				} else {
					return $author$project$Html$Overrides$overridesSectionLoading($author$project$Html$Overrides$Read);
				}
			}());
	});
var $author$project$Page$Deployment$deploymentView = F2(
	function (model, deployment) {
		return A2(
			$author$project$Html$Common$divClass,
			'deployment',
			_List_fromArray(
				[
					A2($author$project$Page$Deployment$deploymentSummaryView, model, deployment),
					A2($author$project$Page$Deployment$deploymentLinksView, model, deployment),
					A2($author$project$Page$Deployment$overridesView, model.deploymentOverrides, $author$project$Page$Deployment$DeploymentOverridesMsg),
					A2($author$project$Page$Deployment$overridesView, model.appOverrides, $author$project$Page$Deployment$AppOverridesMsg),
					$author$project$Page$Deployment$actionsView(model)
				]));
	});
var $author$project$Page$Deployment$failureDeploymentView = function (model) {
	return A2(
		$author$project$Html$Common$divClass,
		'no-deployment',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'null null--data',
				_List_fromArray(
					[
						A2(
						$author$project$Html$Common$divClass,
						'null__content',
						_List_fromArray(
							[
								A2(
								$author$project$Html$Common$bClass,
								'null__heading',
								_List_fromArray(
									[
										$elm$html$Html$text('Cannot retrieve the data'),
										A2(
										$author$project$Html$Common$divClass,
										'null__message',
										_List_fromArray(
											[
												$elm$html$Html$text('Try to reload page')
											]))
									]))
							]))
					]))
			]));
};
var $author$project$Page$Deployment$loadingDeploymentView = function (model) {
	return A2(
		$author$project$Html$Common$divClass,
		'no-deployment',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'loading loading--enlarged loading--alternate',
				_List_fromArray(
					[
						$elm$html$Html$text('Loading ...')
					]))
			]));
};
var $author$project$Page$Deployment$pageBodyView = function (model) {
	return A2(
		$author$project$Html$Common$divClass,
		'page__body',
		_List_fromArray(
			[
				function () {
				var _v0 = model.deployment;
				switch (_v0.$) {
					case 'Success':
						var deployment = _v0.a;
						return A2($author$project$Page$Deployment$deploymentView, model, deployment);
					case 'Failure':
						return $author$project$Page$Deployment$failureDeploymentView(model);
					default:
						return $author$project$Page$Deployment$loadingDeploymentView(model);
				}
			}()
			]));
};
var $author$project$Page$Deployment$pageWrapper = function (body) {
	return A2(
		$author$project$Html$Common$divClass,
		'page',
		_List_fromArray(
			[
				A2($author$project$Html$Common$divClass, 'page__wrap container', body)
			]));
};
var $author$project$Page$Deployment$pageView = function (model) {
	return $author$project$Page$Deployment$pageWrapper(
		_List_fromArray(
			[
				$author$project$Page$Deployment$backButton,
				$author$project$Page$Deployment$deploymentHeaderView(model),
				$author$project$Page$Deployment$pageBodyView(model)
			]));
};
var $author$project$Page$Sidebar$CreateUpdate$NameInput = function (a) {
	return {$: 'NameInput', a: a};
};
var $author$project$Page$Sidebar$CreateUpdate$hasEmptyName = function (model) {
	var nameLength = $elm$core$String$length(
		$author$project$Types$Deployment$unDeploymentName(model.name));
	return (nameLength < 2) || (nameLength > 17);
};
var $author$project$Page$Sidebar$CreateUpdate$nameSection = function (model) {
	var nameError = model.nameEdited && $author$project$Page$Sidebar$CreateUpdate$hasEmptyName(model);
	var inputClass = nameError ? 'input input--error' : 'input';
	var errorMessage = nameError ? _List_fromArray(
		[
			A2(
			$author$project$Html$Common$divClass,
			'input__output',
			_List_fromArray(
				[
					$elm$html$Html$text('Deployment name length should be longer than 2 characters and under 17 characters and begin with a letter.')
				]))
		]) : _List_Nil;
	return A2(
		$author$project$Html$Common$divClass,
		'deployment__section',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$h3Class,
				'deployment__sub-heading',
				_List_fromArray(
					[
						$elm$html$Html$text('Name')
					])),
				A2(
				$author$project$Html$Common$divClass,
				'deployment__widget',
				A2(
					$elm$core$List$cons,
					A2(
						$author$project$Html$Common$divClass,
						inputClass,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('input__widget tag'),
										$elm$html$Html$Attributes$type_('text'),
										$elm$html$Html$Attributes$placeholder('Name'),
										$elm$html$Html$Attributes$value(
										$author$project$Types$Deployment$unDeploymentName(model.name)),
										$elm$html$Html$Events$onInput($author$project$Page$Sidebar$CreateUpdate$NameInput)
									]),
								_List_Nil)
							])),
					errorMessage))
			]));
};
var $author$project$Page$Sidebar$CreateUpdate$overridesView = F2(
	function (overrides, mapper) {
		return A2(
			$elm$html$Html$map,
			mapper,
			function () {
				if (overrides.$ === 'Success') {
					var x = overrides.a;
					return $author$project$Html$Overrides$view(x);
				} else {
					return $author$project$Html$Overrides$overridesSectionLoading($author$project$Html$Overrides$Write);
				}
			}());
	});
var $author$project$Page$Sidebar$CreateUpdate$sidebarContent = function (model) {
	var errorViewer = function (msg) {
		return _List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'deployment__output notification notification--danger',
				_List_fromArray(
					[
						$elm$html$Html$text(msg)
					]))
			]);
	};
	var errorText = function () {
		var _v0 = model.saveResp;
		if (_v0.$ === 'Failure') {
			if (_v0.a.$ === 'BadStatus') {
				var _v1 = _v0.a;
				var msg = _v1.b;
				return $elm$core$Maybe$Just(msg);
			} else {
				return $elm$core$Maybe$Just('Something went wrong');
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}();
	var errorView = A2(
		$elm$core$Maybe$withDefault,
		_List_Nil,
		A2($elm$core$Maybe$map, errorViewer, errorText));
	return A2(
		$author$project$Html$Common$divClass,
		'popup__content',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'deployment',
				_Utils_ap(
					errorView,
					_List_fromArray(
						[
							$author$project$Page$Sidebar$CreateUpdate$nameSection(model),
							A2($author$project$Page$Sidebar$CreateUpdate$overridesView, model.deploymentOverrides, $author$project$Page$Sidebar$CreateUpdate$DeploymentOverridesMsg),
							A2($author$project$Page$Sidebar$CreateUpdate$overridesView, model.appOverrides, $author$project$Page$Sidebar$CreateUpdate$AppOverridesMsg)
						])))
			]));
};
var $author$project$Page$Sidebar$CreateUpdate$Close = {$: 'Close'};
var $author$project$Page$Sidebar$CreateUpdate$Save = {$: 'Save'};
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Html$Common$h2Class = $author$project$Html$Common$elClass($elm$html$Html$h2);
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Html$Overrides$hasEmptyValues = function (model) {
	return A3(
		$elm$core$Basics$composeR,
		$elm$core$List$isEmpty,
		$elm$core$Basics$not,
		A2(
			$elm$core$List$filter,
			function (v) {
				return v.value === '';
			},
			$author$project$Html$Overrides$getFullOverrides(model)));
};
var $author$project$Page$Sidebar$CreateUpdate$hasEmptyValues = function (model) {
	var _v0 = _Utils_Tuple2(model.appOverrides, model.deploymentOverrides);
	if ((_v0.a.$ === 'Success') && (_v0.b.$ === 'Success')) {
		var appOverrides = _v0.a.a;
		var deploymentOverrides = _v0.b.a;
		return $author$project$Html$Overrides$hasEmptyValues(appOverrides) || ($author$project$Html$Overrides$hasEmptyValues(deploymentOverrides) || $author$project$Page$Sidebar$CreateUpdate$hasEmptyName(model));
	} else {
		return true;
	}
};
var $author$project$Page$Sidebar$CreateUpdate$sidebarHeader = function (model) {
	var button = function () {
		var _v1 = _Utils_Tuple2(
			model.saveResp,
			$author$project$Page$Sidebar$CreateUpdate$hasEmptyValues(model));
		_v1$0:
		while (true) {
			_v1$1:
			while (true) {
				if (_v1.b) {
					switch (_v1.a.$) {
						case 'Loading':
							break _v1$0;
						case 'Success':
							break _v1$1;
						default:
							return A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('button--disabled button button--save popup__action'),
										$elm$html$Html$Attributes$disabled(true)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Save')
									]));
					}
				} else {
					switch (_v1.a.$) {
						case 'Loading':
							break _v1$0;
						case 'Success':
							break _v1$1;
						default:
							return A3(
								$author$project$Html$Common$buttonClass,
								'button button--save popup__action',
								$author$project$Page$Sidebar$CreateUpdate$Save,
								_List_fromArray(
									[
										$elm$html$Html$text('Save')
									]));
					}
				}
			}
			return A2($elm$html$Html$div, _List_Nil, _List_Nil);
		}
		var _v2 = _v1.a;
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('popup__action button button--save-loading'),
					$elm$html$Html$Attributes$disabled(true)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Save')
				]));
	}();
	return A2(
		$author$project$Html$Common$divClass,
		'popup__head',
		_List_fromArray(
			[
				A3($author$project$Html$Common$buttonClass, 'popup__close', $author$project$Page$Sidebar$CreateUpdate$Close, _List_Nil),
				A2(
				$author$project$Html$Common$h2Class,
				'popup__project',
				_List_fromArray(
					[
						$elm$html$Html$text(
						function () {
							var _v0 = model.mode;
							if (_v0.$ === 'Create') {
								return 'Create new deployment';
							} else {
								return 'Update  deployment';
							}
						}())
					])),
				A2(
				$author$project$Html$Common$divClass,
				'popup__operations',
				_List_fromArray(
					[button])),
				A2($author$project$Html$Common$divClass, 'popup__menu drop drop--actions', _List_Nil)
			]));
};
var $author$project$Page$Sidebar$CreateUpdate$view = function (model) {
	return model.visibility ? _List_fromArray(
		[
			A2(
			$author$project$Html$Common$divClass,
			'popup popup--visible',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'aria-hidden', 'true'),
							$elm$html$Html$Attributes$class('popup__overlay')
						]),
					_List_Nil),
					A2(
					$author$project$Html$Common$divClass,
					'popup__body',
					_List_fromArray(
						[
							$author$project$Page$Sidebar$CreateUpdate$sidebarHeader(model),
							$author$project$Page$Sidebar$CreateUpdate$sidebarContent(model)
						]))
				]))
		]) : _List_Nil;
};
var $author$project$Page$Deployment$view = function (model) {
	return {
		content: _Utils_ap(
			A2(
				$elm$core$List$cons,
				$author$project$Page$Deployment$pageView(model),
				A2(
					$elm$core$List$map,
					$elm$html$Html$map($author$project$Page$Deployment$CreateSidebarMsg),
					$author$project$Page$Sidebar$CreateUpdate$view(model.sidebar))),
			function () {
				var _v0 = model.archivePopup;
				if (_v0.$ === 'Nothing') {
					return _List_Nil;
				} else {
					var deploymentName = _v0.a;
					return _List_fromArray(
						[
							$author$project$Page$Deployment$archivePopupView(deploymentName)
						]);
				}
			}()),
		title: 'Deployments'
	};
};
var $author$project$Page$Deployments$Table$CloseArchivePopup = {$: 'CloseArchivePopup'};
var $author$project$Page$Deployments$Table$DeleteDeploymentReq = function (a) {
	return {$: 'DeleteDeploymentReq', a: a};
};
var $author$project$Page$Deployments$Table$archivePopupView = function (deploymentName) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('classic-popup'),
				A2($elm$html$Html$Attributes$style, 'display', 'block')
			]),
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'classic-popup__container',
				_List_fromArray(
					[
						A2(
						$author$project$Html$Common$divClass,
						'classic-popup__viewport',
						_List_fromArray(
							[
								A2(
								$author$project$Html$Common$divClass,
								'classic-popup__slot',
								_List_fromArray(
									[
										A2(
										$author$project$Html$Common$divClass,
										'dialog dialog--archive',
										_List_fromArray(
											[
												A2(
												$author$project$Html$Common$divClass,
												'dialog__content',
												_List_fromArray(
													[
														$elm$html$Html$text('Are you sure you want to archive the'),
														A2($elm$html$Html$br, _List_Nil, _List_Nil),
														$elm$html$Html$text(
														$author$project$Types$Deployment$unDeploymentName(deploymentName)),
														$elm$html$Html$text(' deployment?')
													])),
												A2(
												$author$project$Html$Common$divClass,
												'dialog__footer',
												_List_fromArray(
													[
														A3(
														$author$project$Html$Common$buttonClass,
														'button dialog__action',
														$author$project$Page$Deployments$Table$DeleteDeploymentReq(deploymentName),
														_List_fromArray(
															[
																$elm$html$Html$text('Archive')
															])),
														A3(
														$author$project$Html$Common$buttonClass,
														'button dialog__action button--secondary',
														$author$project$Page$Deployments$Table$CloseArchivePopup,
														_List_fromArray(
															[
																$elm$html$Html$text('Cancel')
															]))
													]))
											])),
										A3($author$project$Html$Common$buttonClass, 'classic-popup__close', $author$project$Page$Deployments$Table$CloseArchivePopup, _List_Nil)
									]))
							]))
					]))
			]));
};
var $author$project$Page$Deployments$Table$CloseAppOverrides = function (a) {
	return {$: 'CloseAppOverrides', a: a};
};
var $author$project$Page$Deployments$Table$CloseDeploymentOverrides = function (a) {
	return {$: 'CloseDeploymentOverrides', a: a};
};
var $author$project$Page$Deployments$Table$GoToDeployment = function (a) {
	return {$: 'GoToDeployment', a: a};
};
var $author$project$Page$Deployments$Table$OpenAppOverrides = function (a) {
	return {$: 'OpenAppOverrides', a: a};
};
var $author$project$Page$Deployments$Table$OpenArchivePopup = function (a) {
	return {$: 'OpenArchivePopup', a: a};
};
var $author$project$Page$Deployments$Table$OpenDeploymentOverrides = function (a) {
	return {$: 'OpenDeploymentOverrides', a: a};
};
var $author$project$Page$Deployments$Table$OpenMenu = function (a) {
	return {$: 'OpenMenu', a: a};
};
var $author$project$Page$Deployments$Table$RestoreDeploymentReq = function (a) {
	return {$: 'RestoreDeploymentReq', a: a};
};
var $author$project$Page$Deployments$Table$ShowEditSidebar = function (a) {
	return {$: 'ShowEditSidebar', a: a};
};
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Page$Deployments$Table$overridesView = F5(
	function (openCmd, closeCmd, dName, opened, overrides) {
		var visibleOverrides = A2($elm$core$List$take, 3, overrides);
		var overrideView = function (override) {
			return A2(
				$author$project$Html$Common$divClass,
				'row',
				function () {
					var _v0 = override.value;
					if (_v0.$ === 'ValueAdded') {
						var v = _v0.a;
						return _List_fromArray(
							[
								A2(
								$author$project$Html$Common$spanClass,
								'key-default-pristine',
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Types$Override$unOverrideName(override.name) + ': ')
									])),
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('value-edited'),
										A2($elm$html$Html$Attributes$attribute, 'style', 'width:400px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:block;')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(v)
									]))
							]);
					} else {
						return _List_fromArray(
							[
								A2(
								$author$project$Html$Common$spanClass,
								'key-deleted',
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Types$Override$unOverrideName(override.name) + ': ')
									])),
								A2($author$project$Html$Common$divClass, 'listing__placeholder listing__placeholder__value', _List_Nil)
							]);
					}
				}());
		};
		var buttonClass_ = F3(
			function (cls, msg, body) {
				return A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(cls),
							$elm_community$html_extra$Html$Events$Extra$onClickStopPropagation(msg)
						]),
					body);
			});
		return _Utils_eq(
			$elm$core$List$length(overrides),
			$elm$core$List$length(visibleOverrides)) ? A2($elm$core$List$map, overrideView, overrides) : (A2($turboMaCk$any_set$Set$Any$member, dName, opened) ? A2(
			$elm$core$List$append,
			A2($elm$core$List$map, overrideView, overrides),
			_List_fromArray(
				[
					A3(
					buttonClass_,
					'expander listing__more expander--open',
					closeCmd(dName),
					_List_fromArray(
						[
							$elm$html$Html$text('Hide default configuration')
						]))
				])) : A2(
			$elm$core$List$append,
			A2($elm$core$List$map, overrideView, visibleOverrides),
			_List_fromArray(
				[
					A3(
					buttonClass_,
					'expander listing__more',
					openCmd(dName),
					_List_fromArray(
						[
							$elm$html$Html$text('Show full configuration')
						]))
				])));
	});
var $author$project$Page$Deployments$Table$deploymentView = F2(
	function (model, deployment) {
		var statusView = function (status) {
			if (status.$ === 'DeploymentPending') {
				return A2(
					$author$project$Html$Common$divClass,
					'status status--pending',
					_List_fromArray(
						[
							$elm$html$Html$text('Pending...')
						]));
			} else {
				switch (status.a.$) {
					case 'Running':
						var _v3 = status.a;
						return A2(
							$author$project$Html$Common$divClass,
							'status status--success',
							_List_fromArray(
								[
									$elm$html$Html$text('Running')
								]));
					case 'Failure':
						return A2(
							$author$project$Html$Common$divClass,
							'status status--failure',
							_List_fromArray(
								[
									$elm$html$Html$text('Failure')
								]));
					case 'CreatePending':
						var _v4 = status.a;
						return A2(
							$author$project$Html$Common$divClass,
							'loading loading--status-alike',
							_List_fromArray(
								[
									$elm$html$Html$text('Creating...')
								]));
					case 'UpdatePending':
						var _v5 = status.a;
						return A2(
							$author$project$Html$Common$divClass,
							'loading loading--status-alike',
							_List_fromArray(
								[
									$elm$html$Html$text('Updating...')
								]));
					case 'ArchivePending':
						var _v6 = status.a;
						return A2(
							$author$project$Html$Common$divClass,
							'loading loading--status-alike',
							_List_fromArray(
								[
									$elm$html$Html$text('Archiving...')
								]));
					case 'Archived':
						var _v7 = status.a;
						return A2(
							$author$project$Html$Common$divClass,
							'status status--archived',
							_List_fromArray(
								[
									$elm$html$Html$text('Archived')
								]));
					default:
						var _v8 = status.a;
						return A2(
							$author$project$Html$Common$divClass,
							'status status--failure',
							_List_fromArray(
								[
									$elm$html$Html$text('Cleanup failed (contact admin)')
								]));
				}
			}
		};
		var name = _List_fromArray(
			[
				$elm$html$Html$text(
				$author$project$Types$Deployment$unDeploymentName(deployment.deployment.name)),
				statusView(deployment.status)
			]);
		var menuButtonClass = F2(
			function (status, _class) {
				return $author$project$Types$Deployment$isPending(status) ? (_class + ' action--disabled ') : _class;
			});
		var link = function (m) {
			return A3(
				$author$project$Html$Common$aClassHrefExternal,
				'listing__item external bar',
				m.link,
				_List_fromArray(
					[
						$elm$html$Html$text(m.name)
					]));
		};
		var links = function () {
			var _v1 = model.tableType;
			if (_v1.$ === 'ActiveTable') {
				return _List_fromArray(
					[
						A2(
						$author$project$Html$Common$divClass,
						'listing',
						A2($elm$core$List$map, link, deployment.metadata))
					]);
			} else {
				return _List_fromArray(
					[
						$elm$html$Html$text('...')
					]);
			}
		}();
		var deploymentOverrides = _List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'row',
				A5($author$project$Page$Deployments$Table$overridesView, $author$project$Page$Deployments$Table$OpenDeploymentOverrides, $author$project$Page$Deployments$Table$CloseDeploymentOverrides, deployment.deployment.name, model.openedDeployemntOverrides, deployment.deployment.deploymentOverrides))
			]);
		var buttonClass_ = F3(
			function (cls, msg, body) {
				return A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(cls),
							$elm_community$html_extra$Html$Events$Extra$onClickStopPropagation(msg)
						]),
					body);
			});
		var dropDown = A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					'drop drop--actions ' + (_Utils_eq(
						model.menuButton,
						$elm$core$Maybe$Just(deployment.deployment.name)) ? 'drop--expanded' : '')),
					$elm$html$Html$Attributes$id(
					$author$project$Page$Deployments$Table$getDropdownId(model))
				]),
			_List_fromArray(
				[
					A3(
					buttonClass_,
					'drop__handler',
					$author$project$Page$Deployments$Table$OpenMenu(deployment.deployment.name),
					_List_fromArray(
						[
							$elm$html$Html$text('Actions')
						])),
					A2(
					$author$project$Html$Common$divClass,
					'drop__dropdown',
					function () {
						var _v0 = model.tableType;
						if (_v0.$ === 'ActiveTable') {
							return _List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class(
											A2(menuButtonClass, deployment.status, 'action action--edit')),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Attributes$disabled(
											$author$project$Types$Deployment$isPending(deployment.status)),
											$elm_community$html_extra$Html$Events$Extra$onClickStopPropagation(
											$author$project$Page$Deployments$Table$ShowEditSidebar(deployment.deployment.name))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Edit')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class(
											A2(menuButtonClass, deployment.status, 'action action--archive classic-popup-handler')),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Attributes$disabled(
											$author$project$Types$Deployment$isPending(deployment.status)),
											$elm_community$html_extra$Html$Events$Extra$onClickStopPropagation(
											$author$project$Page$Deployments$Table$OpenArchivePopup(deployment.deployment.name))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Move to archive')
										])),
									A3(
									$author$project$Html$Common$aClassHrefExternal,
									'action action--logs',
									_Utils_ap(
										model.config.k8sDashboardUrlTemplate,
										$author$project$Types$Deployment$unDeploymentName(deployment.deployment.name)),
									_List_fromArray(
										[
											$elm$html$Html$text('Details')
										]))
								]);
						} else {
							return _List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('action action--restore'),
											$elm_community$html_extra$Html$Events$Extra$onClickStopPropagation(
											$author$project$Page$Deployments$Table$RestoreDeploymentReq(deployment.deployment.name))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Restore from archive')
										]))
								]);
						}
					}())
				]));
		var appOverrides = _List_fromArray(
			[
				A2(
				$author$project$Html$Common$divClass,
				'row',
				A5($author$project$Page$Deployments$Table$overridesView, $author$project$Page$Deployments$Table$OpenAppOverrides, $author$project$Page$Deployments$Table$CloseAppOverrides, deployment.deployment.name, model.openedAppOverrides, deployment.deployment.appOverrides))
			]);
		return A2(
			$elm$html$Html$tr,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Page$Deployments$Table$GoToDeployment(deployment.deployment.name))
				]),
			_List_fromArray(
				[
					A2($elm$html$Html$td, _List_Nil, name),
					A2($elm$html$Html$td, _List_Nil, links),
					A2($elm$html$Html$td, _List_Nil, deploymentOverrides),
					A2($elm$html$Html$td, _List_Nil, appOverrides),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Html$Common$dateView, model.settings.zone, deployment.createdAt)
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							A2($author$project$Html$Common$dateView, model.settings.zone, deployment.updatedAt)
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[dropDown]))
				]));
	});
var $author$project$Page$Deployments$Table$tableFailureView = A2(
	$elm$html$Html$tbody,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$author$project$Html$Common$trClass,
			'no-table',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$colspan(8)
						]),
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$divClass,
							'null null--data',
							_List_fromArray(
								[
									A2(
									$author$project$Html$Common$bClass,
									'null__heading',
									_List_fromArray(
										[
											$elm$html$Html$text('Cannot retrieve the data')
										])),
									A2(
									$author$project$Html$Common$divClass,
									'null__message',
									_List_fromArray(
										[
											$elm$html$Html$text('Try to reload page')
										]))
								]))
						]))
				]))
		]));
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Page$Deployments$Table$tableDeploymentsView = F3(
	function (model, deployments, search) {
		var compareDeployments = F2(
			function (a, b) {
				var _v0 = model.sort;
				if (_v0.$ === 'Asc') {
					switch (_v0.a.$) {
						case 'Name':
							var _v1 = _v0.a;
							return A2(
								$elm$core$Basics$compare,
								$author$project$Types$Deployment$unDeploymentName(a.deployment.name),
								$author$project$Types$Deployment$unDeploymentName(b.deployment.name));
						case 'Created':
							var _v3 = _v0.a;
							return A2(
								$elm$core$Basics$compare,
								$elm$time$Time$posixToMillis(a.createdAt),
								$elm$time$Time$posixToMillis(b.createdAt));
						default:
							var _v5 = _v0.a;
							return A2(
								$elm$core$Basics$compare,
								$elm$time$Time$posixToMillis(a.updatedAt),
								$elm$time$Time$posixToMillis(b.updatedAt));
					}
				} else {
					switch (_v0.a.$) {
						case 'Name':
							var _v2 = _v0.a;
							return A2(
								$elm$core$Basics$compare,
								$author$project$Types$Deployment$unDeploymentName(b.deployment.name),
								$author$project$Types$Deployment$unDeploymentName(a.deployment.name));
						case 'Created':
							var _v4 = _v0.a;
							return A2(
								$elm$core$Basics$compare,
								$elm$time$Time$posixToMillis(b.createdAt),
								$elm$time$Time$posixToMillis(a.createdAt));
						default:
							var _v6 = _v0.a;
							return A2(
								$elm$core$Basics$compare,
								$elm$time$Time$posixToMillis(b.updatedAt),
								$elm$time$Time$posixToMillis(a.updatedAt));
					}
				}
			});
		var check = function (deployment) {
			return (search === '') ? true : A2(
				$elm$core$String$contains,
				$elm$core$String$toUpper(search),
				$elm$core$String$toUpper(
					$author$project$Types$Deployment$unDeploymentName(deployment.deployment.name)));
		};
		var sorted = A2(
			$elm$core$List$filter,
			check,
			A2($elm$core$List$sortWith, compareDeployments, deployments));
		return ($elm$core$List$length(sorted) > 0) ? A2(
			$elm$html$Html$tbody,
			_List_Nil,
			A2(
				$elm$core$List$map,
				$author$project$Page$Deployments$Table$deploymentView(model),
				sorted)) : $author$project$Page$Deployments$Table$tableFailureView;
	});
var $author$project$Page$Deployments$Table$tableLoadingView = A2(
	$elm$html$Html$tbody,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$author$project$Html$Common$trClass,
			'no-table',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$colspan(8)
						]),
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$divClass,
							'loading loading--enlarged loading--alternate',
							_List_fromArray(
								[
									$elm$html$Html$text('Loading...')
								]))
						]))
				]))
		]));
var $author$project$Page$Deployments$Table$tablePrimaryBodyView = F3(
	function (model, deployments, search) {
		switch (deployments.$) {
			case 'Success':
				var d = deployments.a;
				return A3($author$project$Page$Deployments$Table$tableDeploymentsView, model, d, search);
			case 'Failure':
				return $author$project$Page$Deployments$Table$tableFailureView;
			default:
				return $author$project$Page$Deployments$Table$tableLoadingView;
		}
	});
var $author$project$Page$Deployments$Table$Asc = function (a) {
	return {$: 'Asc', a: a};
};
var $author$project$Page$Deployments$Table$Created = {$: 'Created'};
var $author$project$Page$Deployments$Table$Name = {$: 'Name'};
var $author$project$Page$Deployments$Table$SortChanged = function (a) {
	return {$: 'SortChanged', a: a};
};
var $author$project$Page$Deployments$Table$tablePrimaryHeaderView = function (model) {
	var sortTh = F3(
		function (clm, sort, txt) {
			var _v0 = function () {
				if (sort.$ === 'Asc') {
					var sClm = sort.a;
					return _Utils_eq(sClm, clm) ? _Utils_Tuple2(
						'sort sort--active sort--asc',
						$author$project$Page$Deployments$Table$SortChanged(
							$author$project$Page$Deployments$Table$Desc(clm))) : _Utils_Tuple2(
						'sort',
						$author$project$Page$Deployments$Table$SortChanged(
							$author$project$Page$Deployments$Table$Asc(clm)));
				} else {
					var sClm = sort.a;
					return _Utils_eq(sClm, clm) ? _Utils_Tuple2(
						'sort sort--active sort--desc',
						$author$project$Page$Deployments$Table$SortChanged(
							$author$project$Page$Deployments$Table$Asc(clm))) : _Utils_Tuple2(
						'sort',
						$author$project$Page$Deployments$Table$SortChanged(
							$author$project$Page$Deployments$Table$Asc(clm)));
				}
			}();
			var cls = _v0.a;
			var event = _v0.b;
			return A2(
				$elm$html$Html$th,
				_List_Nil,
				_List_fromArray(
					[
						A3(
						$author$project$Html$Common$buttonClass,
						cls,
						event,
						_List_fromArray(
							[
								$elm$html$Html$text(txt)
							]))
					]));
		});
	var simpleTh = function (txt) {
		return A2(
			$elm$html$Html$th,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(txt)
				]));
	};
	return A2(
		$elm$html$Html$thead,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A3(sortTh, $author$project$Page$Deployments$Table$Name, model.sort, 'Name'),
						simpleTh('Links'),
						simpleTh('Deployment configuration'),
						simpleTh('App configuration'),
						A3(sortTh, $author$project$Page$Deployments$Table$Created, model.sort, 'Created'),
						A3(sortTh, $author$project$Page$Deployments$Table$Updated, model.sort, 'Updated'),
						A2(
						$elm$html$Html$th,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$author$project$Html$Common$spanClass,
								'visuallyhidden',
								_List_fromArray(
									[
										$elm$html$Html$text('Menu')
									]))
							]))
					]))
			]));
};
var $author$project$Page$Deployments$Table$view = F3(
	function (model, deployments, search) {
		return A2(
			$author$project$Html$Common$divClass,
			'table table--deployments table--clickable table--double-click',
			A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$table,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Page$Deployments$Table$tablePrimaryHeaderView(model),
							A3($author$project$Page$Deployments$Table$tablePrimaryBodyView, model, deployments, search)
						])),
				function () {
					var _v0 = model.archivePopup;
					if (_v0.$ === 'Nothing') {
						return _List_Nil;
					} else {
						var deploymentName = _v0.a;
						return _List_fromArray(
							[
								$author$project$Page$Deployments$Table$archivePopupView(deploymentName)
							]);
					}
				}()));
	});
var $author$project$Page$Deployments$dataArchivedView = F2(
	function (model, archivedDeploymets) {
		var dataClass = model.showArchived ? 'data__archive data__archive--open' : 'data__archive';
		return A2(
			$author$project$Html$Common$divClass,
			dataClass,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$map,
					$author$project$Page$Deployments$ArchivedTableMsg,
					A3($author$project$Page$Deployments$Table$view, model.archivedTable, archivedDeploymets, model.search))
				]));
	});
var $author$project$Page$Deployments$dataPrimaryView = F2(
	function (model, activeDeploymets) {
		return A2(
			$author$project$Html$Common$divClass,
			'data__primary',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$map,
					$author$project$Page$Deployments$ActiveTableMsg,
					A3($author$project$Page$Deployments$Table$view, model.activeTable, activeDeploymets, model.search))
				]));
	});
var $author$project$Page$Deployments$pageBodyWrapper = function (body) {
	return A2(
		$author$project$Html$Common$divClass,
		'page__body',
		_List_fromArray(
			[
				A2($author$project$Html$Common$divClass, 'body', body)
			]));
};
var $author$project$Page$Deployments$ShowCreateSidebar = {$: 'ShowCreateSidebar'};
var $author$project$Page$Deployments$SearchInput = function (a) {
	return {$: 'SearchInput', a: a};
};
var $author$project$Page$Deployments$pageHeaderSearchView = function (_v0) {
	return A2(
		$author$project$Html$Common$divClass,
		'page__action page__action--search input input--search input--has-clear-type',
		_List_fromArray(
			[
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('input__widget'),
						$elm$html$Html$Attributes$type_('text'),
						$elm$html$Html$Attributes$placeholder('Search for deployments'),
						$elm$html$Html$Events$onInput($author$project$Page$Deployments$SearchInput)
					]),
				_List_Nil)
			]));
};
var $author$project$Page$Deployments$pageHeaderTimeUpdateView = function (model) {
	var msg = (model.updated < 30) ? 'Updated just now' : ((model.updated < 90) ? 'Updated 1 minute ago' : ((_Utils_cmp(model.updated, 60 * 60) < 0) ? ('Updated ' + ($elm$core$String$fromInt((model.updated / 60) | 0) + ' minutes ago')) : ((_Utils_cmp(model.updated, (2 * 60) * 60) < 0) ? 'Update 1 hour ago' : ('Updated ' + ($elm$core$String$fromInt((model.updated / (60 * 60)) | 0) + ' ho ago')))));
	return A2(
		$author$project$Html$Common$divClass,
		'page__note',
		_List_fromArray(
			[
				$elm$html$Html$text(msg)
			]));
};
var $author$project$Page$Deployments$pageHeaderView = function (model) {
	return A2(
		$author$project$Html$Common$divClass,
		'page__head',
		_List_fromArray(
			[
				A2(
				$author$project$Html$Common$h1Class,
				'page__heading title',
				_List_fromArray(
					[
						$elm$html$Html$text('All deployments')
					])),
				$author$project$Page$Deployments$pageHeaderTimeUpdateView(model),
				$author$project$Page$Deployments$pageHeaderSearchView(model),
				A3(
				$author$project$Html$Common$buttonClass,
				'page__action button button--add popup-handler',
				$author$project$Page$Deployments$ShowCreateSidebar,
				_List_fromArray(
					[
						$elm$html$Html$text('New deployment')
					]))
			]));
};
var $author$project$Page$Deployments$pageWrapper = function (body) {
	return A2(
		$author$project$Html$Common$divClass,
		'page',
		_List_fromArray(
			[
				A2($author$project$Html$Common$divClass, 'page__wrap container', body)
			]));
};
var $author$project$Page$Deployments$ToggleArchived = {$: 'ToggleArchived'};
var $author$project$Page$Deployments$toggleButtonView = function (model) {
	return model.showArchived ? A3(
		$author$project$Html$Common$buttonClass,
		'data__show-archive expander expander--stand-alone expander--open',
		$author$project$Page$Deployments$ToggleArchived,
		_List_fromArray(
			[
				$elm$html$Html$text('Hide Archived deployments')
			])) : A3(
		$author$project$Html$Common$buttonClass,
		'data__show-archive expander expander--stand-alone',
		$author$project$Page$Deployments$ToggleArchived,
		_List_fromArray(
			[
				$elm$html$Html$text('Show Archived deployments')
			]));
};
var $author$project$Page$Deployments$pageView = function (model) {
	var archivedDeploymets = A2(
		$krisajenkins$remotedata$RemoteData$map,
		$elm$core$List$filter($author$project$Types$Deployment$isDeploymentArchived),
		model.deployments);
	var archivedCount = A3($krisajenkins$remotedata$RemoteData$unwrap, 0, $elm$core$List$length, archivedDeploymets);
	var activeDeploymets = A2(
		$krisajenkins$remotedata$RemoteData$map,
		$elm$core$List$filter(
			A2($elm$core$Basics$composeR, $author$project$Types$Deployment$isDeploymentArchived, $elm$core$Basics$not)),
		model.deployments);
	return $author$project$Page$Deployments$pageWrapper(
		_List_fromArray(
			[
				$author$project$Page$Deployments$pageHeaderView(model),
				$author$project$Page$Deployments$pageBodyWrapper(
				A2(
					$elm$core$List$cons,
					A2($author$project$Page$Deployments$dataPrimaryView, model, activeDeploymets),
					(!archivedCount) ? _List_Nil : _List_fromArray(
						[
							$author$project$Page$Deployments$toggleButtonView(model),
							A2($author$project$Page$Deployments$dataArchivedView, model, archivedDeploymets)
						])))
			]));
};
var $author$project$Page$Deployments$view = function (model) {
	return {
		content: A2(
			$elm$core$List$cons,
			$author$project$Page$Deployments$pageView(model),
			A2(
				$elm$core$List$map,
				$elm$html$Html$map($author$project$Page$Deployments$CreateSidebarMsg),
				$author$project$Page$Sidebar$CreateUpdate$view(model.sidebar))),
		title: 'Deployments'
	};
};
var $author$project$Page$Initialization$failureView = A2(
	$author$project$Html$Common$divClass,
	'no-page',
	_List_fromArray(
		[
			A2(
			$author$project$Html$Common$divClass,
			'no-page__inner',
			_List_fromArray(
				[
					A2(
					$author$project$Html$Common$divClass,
					'null null--data',
					_List_fromArray(
						[
							A2(
							$author$project$Html$Common$bClass,
							'null__heading',
							_List_fromArray(
								[
									$elm$html$Html$text('Cannot retrieve the data')
								])),
							A2(
							$author$project$Html$Common$divClass,
							'null__message',
							_List_fromArray(
								[
									$elm$html$Html$text('Try to reload page')
								]))
						]))
				]))
		]));
var $author$project$Page$Initialization$loadingView = A2(
	$author$project$Html$Common$divClass,
	'no-page',
	_List_fromArray(
		[
			A2(
			$author$project$Html$Common$divClass,
			'no-page__inner',
			_List_fromArray(
				[
					A2(
					$author$project$Html$Common$divClass,
					'loading loading--enlarged loading--alternate',
					_List_fromArray(
						[
							$elm$html$Html$text('Loading...')
						]))
				]))
		]));
var $author$project$Page$Initialization$view = function (model) {
	var _v0 = _Utils_Tuple2(model.config, model.projectName);
	_v0$1:
	while (true) {
		_v0$2:
		while (true) {
			switch (_v0.a.$) {
				case 'Failure':
					return {
						content: _List_fromArray(
							[$author$project$Page$Initialization$failureView]),
						title: 'Octopod'
					};
				case 'Loading':
					switch (_v0.b.$) {
						case 'Failure':
							break _v0$1;
						case 'Loading':
							break _v0$2;
						default:
							break _v0$2;
					}
				default:
					switch (_v0.b.$) {
						case 'Failure':
							break _v0$1;
						case 'Loading':
							var _v2 = _v0.b;
							return {
								content: _List_fromArray(
									[$author$project$Page$Initialization$loadingView]),
								title: 'Octopod'
							};
						default:
							return {content: _List_Nil, title: 'Octopod'};
					}
			}
		}
		var _v1 = _v0.a;
		return {
			content: _List_fromArray(
				[$author$project$Page$Initialization$loadingView]),
			title: 'Octopod'
		};
	}
	return {
		content: _List_fromArray(
			[$author$project$Page$Initialization$failureView]),
		title: 'Octopod'
	};
};
var $author$project$Main$view = function (model) {
	var viewPage = F3(
		function (page, toMsg, config) {
			var _v1 = A3(
				$author$project$Page$view,
				$author$project$Main$getSettings(model).projectName,
				page,
				config);
			var title = _v1.title;
			var body = _v1.body;
			return {
				body: A2(
					$elm$core$List$map,
					$elm$html$Html$map(toMsg),
					body),
				title: title
			};
		});
	switch (model.$) {
		case 'Initialization':
			var subModel = model.a;
			return A3(
				viewPage,
				$author$project$Page$Initialization,
				$author$project$Main$InitializationMsg,
				$author$project$Page$Initialization$view(subModel));
		case 'Deployments':
			var subModel = model.a;
			return A3(
				viewPage,
				$author$project$Page$Deployments,
				$author$project$Main$DeploymentsMsg,
				$author$project$Page$Deployments$view(subModel));
		default:
			var subModel = model.a;
			return A3(
				viewPage,
				$author$project$Page$Deployment,
				$author$project$Main$DeploymentMsg,
				$author$project$Page$Deployment$view(subModel));
	}
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Main$UrlChanged, onUrlRequest: $author$project$Main$LinkClicked, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));