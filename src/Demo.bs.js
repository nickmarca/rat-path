// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");

function goBottom(i, param) {
  var y = param[1];
  var x = param[0];
  Caml_array.caml_array_set(Caml_array.caml_array_get(i, x + 1 | 0), y, Caml_array.caml_array_get(Caml_array.caml_array_get(i, x + 1 | 0), y) + 1 | 0);
  return /* tuple */[
          x + 1 | 0,
          y
        ];
}

function goUp(i, param) {
  var y = param[1];
  var x = param[0];
  Caml_array.caml_array_set(Caml_array.caml_array_get(i, x - 1 | 0), y, Caml_array.caml_array_get(Caml_array.caml_array_get(i, x - 1 | 0), y) + 1 | 0);
  return /* tuple */[
          x - 1 | 0,
          y
        ];
}

function goRight(i, param) {
  var y = param[1];
  var x = param[0];
  Caml_array.caml_array_set(Caml_array.caml_array_get(i, x), y + 1 | 0, Caml_array.caml_array_get(Caml_array.caml_array_get(i, x), y + 1 | 0) + 1 | 0);
  return /* tuple */[
          x,
          y + 1 | 0
        ];
}

function goLeft(i, param) {
  var y = param[1];
  var x = param[0];
  Caml_array.caml_array_set(Caml_array.caml_array_get(i, x), y - 1 | 0, Caml_array.caml_array_get(Caml_array.caml_array_get(i, x), y - 1 | 0) + 1 | 0);
  return /* tuple */[
          x,
          y - 1 | 0
        ];
}

function caseBottom(i, param) {
  var y = param[1];
  var x = param[0];
  var match = x < 3 && Caml_array.caml_array_get(Caml_array.caml_array_get(i, x + 1 | 0), y) !== -1;
  if (match) {
    return Caml_array.caml_array_get(Caml_array.caml_array_get(i, x + 1 | 0), y);
  } else {
    return -1;
  }
}

function caseRight(i, param) {
  var y = param[1];
  var x = param[0];
  var match = y < 3 && Caml_array.caml_array_get(Caml_array.caml_array_get(i, x), y + 1 | 0) !== -1;
  if (match) {
    return Caml_array.caml_array_get(Caml_array.caml_array_get(i, x), y + 1 | 0);
  } else {
    return -1;
  }
}

function goBack(i, param, lastMove) {
  var y = param[1];
  var x = param[0];
  if (lastMove) {
    return goUp(i, /* tuple */[
                x,
                y
              ]);
  } else {
    return goLeft(i, /* tuple */[
                x,
                y
              ]);
  }
}

function go(i, _param, _mov) {
  while(true) {
    var param = _param;
    var mov = _mov;
    var y = param[1];
    var x = param[0];
    if (x === 3 && y === 3) {
      return mov;
    } else {
      var d1 = caseRight(i, /* tuple */[
            x,
            y
          ]);
      var d2 = caseBottom(i, /* tuple */[
            x,
            y
          ]);
      if (d1 === 0) {
        var newCoords = goRight(i, /* tuple */[
              x,
              y
            ]);
        _mov = /* :: */[
          /* Right */0,
          mov
        ];
        _param = newCoords;
        continue ;
      } else if (d2 === 0) {
        var newCoords$1 = goBottom(i, /* tuple */[
              x,
              y
            ]);
        _mov = /* :: */[
          /* Down */1,
          mov
        ];
        _param = newCoords$1;
        continue ;
      } else if (mov) {
        var newCoords$2 = goBack(i, /* tuple */[
              x,
              y
            ], mov[0]);
        _mov = mov[1];
        _param = newCoords$2;
        continue ;
      } else {
        return /* [] */0;
      }
    }
  };
}

var input = $$Array.make_matrix(4, 4, 0);

Caml_array.caml_array_set(Caml_array.caml_array_get(input, 0), 0, 1);

Caml_array.caml_array_set(Caml_array.caml_array_get(input, 1), 1, -1);

Caml_array.caml_array_set(Caml_array.caml_array_get(input, 2), 2, -1);

Caml_array.caml_array_set(Caml_array.caml_array_get(input, 2), 3, -1);

var res = go(input, /* tuple */[
      0,
      0
    ], /* [] */0);

function printMov(l) {
  if (l) {
    if (l[0]) {
      console.log("Down");
      printMov(l[1]);
      return /* () */0;
    } else {
      console.log("Right");
      printMov(l[1]);
      return /* () */0;
    }
  } else {
    return /* () */0;
  }
}

printMov(List.rev(res));

exports.goBottom = goBottom;
exports.goUp = goUp;
exports.goRight = goRight;
exports.goLeft = goLeft;
exports.caseBottom = caseBottom;
exports.caseRight = caseRight;
exports.goBack = goBack;
exports.go = go;
exports.input = input;
exports.res = res;
exports.printMov = printMov;
/* input Not a pure module */
