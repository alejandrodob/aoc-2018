// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var $$Set = require("bs-platform/lib/js/set.js");
var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");

function readFile(path) {
  return Fs.readFileSync(path, "utf8");
}

function inputFileToList(path) {
  var input = Fs.readFileSync(path, "utf8");
  return List.filter((function (i) {
                  return i !== "";
                }))($$Array.to_list(input.split("\n")));
}

function scanLeft(fn, initial, list) {
  if (list) {
    var head = list[0];
    return /* :: */[
            Curry._2(fn, initial, head),
            scanLeft(fn, Curry._2(fn, initial, head), list[1])
          ];
  } else {
    return /* [] */0;
  }
}

var compare = Caml_obj.caml_compare;

var IntSet = $$Set.Make(/* module */[/* compare */compare]);

exports.readFile = readFile;
exports.inputFileToList = inputFileToList;
exports.scanLeft = scanLeft;
exports.IntSet = IntSet;
/* IntSet Not a pure module */
