let readFile = path => Node.Fs.readFileAsUtf8Sync(path);

let inputFileToList = path => {
  let input = readFile(path);
  List.filter(i => i != "", Array.to_list(Js.String.split("\n", input)));
};

let rec scanLeft = (fn, initial, list) => {
  switch (list) {
  | [] => []
  | [head, ...tail] => [
      fn(initial, head),
      ...scanLeft(fn, fn(initial, head), tail),
    ]
  };
};

module IntSet =
  Set.Make({
    type t = int;
    let compare = compare;
  });