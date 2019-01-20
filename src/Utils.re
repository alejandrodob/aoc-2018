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

module Stream = {
  type t('a) =
    | Stream('a, unit => t('a))
    | Done;

  let hd =
    fun
    | Stream(a, _) => Some(a)
    | Done => None;

  let tl =
    fun
    | Stream(_, s) => s()
    | Done => Done;

  let rec ofList = list => {
    switch (list) {
    | [] => Done
    | [head, ...tail] => Stream(head, () => ofList(tail))
    };
  };

  let cycle = stream => {
    let rec c = (complete, remaining) =>
      switch (hd(remaining)) {
      | None => c(complete, complete)
      | Some(head) => Stream(head, () => c(complete, tl(remaining)))
      };

    c(stream, stream);
  };

  let rec scanLeft = (fn, initial, stream) => {
    switch (hd(stream)) {
    | None => Done
    | Some(head) =>
      Stream(
        fn(initial, head),
        () => scanLeft(fn, fn(initial, head), tl(stream)),
      )
    };
  };

  let rec take = (n, stream) =>
    switch (n) {
    | 0 => []
    | n => [hd(stream), ...take(n - 1, tl(stream))]
    };
};